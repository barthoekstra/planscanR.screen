# Tests for the lexical keyword layer.

# Local lexicon fixture so the framework tests carry their own terms
# (score_keywords() now requires a lexicon; the BIOGAIN lexicon lives in
# planscanR.biogain). Mirrors the BIOGAIN term lists the assertions key on.
kw_lexicon <- function() {
  list(
    wind = c("wind", "repowering"),
    solar = c("solar", "zonne", "fotovolta", "photovolta"),
    power_grid = c(
      "hoogspann", "spannung", "freileitung", "umspann",
      "stromnetz", "netaansluiting", "transformator", "trafostation"
    ),
    other_renewable = c(
      "biogas", "biomass", "geotherm", "aardwarmte",
      "waterkracht", "wasserkraft", "vergisting"
    ),
    energy_strategy = c(
      "energiestrategie", "energietransitie", "energieperspectief",
      "energievisie", "klimaat", "klimaschutz"
    ),
    renewable_zoning = c("zoekgebied", "opwek", "vorranggebiet", "vorrangzone")
  )
}

test_that("score_keywords adds kw_<topic> + kw_total counts", {
  recs <- tibble::tibble(
    title = c(
      "Windturbines Amsterdam-Noord", # wind (NL compound)
      "Woningbouw Westergouwe, Gouda", # housing -> no energy terms
      "Neubau einer 380 kV-Hochspannungsfreileitung", # grid (DE)
      "Zonnepark De Kwekerij" # solar (NL compound)
    ),
    summary = c(
      "plaatsing van windturbines, bestemmingsplan aangepast",
      "ontwikkeling van een woonwijk met woningen",
      "Errichtung einer Hoechstspannungsleitung",
      "aanleg van een zonnepark met panelen"
    )
  )
  out <- score_keywords(recs, lexicon = kw_lexicon())
  expect_true(all(c("kw_wind", "kw_solar", "kw_power_grid", "kw_total") %in% names(out)))
  # Wind compound matches the `wind` stem (multiple occurrences).
  expect_gt(out$kw_wind[1], 0L)
  expect_gt(out$kw_total[1], 0L)
  # Housing has zero energy keywords — the key discriminator for the zoning
  # overlap.
  expect_identical(out$kw_total[2], 0L)
  # German grid + Dutch solar land on the right topic.
  expect_gt(out$kw_power_grid[3], 0L)
  expect_gt(out$kw_solar[4], 0L)
})

test_that("score_keywords folds in the category (native_type) when present", {
  recs <- tibble::tibble(
    title = "Vorhaben 123",
    summary = "Antrag",
    native_type = "Windkraftanlagen" # category carries the only energy term
  )
  out <- score_keywords(recs, lexicon = kw_lexicon())
  expect_gt(out$kw_wind, 0L)
})

test_that("score_keywords matches accented source text via normalisation", {
  # Höchstspannung (umlaut) should match the ASCII 'hoechstspann' term.
  recs <- tibble::tibble(title = "Höchstspannungsfreileitung", summary = NA_character_)
  out <- score_keywords(recs, lexicon = kw_lexicon())
  expect_gt(out$kw_power_grid, 0L)
})

test_that("score_keywords preserves a zero-row tibble", {
  recs <- tibble::tibble(title = character(0), summary = character(0))
  out <- score_keywords(recs, lexicon = kw_lexicon())
  expect_identical(nrow(out), 0L)
  expect_true("kw_total" %in% names(out))
})
