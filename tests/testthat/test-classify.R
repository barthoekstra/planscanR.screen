# Tests for the zero-shot classification framework. A deterministic mock
# classifier stands in for the reticulate/transformers backend, so the suite
# stays offline and fast (the same strategy as make_fake_model() for embeddings).

# Mock: keyword-rules over the text, returns a softmax-ish score matrix over
# whatever labels it's handed. Deterministic and language-agnostic enough for
# the German/Dutch test titles.
make_fake_classifier <- function() {
  classifier(
    name = "fake-zeroshot",
    classify_fn = function(x, labels, multi_label) {
      slugs <- names(labels)
      kw <- list(
        wind = c("wind", "windpark", "windkraft", "wea"),
        solar = c("solar", "photovolta", "pv "),
        power_grid = c("leitung", "grid", "umspann", "kv"),
        energy_strategy = c("strategie", "energiestrategie", "res "),
        water = c("wasser", "grundwasser", "hochwasser"),
        land_use = c("flurneuordnung", "bedrijventerrein", "bebauungsplan")
      )
      m <- matrix(0, nrow = length(x), ncol = length(slugs), dimnames = list(NULL, slugs))
      for (i in seq_along(x)) {
        t <- tolower(x[i])
        raw <- vapply(
          slugs,
          function(s) {
            hits <- kw[[s]]
            if (is.null(hits)) {
              return(0.1)
            }
            0.1 + sum(vapply(hits, function(h) grepl(h, t, fixed = TRUE), logical(1)))
          },
          numeric(1)
        )
        m[i, ] <- raw / sum(raw) # normalise to a probability-ish row
      }
      m
    }
  )
}

# Local candidate-label fixture so the framework tests carry their own labels
# (classify_assessments() now requires them; the BIOGAIN set lives in
# planscanR.biogain). Mirrors the BIOGAIN cardinality and the slugs the mock
# classifier keys on; `relevant` marks the positive (energy) classes.
fake_labels <- function() {
  labs <- c(
    wind = "wind", solar = "solar", power_grid = "grid",
    other_renewable = "other renewable", energy_strategy = "strategy",
    renewable_zoning = "zoning", fossil_power = "fossil",
    oil_gas_extraction = "oil gas", nuclear = "nuclear", water = "water",
    land_use = "land use", transport = "transport", other = "other"
  )
  attr(labs, "relevant") <- c(
    "wind", "solar", "power_grid",
    "other_renewable", "energy_strategy", "renewable_zoning"
  )
  labs
}

test_that("classifier() validates inputs", {
  expect_error(classifier("", function(x, l, m) NULL), "non-empty string")
  expect_error(classifier("x", "not-a-fn"), "must be a function")
})

test_that("classify_text returns an [n x labels] matrix with slug column names", {
  clf <- make_fake_classifier()
  labs <- fake_labels()
  out <- classify_text(clf, c("Windpark Test", "Flurneuordnung X"), labs)
  expect_true(is.matrix(out))
  expect_identical(dim(out), c(2L, length(labs)))
  expect_identical(colnames(out), names(labs))
})

test_that("classify_text errors on malformed backend output", {
  bad <- classifier("bad", function(x, l, m) matrix(0, nrow = 1, ncol = 1))
  expect_error(classify_text(bad, c("a", "b"), c(w = "wind", o = "other")), "invalid shape")
})

test_that("classify_assessments adds class_* columns and flags relevance", {
  clf <- make_fake_classifier()
  recs <- tibble::tibble(
    country = c("de", "de", "de"),
    title = c("Windpark Nord", "Flurneuordnung Sued", "Grundwasserentnahme"),
    summary = c("Errichtung von WEA", "Plan ueber Anlagen", "Wasserrechtliche Bewilligung"),
    native_type = c(
      "Waermeerzeugung, Bergbau und Energie",
      "Flurbereinigung",
      "Wasserwirtschaftliche Vorhaben"
    )
  )
  out <- classify_assessments(recs, classifier = clf, labels = fake_labels())
  expect_true(all(c("class_label", "class_score", "class_relevant", "class_model") %in% names(out)))
  expect_true(all(paste0("class_score_", names(fake_labels())) %in% names(out)))
  # Windpark -> wind (relevant); Flurneuordnung -> land_use (not relevant);
  # Grundwasser -> water (not relevant).
  expect_identical(out$class_label[1], "wind")
  expect_true(out$class_relevant[1])
  expect_false(out$class_relevant[2])
  expect_false(out$class_relevant[3])
  expect_identical(out$class_model, rep("fake-zeroshot", 3))
})

test_that("classify_assessments uses title + summary + category as input", {
  # The category alone should be enough for the mock to pick 'wind' even when
  # the title/summary are uninformative.
  seen <- NULL
  clf <- classifier("probe", function(x, labels, multi_label) {
    seen <<- x
    m <- matrix(1 / length(labels), nrow = length(x), ncol = length(labels), dimnames = list(NULL, names(labels)))
    m
  })
  recs <- tibble::tibble(
    country = "de",
    title = "Vorhaben 123",
    summary = "Antrag",
    native_type = "Windkraftanlagen"
  )
  classify_assessments(recs, classifier = clf, labels = fake_labels())
  expect_match(seen, "Vorhaben 123")
  expect_match(seen, "Antrag")
  expect_match(seen, "Windkraftanlagen") # category folded into the text
})

test_that("classify_assessments preserves a zero-row tibble", {
  clf <- make_fake_classifier()
  recs <- tibble::tibble(country = character(0), title = character(0), summary = character(0))
  out <- classify_assessments(recs, classifier = clf, labels = fake_labels())
  expect_identical(nrow(out), 0L)
  expect_true("class_label" %in% names(out))
})

test_that("classification round-trips through the sidecar", {
  withr::with_tempdir({
    options(planscanR.cache_dir = file.path(getwd(), "cache"))
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    rec <- tibble::tibble(
      country = "de",
      source_portal = "uvp-verbund.de",
      document_id = "classtest-1",
      url = "https://www.uvp-verbund.de/trefferanzeige?docuuid=classtest-1",
      retrieved_at = as.POSIXct("2026-05-28 12:00:00", tz = "UTC"),
      attachment_urls = list(character(0)),
      local_path = list(character(0)),
      title = "Windpark X",
      summary = "WEA",
      native_type = "Windkraftanlagen",
      download_status = list(planscanR:::empty_download_status())
    )
    out <- classify_assessments(rec, classifier = make_fake_classifier(), labels = fake_labels(), write_sidecar = TRUE)
    back <- planscanR::index_cache(country = "de")
    expect_identical(back$class_label, out$class_label)
    # Sidecar JSON rounds numerics (jsonlite default ~4 dp), so compare with
    # tolerance rather than identity.
    expect_equal(back$class_score, out$class_score, tolerance = 1e-3)
    expect_identical(back$class_relevant, out$class_relevant)
    expect_true("class_score_wind" %in% names(back))
  })
})

test_that("classify_assessments writes sidecars incrementally (crash-safe)", {
  withr::with_tempdir({
    options(planscanR.cache_dir = file.path(getwd(), "cache"))
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    recs <- tibble::tibble(
      country = rep("de", 6),
      source_portal = "uvp-verbund.de",
      document_id = paste0("inc-", 1:6),
      url = paste0("https://www.uvp-verbund.de/trefferanzeige?docuuid=inc-", 1:6),
      retrieved_at = as.POSIXct("2026-05-28 12:00:00", tz = "UTC"),
      attachment_urls = replicate(6, character(0), simplify = FALSE),
      local_path = replicate(6, character(0), simplify = FALSE),
      title = paste("rec", 1:6),
      summary = NA_character_,
      download_status = replicate(6, planscanR:::empty_download_status(), simplify = FALSE)
    )
    # Mock that aborts on the 3rd batch; batches 1-2 (records 1-4) are written
    # before it blows up.
    calls <- 0L
    boom <- classifier("boom", function(x, labels, multi_label) {
      calls <<- calls + 1L
      if (calls == 3L) {
        stop("boom on batch 3")
      }
      matrix(1 / length(labels), nrow = length(x), ncol = length(labels), dimnames = list(NULL, names(labels)))
    })
    expect_error(
      classify_assessments(recs, classifier = boom, labels = fake_labels(), batch_size = 2L, write_sidecar = TRUE),
      "boom"
    )
    written <- list.files(
      file.path(getwd(), "cache", "files", "de"),
      pattern = "\\.meta\\.json$",
      recursive = TRUE
    )
    # Records 1-4 persisted before the crash; 5-6 did not.
    expect_length(written, 4L)
  })
})

test_that("classify_assessments preserves existing attachment URLs on the sidecar", {
  withr::with_tempdir({
    options(planscanR.cache_dir = file.path(getwd(), "cache"))
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    urls <- c(
      "https://www.uvp-verbund.de/documents-ige-ng/x/doc-att/a.pdf",
      "https://www.uvp-verbund.de/documents-ige-ng/x/doc-att/b.pdf"
    )
    # A scanned (un-downloaded) record: attachment URLs present as pending rows.
    rec <- tibble::tibble(
      country = "de",
      source_portal = "uvp-verbund.de",
      document_id = "doc-att",
      url = "https://www.uvp-verbund.de/trefferanzeige?docuuid=doc-att",
      retrieved_at = as.POSIXct("2026-05-28 12:00:00", tz = "UTC"),
      attachment_urls = list(urls),
      attachment_urls_uvp_bericht = list(urls),
      local_path = list(rep(NA_character_, 2)),
      title = "Windpark X",
      summary = "WEA",
      native_type = "Windkraftanlagen",
      download_status = list(planscanR:::pending_download_status(urls))
    )
    planscanR::write_record_sidecar(rec, downloads = rec$download_status[[1]])

    # Classify it (the operation that used to wipe files[]).
    recs <- planscanR::index_cache(country = "de")
    expect_length(recs$attachment_urls[[1]], 2L) # sanity: present before
    classify_assessments(recs, classifier = make_fake_classifier(), labels = fake_labels(), write_sidecar = TRUE)

    back <- planscanR::index_cache(country = "de")
    # Attachment URLs survive classification, and the verdict is added.
    expect_length(back$attachment_urls[[1]], 2L)
    expect_true("class_label" %in% names(back))
  })
})

test_that("a portal-side sidecar rewrite preserves an existing classification", {
  withr::with_tempdir({
    options(planscanR.cache_dir = file.path(getwd(), "cache"))
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    base <- tibble::tibble(
      country = "de",
      source_portal = "uvp-verbund.de",
      document_id = "preserve-1",
      url = "https://www.uvp-verbund.de/trefferanzeige?docuuid=preserve-1",
      retrieved_at = as.POSIXct("2026-05-28 12:00:00", tz = "UTC"),
      attachment_urls = list(character(0)),
      local_path = list(character(0)),
      title = "Windpark Y",
      summary = "WEA",
      native_type = "Windkraftanlagen",
      download_status = list(planscanR:::empty_download_status())
    )
    # First: classify + persist.
    classify_assessments(base, classifier = make_fake_classifier(), labels = fake_labels(), write_sidecar = TRUE)
    # Then: a portal-side rewrite (no class_* columns on the record).
    planscanR::write_record_sidecar(base)
    back <- planscanR::index_cache(country = "de")
    expect_true("class_label" %in% names(back))
    expect_identical(back$class_label, "wind")
  })
})
