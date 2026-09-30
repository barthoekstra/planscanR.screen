test_that("a NULL translation result fails soft and preserves source text", {
  record <- data.frame(
    country = "nl",
    document_id = "fixture-1",
    title = "Oorspronkelijke titel",
    summary = "Oorspronkelijke samenvatting",
    stringsAsFactors = FALSE
  )
  provider <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) NULL,
    service_id = "fixture-null"
  )

  result <- curation_translate(
    provider,
    record,
    key = list(country = "nl", document_id = "fixture-1"),
    source_language = "nld_Latn"
  )

  expect_identical(result$status, "error")
  expect_identical(result$title, "Oorspronkelijke titel")
  expect_identical(result$summary, "Oorspronkelijke samenvatting")
  expect_match(result$error, "NULL")
})

test_that("classed worker failures fail soft and preserve source text", {
  record <- data.frame(
    country = "de",
    document_id = "fixture-2",
    title = "Original title",
    summary = "Original summary",
    stringsAsFactors = FALSE
  )
  provider <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) {
      structure(
        list(message = "worker returned a classed failure"),
        class = c("fixture_translation_error", "error", "condition")
      )
    },
    service_id = "fixture-condition"
  )

  result <- curation_translate(
    provider,
    record,
    key = list(country = "de", document_id = "fixture-2"),
    source_language = "deu_Latn"
  )

  expect_identical(result$status, "error")
  expect_identical(result$title, "Original title")
  expect_identical(result$summary, "Original summary")
  expect_match(result$error, "classed failure")
})

test_that("translation cache identity includes target language", {
  record <- data.frame(
    country = "nl",
    document_id = "fixture-3",
    title = "Original title",
    summary = "Original summary",
    stringsAsFactors = FALSE
  )
  seen <- list()
  calls <- 0L
  provider <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) {
      calls <<- calls + 1L
      list(title_en = paste0("translated-", target_language))
    },
    service_id = "fixture-cache",
    cache_read_fn = function(key, service_id) {
      seen[[length(seen) + 1L]] <<- key
      if (identical(key$target_language, "eng_Latn")) {
        return(list(title_en = "cached-English"))
      }
      NULL
    }
  )

  english <- curation_translate(
    provider,
    record,
    key = list(country = "nl", document_id = "fixture-3"),
    source_language = "nld_Latn",
    target_language = "eng_Latn"
  )
  german <- curation_translate(
    provider,
    record,
    key = list(country = "nl", document_id = "fixture-3"),
    source_language = "nld_Latn",
    target_language = "deu_Latn"
  )

  expect_identical(english$status, "cached")
  expect_identical(english$title_en, "cached-English")
  expect_identical(german$status, "translated")
  expect_identical(german$title_en, "translated-deu_Latn")
  expect_identical(calls, 1L)
  expect_equal(
    vapply(seen, `[[`, character(1), "target_language"),
    c("eng_Latn", "deu_Latn")
  )
})

test_that("serialized writes lock the record while cache keys retain target", {
  record <- data.frame(
    country = "nl",
    document_id = "fixture-4",
    title = "Original title",
    summary = "Original summary",
    stringsAsFactors = FALSE
  )
  gate_keys <- list()
  cache_keys <- list()
  provider <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) {
      list(title_en = "Translated title")
    },
    service_id = "fixture-write",
    cache_write_fn = function(key, value) {
      cache_keys[[length(cache_keys) + 1L]] <<- key
      invisible(value)
    },
    write_gate_fn = function(key, write_fn) {
      gate_keys[[length(gate_keys) + 1L]] <<- key
      write_fn()
    },
    write_policy = "serialized"
  )

  curation_translate(
    provider,
    record,
    key = list(country = "nl", document_id = "fixture-4"),
    source_language = "nld_Latn",
    target_language = "eng_Latn",
    write = TRUE
  )

  expect_identical(gate_keys[[1L]], list(country = "nl", document_id = "fixture-4"))
  expect_identical(cache_keys[[1L]]$target_language, "eng_Latn")
})
