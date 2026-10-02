# ACQ-C4 regression coverage for the legacy version-3 artifact hash bytes.

acq_c4_reference_artifact_hash <- function(value) {
  bytes <- serialize(value, NULL, version = 3)
  path <- tempfile("acq-c4-reference-hash-")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  connection <- file(path, open = "wb")
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  writeBin(bytes, connection)
  close(connection)
  unname(as.character(tools::md5sum(path)))
}

test_that("artifact hash preserves exact version-3 serialization bytes", {
  complex_data <- data.frame(
    country = factor(c("nl", "de", "nl"), levels = c("de", "nl", "fr")),
    document_id = c("doc-1", "doc-2", "doc-3"),
    values = I(list(c(1, 2), list(flag = TRUE, score = 0.5), NULL)),
    occurred_on = as.POSIXct(
      c("2026-01-02 03:04:05", "2026-02-03 04:05:06", "2026-03-04 05:06:07"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  attr(complex_data, "acq_c4_metadata") <- list(
    schema = structure(list(version = 2L), class = "hash-schema"),
    nested = list(raw = as.raw(c(0L, 127L, 255L)), labels = c("a", "b"))
  )

  values <- list(
    nested_list = structure(
      list(
        alpha = c(TRUE, FALSE, NA),
        beta = list(numbers = c(pi, exp(1)), bytes = as.raw(0:5)),
        gamma = matrix(seq_len(6L), nrow = 2L, dimnames = list(NULL, letters[1:3]))
      ),
      audit = list(source = "fixture", version = 1L),
      class = c("acq_c4_hash_fixture", "list")
    ),
    complex_data_frame = complex_data,
    attributes = structure(
      list(
        text = enc2utf8(c("één", "二")),
        missing = NA_real_,
        date = as.Date("2026-10-02"),
        pairlist = pairlist(left = "x", right = 7L)
      ),
      class = "acq_c4_attribute_fixture",
      source = list(kind = "legacy-c3", bytes = as.raw(c(1L, 2L, 3L)))
    )
  )

  for (value in values) {
    expect_identical(
      curation_artifact_hash(value),
      acq_c4_reference_artifact_hash(value)
    )
  }
})

test_that("artifact hash cleanup leaves no open connection after hashing", {
  before <- nrow(showConnections(all = TRUE))
  replicate(12L, curation_artifact_hash(list(ok = TRUE)))
  after <- nrow(showConnections(all = TRUE))
  expect_identical(after, before)
})
