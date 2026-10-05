skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")
skip_if_not_installed("jsonlite")

test_that("project manifests preserve catalog identity and portable file versions", {
  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "archive")
  acquisition_storage_register_root(catalog, "archive", root, admin)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal-a", "native-project-1",
    source_url = "https://portal-a.test/project-1"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "attachment-1",
      attachment_url = "https://portal-a.test/project-1/report.pdf",
      section = "decision",
    legacy_local_path = "files/nl/native-project-1/legacy-report.pdf"
  )
  file <- acquisition_catalog_register_file(
    catalog, document$document_ref_id, "archive",
    "objects/sha256/ab/report.pdf", sha256 = paste(rep("a", 64), collapse = ""),
    size_bytes = 12L, verification_status = "verified", provenance = "portal"
  )

  manifest <- acquisition_catalog_write_project_manifest(catalog, source$source_ref_id)
  path <- attr(manifest, "path")
  decoded <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  version <- decoded$documents[[1L]]$versions[[1L]]

  expect_identical(decoded$project_id, source$source_ref_id)
  expect_identical(decoded$source_references[[1L]]$source_ref_id, source$source_ref_id)
  expect_identical(decoded$documents[[1L]]$document_id, document$document_ref_id)
  expect_identical(version$version_id, file$file_ref_id)
  expect_identical(version$location$storage_root_id, "archive")
  expect_identical(version$location$relative_path, "objects/sha256/ab/report.pdf")
  expect_identical(version$original_filename, "legacy-report.pdf")
  expect_false(any(grepl(root, readLines(path, warn = FALSE), fixed = TRUE)))
  expect_identical(
    acquisition_storage_resolve_location(catalog, version$location),
    file.path(root, "objects", "sha256", "ab", "report.pdf")
  )
})
