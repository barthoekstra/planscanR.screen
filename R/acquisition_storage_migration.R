# Verified content-addressed relocation. Source files and claims are retained;
# successful target claims plus preferred_location in manifests record the
# active copy. A later batch resumes by selecting source claims not yet linked
# to the target object key.

acquisition_storage_object_relative_path <- function(sha256) {
  sha256 <- tolower(acquisition_catalog_scalar_character(sha256, "sha256"))
  if (!grepl("^[[:xdigit:]]{64}$", sha256)) {
    acquisition_storage_abort("A content-addressed object requires a 64-character SHA-256.")
  }
  file.path("objects", "sha256", substr(sha256, 1L, 2L), substr(sha256, 3L, 4L), sha256)
}

acquisition_storage_migration_claims <- function(catalog, source_root_id) {
  DBI::dbGetQuery(
    acquisition_catalog_connection(catalog),
    paste(
      "SELECT f.file_ref_id, f.document_ref_id, f.root_id, f.relative_path, f.sha256,",
      "f.size_bytes, f.verification_status, f.legacy_path, f.provenance,",
      "d.source_ref_id, d.attachment_url, d.legacy_local_path,",
      "s.country, s.portal, s.native_project_id, s.legacy_sidecar_path",
      "FROM acquisition_files f JOIN acquisition_documents d ON d.document_ref_id = f.document_ref_id",
      "JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
      "WHERE f.root_id = ? AND f.verification_status = 'verified'",
      "AND f.sha256 IS NOT NULL AND f.size_bytes IS NOT NULL",
      "ORDER BY f.document_ref_id, f.file_ref_id"
    ),
    list(source_root_id)
  )
}

acquisition_storage_migration_query <- function(catalog, sql, params = list()) {
  DBI::dbGetQuery(acquisition_catalog_connection(catalog), sql, params = params)
}

acquisition_storage_migration_target_claim <- function(catalog, document_ref_id,
                                                        target_root_id, relative_path) {
  acquisition_catalog_one(
    acquisition_catalog_connection(catalog),
    paste(
      "SELECT file_ref_id, sha256, size_bytes, verification_status FROM acquisition_files",
      "WHERE document_ref_id = ? AND root_id = ? AND relative_path = ?"
    ),
    list(document_ref_id, target_root_id, relative_path)
  )
}

acquisition_storage_migration_digest_matches <- function(path, sha256, size_bytes) {
  if (!file.exists(path) || isTRUE(file.info(path)$isdir[[1L]])) return(FALSE)
  actual_size <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
  expected_size <- suppressWarnings(as.numeric(size_bytes))
  identical(actual_size, expected_size) && identical(
    tolower(digest::digest(file = path, algo = "sha256")), tolower(as.character(sha256))
  )
}

acquisition_storage_migration_capacity <- function(catalog, job, target_root_id,
                                                    copy_bytes) {
  reserved <- suppressWarnings(as.numeric(job$storage_reservation_bytes %||% NA_real_))
  if (length(reserved) != 1L || is.na(reserved) || reserved < copy_bytes) {
    return(list(ok = FALSE, reason = "migration-reservation-too-small"))
  }
  conn <- acquisition_catalog_connection(catalog)
  root <- acquisition_catalog_one(conn,
    "SELECT * FROM acquisition_storage_roots WHERE root_id = ? AND active = 1",
    list(target_root_id))
  if (is.null(root)) return(list(ok = FALSE, reason = "inactive-target-root"))
  capacity <- acquisition_storage_capacity(as.character(root$root_path[[1L]]))
  held <- DBI::dbGetQuery(conn,
    "SELECT COALESCE(SUM(bytes), 0) AS bytes FROM acquisition_storage_reservations WHERE state = 'held' AND job_id <> ?",
    params = list(as.character(job$job_id[[1L]])))$bytes[[1L]]
  reserve <- max(as.numeric(root$reserve_bytes[[1L]]),
    capacity$total_bytes * as.numeric(root$reserve_fraction[[1L]]))
  if (capacity$free_bytes - as.numeric(held) - copy_bytes < reserve) {
    return(list(ok = FALSE, reason = "storage-reserve-exhausted"))
  }
  list(ok = TRUE)
}

acquisition_storage_migration_sync_sidecar <- function(catalog, row, target_root_id,
                                                        relative_path, target_path,
                                                        cache_root) {
  result <- list(
    status = "succeeded",
    document_ref_id = as.character(row$document_ref_id[[1L]]),
    root_id = target_root_id,
    relative_path = relative_path,
    local_path = target_path,
    size_bytes = as.numeric(row$size_bytes[[1L]]),
    sha256 = as.character(row$sha256[[1L]]),
    verification_status = "verified",
    provenance = "storage-migration",
    url = as.character(row$attachment_url[[1L]]),
    filename = basename(if (!is.na(row$legacy_path[[1L]])) {
      as.character(row$legacy_path[[1L]])
    } else {
      as.character(row$relative_path[[1L]])
    }),
    download_status = "cached"
  )
  if (!is.na(row$legacy_sidecar_path[[1L]]) && nzchar(as.character(row$legacy_sidecar_path[[1L]]))) {
    result$legacy_sidecar_path <- as.character(row$legacy_sidecar_path[[1L]])
  }
  acquisition_queue_sync_sidecar_row(row, result, cache_root)
}

acquisition_storage_migration_live <- function(conn, relocation_id, job = NULL) {
  relocation <- acquisition_catalog_one(conn,
    "SELECT job_id, state FROM acquisition_storage_relocations WHERE relocation_id = ?",
    list(relocation_id))
  if (is.null(relocation) || !identical(as.character(relocation$state[[1L]]), "running")) return(FALSE)
  if (is.null(job) || is.null(job$lease_token) || is.na(job$lease_token[[1L]])) return(TRUE)
  if (!identical(as.character(relocation$job_id[[1L]]), as.character(job$job_id[[1L]]))) return(FALSE)
  current <- acquisition_catalog_one(conn,
    "SELECT state, lease_token, lease_expires_at FROM acquisition_jobs WHERE job_id = ?",
    list(as.character(job$job_id[[1L]])))
  !is.null(current) && identical(as.character(current$state[[1L]]), "running") &&
    identical(as.character(current$lease_token[[1L]]), as.character(job$lease_token[[1L]])) &&
    as.character(current$lease_expires_at[[1L]]) > acquisition_catalog_now()
}

acquisition_storage_rollback_fenced_restore <- function(catalog, relocation_id,
    row, source_root_id, source_path, cache_root) {
  acquisition_catalog_transaction(catalog, function(conn) {
    relocation <- acquisition_catalog_one(conn,
      "SELECT state FROM acquisition_storage_relocations WHERE relocation_id = ?",
      list(relocation_id))
    if (is.null(relocation) || !identical(as.character(relocation$state[[1L]]), "cancelled")) {
      return(list(status = "blocked", error_category = "rollback-not-fenced"))
    }
    claim <- acquisition_catalog_register_file_connection(conn,
      document_ref_id = as.character(row$document_ref_id[[1L]]),
      root_id = source_root_id, relative_path = as.character(row$relative_path[[1L]]),
      sha256 = as.character(row$sha256[[1L]]), size_bytes = as.numeric(row$size_bytes[[1L]]),
      verification_status = "verified", provenance = as.character(row$provenance[[1L]] %||% "catalog"),
      legacy_path = if (!is.na(row$legacy_path[[1L]])) as.character(row$legacy_path[[1L]]) else NULL)
    result <- list(
      status = "succeeded", document_ref_id = as.character(row$document_ref_id[[1L]]),
      root_id = source_root_id, relative_path = as.character(row$relative_path[[1L]]),
      local_path = source_path, size_bytes = as.numeric(row$size_bytes[[1L]]),
      sha256 = as.character(row$sha256[[1L]]), verification_status = "verified",
      provenance = "storage-migration-rollback", url = as.character(row$attachment_url[[1L]]),
      filename = basename(if (!is.na(row$legacy_path[[1L]])) as.character(row$legacy_path[[1L]]) else as.character(row$relative_path[[1L]])),
      download_status = "cached"
    )
    synced <- acquisition_queue_sync_sidecar_row(row, result, cache_root)
    if (!identical(synced$status, "succeeded")) return(synced)
    tryCatch({
      acquisition_storage_write_migration_manifests(catalog,
        as.character(row$source_ref_id[[1L]]), cache_root)
      list(status = "succeeded", claim = claim, sync = synced)
    }, error = function(error) list(status = "retryable",
      error_category = "manifest-write-failed", error_message = conditionMessage(error)))
  })
}

acquisition_storage_migration_fenced_register <- function(catalog, relocation_id,
    job, row, target_root_id, relative_path) {
  acquisition_catalog_transaction(catalog, function(conn) {
    if (!acquisition_storage_migration_live(conn, relocation_id, job)) {
      return(list(status = "cancelled", error_category = "relocation-cancelled"))
    }
    legacy_path <- if (!is.na(row$legacy_path[[1L]])) as.character(row$legacy_path[[1L]]) else as.character(row$relative_path[[1L]])
    claim <- acquisition_catalog_register_file_connection(conn,
      document_ref_id = as.character(row$document_ref_id[[1L]]),
      root_id = target_root_id, relative_path = relative_path,
      sha256 = tolower(as.character(row$sha256[[1L]])),
      size_bytes = as.numeric(row$size_bytes[[1L]]), verification_status = "verified",
      provenance = "storage-migration", legacy_path = legacy_path)
    list(status = "succeeded", claim = claim)
  })
}

acquisition_storage_migration_fenced_sync_manifest <- function(catalog, relocation_id,
    job, row, target_root_id, relative_path, target_path, cache_root, source_ref_ids) {
  # The target claim has already committed. This second transaction holds the
  # DB lock through the sidecar lock and manifest snapshot/write; rollback uses
  # the same DB -> sidecar lock order and can always discover the claim.
  acquisition_catalog_transaction(catalog, function(conn) {
    if (!acquisition_storage_migration_live(conn, relocation_id, job)) {
      return(list(status = "cancelled", error_category = "relocation-cancelled"))
    }
    switched <- acquisition_storage_migration_sync_sidecar(catalog, row, target_root_id,
      relative_path, target_path, cache_root)
    if (!identical(as.character(switched$status[[1L]]), "succeeded")) return(switched)
    tryCatch({
      acquisition_storage_write_migration_manifests(catalog, source_ref_ids, cache_root)
      switched
    }, error = function(error) list(status = "retryable",
      error_category = "manifest-write-failed", error_message = conditionMessage(error)))
  })
}

acquisition_storage_migration_fenced_manifests <- function(catalog, relocation_id,
    job, source_ref_ids, cache_root) {
  acquisition_catalog_transaction(catalog, function(conn) {
    if (!acquisition_storage_migration_live(conn, relocation_id, job)) {
      return(list(status = "cancelled", error_category = "relocation-cancelled"))
    }
    tryCatch({
      acquisition_storage_write_migration_manifests(catalog, source_ref_ids, cache_root)
      list(status = "succeeded")
    }, error = function(error) list(status = "retryable",
      error_category = "manifest-write-failed", error_message = conditionMessage(error)))
  })
}

acquisition_storage_migrate_batch <- function(job, catalog, batch_size = 25L,
                                              copy_fn = file.copy, cache_root = NULL) {
  batch_size <- acquisition_catalog_scalar_integer(batch_size, "batch_size", lower = 1L, upper = 1000L)
  relocation <- acquisition_catalog_one(acquisition_catalog_connection(catalog),
    "SELECT * FROM acquisition_storage_relocations WHERE job_id = ?",
    list(as.character(job$job_id[[1L]])))
  if (is.null(relocation)) return(list(status = "blocked", error_category = "missing-relocation"))
  if (!identical(as.character(relocation$state[[1L]]), "running")) {
    return(list(status = "cancelled", error_category = "relocation-not-running"))
  }
  source_root_id <- as.character(relocation$source_root_id[[1L]])
  target_root_id <- as.character(relocation$target_root_id[[1L]])
  roots <- acquisition_storage_roots(catalog)
  root_paths <- stats::setNames(as.character(roots$root_path), roots$root_id)
  source_root <- root_paths[[source_root_id]]
  target_root <- root_paths[[target_root_id]]
  if (is.null(source_root) || is.null(target_root)) {
    return(list(status = "blocked", error_category = "unknown-storage-root"))
  }
  cache_root <- cache_root %||% planscanR::cache_dir_default()
  prior_roots <- getOption("planscanR.storage_roots", NULL)
  options(planscanR.storage_roots = root_paths)
  on.exit(options(planscanR.storage_roots = prior_roots), add = TRUE)

  claims <- acquisition_storage_migration_claims(catalog, source_root_id)
  if (nrow(claims) == 0L) return(list(status = "succeeded", migrated = 0L, remaining = 0L))
  rows <- lapply(seq_len(nrow(claims)), function(i) claims[i, , drop = FALSE])
  relative <- lapply(rows, function(row) acquisition_storage_object_relative_path(row$sha256[[1L]]))
  target_claims <- lapply(seq_along(rows), function(i) acquisition_storage_migration_target_claim(
    catalog, rows[[i]]$document_ref_id[[1L]], target_root_id, relative[[i]]
  ))
  for (i in seq_along(target_claims)) {
    claim <- target_claims[[i]]
    if (!is.null(claim) && (!identical(as.character(claim$verification_status[[1L]]), "verified") ||
      !identical(tolower(as.character(claim$sha256[[1L]])), tolower(as.character(rows[[i]]$sha256[[1L]]))))) {
      return(list(status = "blocked", error_category = "target-claim-conflict",
        error_message = "An existing target claim conflicts with the source bytes."))
    }
  }
  # A persisted claim is not proof that its bytes remain intact. Verify every
  # claimed object before treating it as completed or publishing a manifest.
  for (i in which(!vapply(target_claims, is.null, logical(1)))) {
    target_path <- planscanR::storage_path(target_root, relative[[i]])
    claim <- target_claims[[i]]
    if (!acquisition_storage_migration_digest_matches(target_path,
      rows[[i]]$sha256[[1L]], rows[[i]]$size_bytes[[1L]])) {
      return(list(status = "blocked", error_category = "target-verification-failed",
        error_message = "A previously registered target object is missing or does not match its verified claim."))
    }
    receipt <- planscanR::reconcile_cached_download(target_path,
      expected_size_bytes = rows[[i]]$size_bytes[[1L]],
      expected_sha256 = rows[[i]]$sha256[[1L]], repair = TRUE)
    if (!as.character(receipt$status[[1L]]) %in% c("verified", "repaired")) {
      return(list(status = "blocked", error_category = "target-receipt-failed",
        error_message = "A previously registered target object receipt could not be verified."))
    }
    # Recover a crash after catalog registration but before the sidecar switch.
    switched <- acquisition_storage_migration_fenced_sync_manifest(catalog,
      as.character(relocation$relocation_id[[1L]]), job, rows[[i]], target_root_id,
      relative[[i]], target_path, cache_root, rows[[i]]$source_ref_id[[1L]])
    if (!identical(as.character(switched$status[[1L]]), "succeeded")) {
      return(list(status = as.character(switched$status[[1L]]),
        error_category = switched$error_category %||% "sidecar-recovery-failed",
        error_message = switched$error_message %||% "The sidecar location could not be recovered."))
    }
  }
  pending <- which(vapply(target_claims, is.null, logical(1)))
  if (length(pending) == 0L) {
    refreshed <- acquisition_storage_migration_fenced_manifests(catalog,
      as.character(relocation$relocation_id[[1L]]), job, unique(claims$source_ref_id), cache_root)
    if (!identical(refreshed$status, "succeeded")) return(refreshed)
    return(list(status = "succeeded", migrated = 0L, remaining = 0L))
  }
  selected <- head(pending, batch_size)
  selected_paths <- vapply(selected, function(i) relative[[i]], character(1))
  unique_paths <- unique(selected_paths)
  unique_path_indexes <- match(unique_paths, selected_paths)
  copy_bytes <- sum(vapply(seq_along(unique_paths), function(j) {
    path <- unique_paths[[j]]
    target_path <- planscanR::storage_path(target_root, path)
    if (file.exists(target_path)) 0 else as.numeric(rows[[selected[[unique_path_indexes[[j]]]]]]$size_bytes[[1L]])
  }, numeric(1)))
  # Staging and the retained source coexist with the published object until
  # atomic rename; reserve peak duplicate bytes, not just final object size.
  capacity <- acquisition_storage_migration_capacity(catalog, job, target_root_id, copy_bytes * 2)
  if (!isTRUE(capacity$ok)) return(list(status = "blocked", error_category = capacity$reason,
    error_message = "The target root does not have the reserved space required for this verified batch."))

  migrated <- 0L
  for (j in seq_along(selected)) {
    i <- selected[[j]]
    row <- rows[[i]]
    source_path <- planscanR::storage_path(source_root, as.character(row$relative_path[[1L]]))
    if (!identical(as.character(row$verification_status[[1L]]), "verified") ||
      !acquisition_storage_migration_digest_matches(source_path, row$sha256[[1L]], row$size_bytes[[1L]])) {
      return(list(status = "blocked", error_category = "source-verification-failed",
        error_message = "The registered source claim does not match its verified size and SHA-256."))
    }
    target_path <- planscanR::storage_path(target_root, relative[[i]], create_parent = TRUE)
    if (file.exists(target_path)) {
      if (!acquisition_storage_migration_digest_matches(target_path, row$sha256[[1L]], row$size_bytes[[1L]])) {
        return(list(status = "blocked", error_category = "object-integrity-conflict",
          error_message = "An existing content-addressed object has different bytes; it was left untouched."))
      }
    } else {
      part <- tempfile(pattern = paste0(basename(target_path), "."),
        tmpdir = dirname(target_path), fileext = ".part")
      copied <- tryCatch(copy_fn(source_path, part, overwrite = FALSE), error = function(error) FALSE)
      if (!isTRUE(copied) || !acquisition_storage_migration_digest_matches(part,
        row$sha256[[1L]], row$size_bytes[[1L]])) {
        if (file.exists(part)) unlink(part)
        return(list(status = "retryable", error_category = "copy-verification-failed",
          error_message = "The staged copy was incomplete or failed SHA-256 verification."))
      }
      if (!isTRUE(file.rename(part, target_path))) {
        if (file.exists(part)) unlink(part)
        if (!acquisition_storage_migration_digest_matches(target_path,
          row$sha256[[1L]], row$size_bytes[[1L]])) {
          return(list(status = "retryable", error_category = "copy-publish-failed",
            error_message = "The verified staged object could not be atomically published."))
        }
      }
    }
    receipt <- planscanR::reconcile_cached_download(target_path,
      expected_size_bytes = row$size_bytes[[1L]], expected_sha256 = row$sha256[[1L]],
      repair = TRUE)
    if (!as.character(receipt$status[[1L]]) %in% c("verified", "repaired")) {
      return(list(status = "blocked", error_category = "object-receipt-failed",
        error_message = as.character(receipt$reason[[1L]] %||% "Object receipt verification failed.")))
    }
    registered <- acquisition_storage_migration_fenced_register(catalog,
      as.character(relocation$relocation_id[[1L]]), job, row, target_root_id, relative[[i]])
    if (!identical(registered$status, "succeeded")) return(registered)
    switched <- acquisition_storage_migration_fenced_sync_manifest(catalog,
      as.character(relocation$relocation_id[[1L]]), job, row, target_root_id,
      relative[[i]], target_path, cache_root, as.character(row$source_ref_id[[1L]]))
    if (!identical(as.character(switched$status[[1L]]), "succeeded")) {
      return(list(status = as.character(switched$status[[1L]]),
        error_category = switched$error_category %||% "sidecar-switch-failed",
        error_message = switched$error_message %||% "The sidecar location could not be switched."))
    }
    migrated <- migrated + 1L
  }
  refreshed <- acquisition_storage_migration_fenced_manifests(catalog,
    as.character(relocation$relocation_id[[1L]]), job, unique(claims$source_ref_id), cache_root)
  if (!identical(refreshed$status, "succeeded")) return(refreshed)
  remaining <- sum(vapply(pending, function(i) is.null(acquisition_storage_migration_target_claim(
    catalog, rows[[i]]$document_ref_id[[1L]], target_root_id, relative[[i]]
  )), logical(1)))
  if (remaining > 0L) {
  return(list(status = "retryable", migrated = migrated, remaining = remaining,
      retry_after_seconds = 1, error_category = "migration-batch-continue",
      error_message = "A verified batch completed; the queued relocation will resume with remaining claims."))
  }
  list(status = "succeeded", migrated = migrated, remaining = 0L)
}

acquisition_storage_write_migration_manifests <- function(catalog, source_ref_ids, cache_root) {
  for (source_ref_id in source_ref_ids) {
    acquisition_catalog_write_project_manifest(catalog, source_ref_id,
      path = file.path(cache_root, "projects", source_ref_id, "manifest.json"))
  }
  invisible(TRUE)
}

#' Roll back migrated sidecar references to the retained verified source copies
#'
#' Rollback only switches references and advances the preferred manifest
#' location back to the verified source. It never removes the target object or
#' any unverified file.
#'
#' @param catalog Acquisition catalog.
#' @param relocation_id Relocation ID returned by
#'   [acquisition_storage_enqueue_relocation()].
#' @param actor Authenticated administrator.
#' @param cache_root Cache root for sidecars and portable project manifests.
#' @return A list with `status` and the count of references restored.
#' @export
acquisition_storage_rollback_relocation <- function(catalog, relocation_id, actor,
                                                    cache_root = NULL) {
  acquisition_storage_require_admin(actor)
  relocation_id <- acquisition_catalog_scalar_character(relocation_id, "relocation_id")
  cache_root <- cache_root %||% planscanR::cache_dir_default()
  relocation <- acquisition_catalog_one(acquisition_catalog_connection(catalog),
    "SELECT * FROM acquisition_storage_relocations WHERE relocation_id = ?", list(relocation_id))
  if (is.null(relocation)) return(list(status = "blocked", restored = 0L, error_category = "missing-relocation"))
  # Fence queued/running workers before switching any references. A stale
  # worker then fails its next relocation-state check and its lease completion.
  acquisition_catalog_transaction(catalog, function(conn) {
    rollback_time <- acquisition_catalog_now()
    DBI::dbExecute(conn,
      "UPDATE acquisition_storage_relocations SET state = 'cancelled', updated_at = ? WHERE relocation_id = ?",
      params = list(rollback_time, relocation_id))
    if (!is.na(relocation$job_id[[1L]])) {
      DBI::dbExecute(conn,
        "UPDATE acquisition_jobs SET state = 'cancelled', lease_token = NULL, lease_expires_at = NULL, updated_at = ? WHERE job_id = ? AND state IN ('queued', 'retry_wait', 'running')",
        params = list(rollback_time, as.character(relocation$job_id[[1L]])))
      DBI::dbExecute(conn,
        "UPDATE acquisition_job_attempts SET outcome = 'cancelled', finished_at = ?, error_category = 'relocation-rollback' WHERE job_id = ? AND outcome = 'running'",
        params = list(rollback_time, as.character(relocation$job_id[[1L]])))
      acquisition_storage_release_connection(conn, as.character(relocation$job_id[[1L]]))
    }
    acquisition_queue_sync_host_counts(conn, rollback_time)
    invisible(TRUE)
  })
  source_root_id <- as.character(relocation$source_root_id[[1L]])
  target_root_id <- as.character(relocation$target_root_id[[1L]])
  roots <- acquisition_storage_roots(catalog)
  root_paths <- stats::setNames(as.character(roots$root_path), roots$root_id)
  prior_roots <- getOption("planscanR.storage_roots", NULL)
  options(planscanR.storage_roots = root_paths)
  on.exit(options(planscanR.storage_roots = prior_roots), add = TRUE)
  pairs <- acquisition_storage_migration_query(catalog, paste(
    "SELECT src.document_ref_id, src.root_id, src.relative_path, src.sha256, src.size_bytes,",
    "src.legacy_path, d.source_ref_id, d.attachment_url, d.legacy_local_path,",
    "s.country, s.portal, s.native_project_id, s.legacy_sidecar_path",
    "FROM acquisition_files dst JOIN acquisition_files src ON src.document_ref_id = dst.document_ref_id",
    "AND src.sha256 = dst.sha256 JOIN acquisition_documents d ON d.document_ref_id = src.document_ref_id",
    "JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
    "WHERE dst.root_id = ? AND dst.provenance = 'storage-migration' AND src.root_id = ?",
    "AND src.verification_status = 'verified'"
  ), list(target_root_id, source_root_id))
  restored <- 0L
  for (i in seq_len(nrow(pairs))) {
    row <- pairs[i, , drop = FALSE]
    source_path <- planscanR::storage_path(root_paths[[source_root_id]], row$relative_path[[1L]])
    if (!acquisition_storage_migration_digest_matches(source_path, row$sha256[[1L]], row$size_bytes[[1L]])) {
      return(list(status = "blocked", restored = restored, error_category = "source-verification-failed",
        error_message = "A retained source copy failed size or SHA-256 verification."))
    }
    restored_result <- acquisition_storage_rollback_fenced_restore(catalog,
      relocation_id, row, source_root_id, source_path, cache_root)
    if (!identical(restored_result$status, "succeeded")) {
      return(list(status = restored_result$status, restored = restored,
        error_category = restored_result$error_category, error_message = restored_result$error_message))
    }
    restored <- restored + 1L
  }
  source_ids <- unique(as.character(pairs$source_ref_id))
  if (length(source_ids)) acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_storage_write_migration_manifests(catalog, source_ids, cache_root)
  })
  list(status = "succeeded", restored = restored,
    retained_target_copies = nrow(pairs))
}
