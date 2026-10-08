# ACQ-UI4: a read-only model/campaign preview and a small optional Shiny panel.
#
# The catalog, model registry, and queue remain the source of truth.  Preview
# only reads those services; authenticated submission delegates to the existing
# immutable campaign API.

acquisition_campaign_identity_abort <- function(message) {
  acquisition_campaign_abort(
    message,
    class = "planscanR_error_acquisition_identity"
  )
}

#' Register a published C3 model in the acquisition catalog.
#'
#' The model file is copied and fingerprinted by the existing catalog registry;
#' this helper only connects the two immutable artifact stores.
#' @export
acquisition_catalog_register_curation_model <- function(
  catalog,
  artifact_store,
  scope,
  model_id,
  actor,
  input_snapshot_id = NULL,
  metadata = NULL
) {
  curation_check_artifact_store(artifact_store)
  curation_check_scope(scope, class = "planscanR_error_acquisition")
  curation_check_actor(actor)
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  loaded <- curation_artifact_read_model(artifact_store, scope, model_id, actor)
  if (is.null(loaded) || is.null(loaded$path)) {
    acquisition_campaign_abort("The published curation model artifact is unavailable.")
  }
  provenance <- loaded$provenance %||% list()
  input <- provenance$input %||% list()
  input_snapshot_id <- input_snapshot_id %||% input$snapshot_id %||% input$version
  if (is.null(input_snapshot_id)) {
    acquisition_campaign_abort("The published curation model has no immutable input snapshot identity.")
  }
  acquisition_catalog_register_model(
    catalog = catalog,
    model_id = model_id,
    artifact_path = loaded$path,
    config_id = scope$config_id,
    input_snapshot_id = input_snapshot_id,
    metadata = utils::modifyList(provenance, metadata %||% list()),
    created_by = actor$id
  )
}

acquisition_campaign_source_rows <- function(catalog, selection) {
  if (!is.data.frame(selection) || nrow(selection) == 0L) {
    acquisition_campaign_abort("{.arg selection} must contain at least one source row.")
  }
  has_source <- "source_ref_id" %in% names(selection)
  has_native_project <- "native_project_id" %in% names(selection)
  has_document <- "document_id" %in% names(selection)
  has_composite <- all(c("country", "portal") %in% names(selection)) && (has_native_project || has_document)
  if (!has_source && !has_composite) {
    acquisition_campaign_identity_abort("Selection must identify each source with source_ref_id or the full country, portal, and project identity composite.")
  }
  conn <- acquisition_catalog_connection(catalog)
  native_project <- if (has_native_project) selection$native_project_id else selection$document_id
  source_ids <- if (has_source) {
    as.character(selection$source_ref_id)
  } else {
    vapply(seq_len(nrow(selection)), function(index) {
      row <- acquisition_catalog_one(
        conn,
        paste(
          "SELECT source_ref_id FROM acquisition_sources",
          "WHERE country = ? AND portal = ? AND native_project_id = ?"
        ),
        params = list(
          as.character(selection$country[[index]]),
          as.character(selection$portal[[index]]),
          as.character(native_project[[index]])
        )
      )
      if (is.null(row)) NA_character_ else as.character(row$source_ref_id[[1L]])
    }, character(1))
  }
  if (anyNA(source_ids) || any(!nzchar(source_ids)) || anyDuplicated(source_ids)) {
    acquisition_campaign_identity_abort("Selection source identities must be unique, known, and non-empty.")
  }
  chunks <- split(source_ids, ceiling(seq_along(source_ids) / 900L))
  rows <- do.call(rbind, lapply(chunks, function(chunk) {
    placeholders <- paste(rep("?", length(chunk)), collapse = ",")
    DBI::dbGetQuery(
      conn,
      paste(
        "SELECT source_ref_id, country, portal, native_project_id FROM acquisition_sources",
        "WHERE source_ref_id IN (", placeholders, ") ORDER BY source_ref_id"
      ),
      params = as.list(chunk)
    )
  }))
  if (nrow(rows) != length(source_ids)) {
    acquisition_campaign_identity_abort("Every selected source must already exist in the acquisition catalog.")
  }
  rows <- rows[match(source_ids, rows$source_ref_id), , drop = FALSE]
  rownames(rows) <- NULL
  if (has_composite) {
    mismatched <- as.character(selection$country) != rows$country |
      as.character(selection$portal) != rows$portal |
      as.character(native_project) != rows$native_project_id
    if (any(mismatched)) {
      acquisition_campaign_identity_abort(
        "The supplied country, portal, and project identity do not match the catalog source; refusing to merge portal identities."
      )
    }
  }
  rows
}

acquisition_campaign_validate_items_for_preview <- function(catalog, items) {
  chunks <- split(seq_along(items), ceiling(seq_along(items) / 900L))
  do.call(c, lapply(chunks, function(indices) {
    acquisition_campaign_validate_items(catalog, items[indices])
  }))
}

acquisition_campaign_selection_from_records <- function(catalog, records, threshold) {
  if (!is.data.frame(records) || nrow(records) == 0L) {
    acquisition_campaign_abort("The prepared acquisition snapshot has no candidate records.")
  }
  required <- c("country", "document_id", "select_prob")
  missing <- setdiff(required, names(records))
  if (length(missing) > 0L) {
    acquisition_campaign_identity_abort(
      paste0("Prepared acquisition records are missing: ", paste(missing, collapse = ", "), ".")
    )
  }
  conn <- acquisition_catalog_connection(catalog)
  rows <- lapply(seq_len(nrow(records)), function(index) {
    country <- as.character(records$country[[index]])
    native_project_id <- as.character(records$document_id[[index]])
    supplied_portal <- if ("portal" %in% names(records)) {
      as.character(records$portal[[index]])
    } else {
      NA_character_
    }
    has_supplied_portal <- !is.na(supplied_portal) && nzchar(supplied_portal)
    source_where <- if (has_supplied_portal) {
      "WHERE country = ? AND portal = ? AND native_project_id = ?"
    } else {
      "WHERE country = ? AND native_project_id = ?"
    }
    source_params <- if (has_supplied_portal) {
      list(country, supplied_portal, native_project_id)
    } else {
      list(country, native_project_id)
    }
    candidates <- DBI::dbGetQuery(
      conn,
      paste(
        "SELECT source_ref_id, country, portal, native_project_id FROM acquisition_sources",
        source_where,
        "ORDER BY portal, source_ref_id"
      ),
      params = source_params
    )
    if (nrow(candidates) == 0L) {
      acquisition_campaign_identity_abort(
        if (has_supplied_portal) {
          paste0("The prepared record portal does not match the catalog source for `", country, "/", native_project_id, "`.")
        } else {
          paste0("No catalog source matches country `", country, "` and native_project_id `", native_project_id, "`.")
        }
      )
    }
    if (nrow(candidates) != 1L) {
      acquisition_campaign_identity_abort(
        paste0("The prepared record `", country, "/", native_project_id, "` has ambiguous portal identity.")
      )
    }
    candidates[1L, , drop = FALSE]
  })
  source_rows <- do.call(rbind, rows)
  if (anyDuplicated(source_rows$source_ref_id)) {
    acquisition_campaign_identity_abort("Prepared records must map to unique catalog sources.")
  }
  probability <- vapply(records$select_prob, acquisition_campaign_probability, numeric(1))
  selection <- data.frame(
    source_ref_id = as.character(source_rows$source_ref_id),
    country = as.character(source_rows$country),
    portal = as.character(source_rows$portal),
    native_project_id = as.character(source_rows$native_project_id),
    probability = probability,
    selected = !is.na(probability) & probability >= threshold,
    stringsAsFactors = FALSE
  )
  for (column in intersect(
    c(
      "energy_tags", "energy_tag", "energy", "energy_evidence", "energy_source",
      "energy_confidence", "confidence", "manual_energy_tag", "energy_override"
    ),
    names(records)
  )) {
    selection[[column]] <- records[[column]]
  }
  selection
}

acquisition_campaign_preview_from_curation <- function(
  catalog,
  model_reference,
  records,
  threshold,
  policy_version,
  priority_rules,
  records_snapshot_id = NULL,
  records_snapshot_version = NULL,
  records_snapshot_fingerprint = NULL
) {
  if (!is.list(model_reference) || !inherits(model_reference$model, "planscanR_selection_model")) {
    return(NULL)
  }
  if (is.null(model_reference$model_id) || is.null(model_reference$input_snapshot_id)) {
    acquisition_campaign_abort("The active curation model has no immutable model or snapshot identity.")
  }
  provenance <- model_reference$provenance %||% list()
  input <- provenance$input %||% list()
  expected_version <- model_reference$input_snapshot_version %||%
    input$snapshot_version %||% provenance$snapshot_version %||%
    (provenance$corpus_snapshot %||% list())$version %||% NULL
  if (is.null(records_snapshot_id) || is.null(records_snapshot_version) || is.null(records_snapshot_fingerprint)) {
    acquisition_campaign_abort(
      "The prepared acquisition snapshot must supply an immutable ID, version, and fingerprint.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  if (is.null(policy_version) || is.null(priority_rules)) {
    return(NULL)
  }
  artifact_path <- model_reference$artifact_path %||% NULL
  expected_artifact_sha256 <- if (is.null(artifact_path)) {
    NULL
  } else {
    if (!file.exists(artifact_path) || isTRUE(file.info(artifact_path)$isdir)) {
      acquisition_campaign_abort(
        "The published curation model artifact is unavailable.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
    acquisition_catalog_sha256(normalizePath(artifact_path, mustWork = TRUE))
  }
  list(
    selection = acquisition_campaign_selection_from_records(catalog, records, threshold),
    model_id = model_reference$model_id,
    model_input_snapshot_id = model_reference$input_snapshot_id,
    model_input_snapshot_version = model_reference$input_snapshot_version %||% expected_version,
    prepared_snapshot_id = as.character(records_snapshot_id),
    prepared_snapshot_version = as.character(records_snapshot_version),
    prepared_snapshot_fingerprint = as.character(records_snapshot_fingerprint),
    input_snapshot_id = model_reference$input_snapshot_id,
    threshold = threshold,
    policy_version = policy_version,
    priority_rules = priority_rules,
    artifact_path = artifact_path,
    config_id = model_reference$config_id %||% NULL,
    model_metadata = provenance,
    expected_artifact_sha256 = expected_artifact_sha256
  )
}

acquisition_campaign_preview_model <- function(
  catalog,
  model_id,
  input_snapshot_id,
  artifact_path = NULL,
  config_id = NULL,
  metadata = NULL,
  created_by = NULL
) {
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  input_snapshot_id <- acquisition_catalog_scalar_character(input_snapshot_id, "input_snapshot_id")
  row <- acquisition_catalog_one(
    acquisition_catalog_connection(catalog),
    "SELECT * FROM acquisition_models WHERE model_id = ?",
    list(model_id)
  )
  if (is.null(row) && !is.null(artifact_path)) {
    artifact_path <- acquisition_catalog_scalar_character(artifact_path, "artifact_path")
    if (!file.exists(artifact_path) || isTRUE(file.info(artifact_path)$isdir)) {
      acquisition_campaign_abort("The supplied model artifact is unavailable.")
    }
    if (is.null(config_id)) {
      acquisition_campaign_abort("{.arg config_id} is required for an unregistered model preview.")
    }
    config_id <- acquisition_catalog_scalar_character(config_id, "config_id")
    return(list(
      model_id = model_id,
      input_snapshot_id = input_snapshot_id,
      artifact_sha256 = acquisition_catalog_sha256(normalizePath(artifact_path, mustWork = TRUE)),
      artifact_path = normalizePath(artifact_path, mustWork = TRUE),
      config_id = config_id,
      metadata = metadata %||% list(),
      created_by = created_by %||% NA_character_,
      created_at = NA_character_,
      registered = FALSE
    ))
  }
  if (is.null(row)) {
    acquisition_campaign_abort("The selected immutable model is not registered in the acquisition catalog.")
  }
  registered_snapshot <- as.character(row$input_snapshot_id[[1L]])
  if (!identical(registered_snapshot, input_snapshot_id)) {
    acquisition_campaign_abort(
      "The preview snapshot does not match the registered model input snapshot.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  registered_path <- as.character(row$artifact_path[[1L]])
  registered_sha <- as.character(row$artifact_sha256[[1L]])
  if (!file.exists(registered_path) || isTRUE(file.info(registered_path)$isdir) ||
    !identical(acquisition_catalog_sha256(registered_path), registered_sha)) {
    acquisition_campaign_abort(
      "The registered model artifact does not match its immutable reference.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  list(
    model_id = model_id,
    input_snapshot_id = registered_snapshot,
    artifact_sha256 = as.character(row$artifact_sha256[[1L]]),
    artifact_path = registered_path,
    config_id = as.character(row$config_id[[1L]]),
    metadata = acquisition_campaign_unblob(row$metadata_blob[[1L]]) %||% list(),
    created_by = as.character(row$created_by[[1L]]),
    created_at = as.character(row$created_at[[1L]]),
    registered = TRUE
  )
}

#' Preview an immutable model-driven campaign without writing catalog state.
#'
#' Attachment and file totals remain explicitly unknown until supervised source
#' discovery runs.  The returned `jobs` count is always zero because preview is
#' read-only.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param selection Source rows carrying portal-aware identity and probability.
#' @param model_id Registered immutable model identifier.
#' @param input_snapshot_id Model input snapshot identifier.
#' @param threshold Selection probability threshold.
#' @param policy_version Immutable caller policy identity.
#' @param priority_rules Ordered country/energy rules.
#' @param artifact_path Optional model artifact path for an unregistered preview.
#' @param config_id Optional model configuration identity for an unregistered preview.
#' @param model_metadata Optional model provenance metadata.
#' @param created_by Optional model creator identity.
#' @param model_input_snapshot_id Optional model training snapshot identity.
#' @param model_input_snapshot_version Optional model training snapshot version.
#' @param prepared_snapshot_id Optional prepared acquisition snapshot identity.
#' @param prepared_snapshot_version Optional prepared acquisition snapshot version.
#' @param prepared_snapshot_fingerprint Optional prepared acquisition snapshot fingerprint.
#' @param expected_artifact_sha256 Optional digest captured before a later submit.
#' @return A read-only preview list containing provenance, items, exclusions,
#'   counts, and explicit unknown attachment totals.
#' @export
acquisition_campaign_preview <- function(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  artifact_path = NULL,
  config_id = NULL,
  model_metadata = NULL,
  created_by = NULL,
  model_input_snapshot_id = NULL,
  model_input_snapshot_version = NULL,
  prepared_snapshot_id = NULL,
  prepared_snapshot_version = NULL,
  prepared_snapshot_fingerprint = NULL,
  expected_artifact_sha256 = NULL
) {
  acquisition_catalog_connection(catalog)
  model_input_snapshot_id <- model_input_snapshot_id %||% input_snapshot_id
  model <- acquisition_campaign_preview_model(
    catalog,
    model_id,
    model_input_snapshot_id,
    artifact_path = artifact_path,
    config_id = config_id,
    metadata = model_metadata,
    created_by = created_by
  )
  if (!is.null(expected_artifact_sha256) && !identical(
    as.character(expected_artifact_sha256), as.character(model$artifact_sha256)
  )) {
    acquisition_campaign_abort(
      "The preview artifact hash does not match the selected model bytes.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  policy_version <- acquisition_catalog_scalar_character(policy_version, "policy_version")
  threshold <- suppressWarnings(as.numeric(threshold))
  if (length(threshold) != 1L || is.na(threshold) || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    acquisition_campaign_abort("{.arg threshold} must be between 0 and 1.")
  }
  rules <- acquisition_campaign_normalise_rules(priority_rules)
  items <- acquisition_campaign_items_from_selection(catalog, selection, threshold = threshold)
  items <- acquisition_campaign_validate_items_for_preview(catalog, items)
  for (i in seq_along(items)) {
    match <- acquisition_campaign_match_rule(
      rules, items[[i]]$country, items[[i]]$energy_kind, items[[i]]$energy_tags
    )
    items[[i]]$priority <- match$priority
    items[[i]]$priority_rule_id <- match$rule_id
    items[[i]]$priority_explanation <- match$explanation
    items[[i]]$exclusion_reason <- if (isTRUE(items[[i]]$selected)) {
      NA_character_
    } else if (
      !is.na(items[[i]]$manual_override)
    ) {
      "manual exclusion"
    } else if (!is.na(items[[i]]$probability) && items[[i]]$probability < threshold) {
      "below threshold"
    } else {
      "not selected"
    }
  }
  item_frame <- do.call(rbind, lapply(items, function(item) {
    data.frame(
      source_ref_id = item$source_ref_id,
      country = item$country,
      portal = item$portal,
      native_project_id = item$native_project_id,
      title = item$title %||% NA_character_,
      probability = item$probability,
      selected = item$selected,
      energy_kind = item$energy_kind,
      energy_tags = paste(item$energy_tags, collapse = ","),
      priority = item$priority,
      priority_rule_id = item$priority_rule_id,
      priority_explanation = item$priority_explanation,
      exclusion_reason = item$exclusion_reason,
      stringsAsFactors = FALSE
    )
  }))
  selected <- vapply(items, `[[`, logical(1), "selected")
  countries <- unique(vapply(items[selected], `[[`, character(1), "country"))
  list(
    model_id = model$model_id,
    input_snapshot_id = model$input_snapshot_id,
    model_input_snapshot_id = model$input_snapshot_id,
    model_input_snapshot_version = model_input_snapshot_version,
    prepared_snapshot_id = prepared_snapshot_id,
    prepared_snapshot_version = prepared_snapshot_version,
    prepared_snapshot_fingerprint = prepared_snapshot_fingerprint,
    expected_artifact_sha256 = model$artifact_sha256,
    artifact_sha256 = model$artifact_sha256,
    artifact_path = model$artifact_path,
    model_config_id = model$config_id,
    model_registered = model$registered,
    policy_version = policy_version,
    threshold = threshold,
    priority_rules = rules,
    scope = list(
      countries = countries,
      energy = unique(item_frame$energy_kind[item_frame$selected]),
      source_ids = item_frame$source_ref_id[item_frame$selected]
    ),
    items = item_frame,
    exclusions = item_frame[!item_frame$selected, c("source_ref_id", "country", "portal", "native_project_id", "probability", "exclusion_reason"), drop = FALSE],
    counts = list(
      projects = as.integer(nrow(item_frame)),
      selected_projects = as.integer(sum(item_frame$selected)),
      excluded_projects = as.integer(sum(!item_frame$selected)),
      jobs = 0L
    ),
    jobs = 0L,
    attachments = list(
      unknown = TRUE,
      projects = as.integer(sum(item_frame$selected)),
      files = NA_integer_,
      bytes = NA_integer_,
      reason = "Attachment and file totals are unknown until source discovery runs."
    ),
    previewed_at = acquisition_catalog_now()
  )
}

#' Submit a campaign after resolving the current authenticated admin.
#'
#' The resolver is evaluated at mutation time, so a reviewer or revoked actor
#' cannot reuse a previously rendered preview to submit work.
#' @param model_input_snapshot_id Optional model training snapshot identity when
#'   the campaign uses a separate prepared snapshot.
#' @param model_input_snapshot_version Optional model training snapshot version.
#' @param prepared_snapshot_id Optional prepared acquisition snapshot identity.
#' @param prepared_snapshot_version Optional prepared acquisition snapshot version.
#' @param prepared_snapshot_fingerprint Optional prepared acquisition snapshot fingerprint.
#' @param expected_artifact_sha256 Digest captured by preview and checked before
#'   model registration or campaign writes.
#' @export
acquisition_campaign_submit_authenticated <- function(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  actor_resolver,
  request = list(),
  campaign_id = NULL,
  metadata = NULL,
  model_artifact_path = NULL,
  model_config_id = NULL,
  model_metadata = NULL,
  artifact_path = NULL,
  config_id = NULL,
  model_input_snapshot_id = NULL,
  model_input_snapshot_version = NULL,
  prepared_snapshot_id = NULL,
  prepared_snapshot_version = NULL,
  prepared_snapshot_fingerprint = NULL,
  expected_artifact_sha256 = NULL
) {
  actor <- acquisition_controls_admin_actor(actor_resolver, request)
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  model_input_snapshot_id <- acquisition_catalog_scalar_character(
    model_input_snapshot_id %||% input_snapshot_id,
    "model_input_snapshot_id"
  )
  campaign_snapshot_id <- acquisition_catalog_scalar_character(
    prepared_snapshot_id %||% input_snapshot_id,
    "input_snapshot_id"
  )
  model_artifact_path <- model_artifact_path %||% artifact_path
  model_config_id <- model_config_id %||% config_id
  if (!is.null(model_artifact_path)) {
    model_artifact_path <- acquisition_catalog_scalar_character(model_artifact_path, "model_artifact_path")
  }
  if (!is.null(model_config_id)) {
    model_config_id <- acquisition_catalog_scalar_character(model_config_id, "model_config_id")
  }
  if (!is.null(expected_artifact_sha256)) {
    expected_artifact_sha256 <- acquisition_catalog_scalar_character(
      expected_artifact_sha256,
      "expected_artifact_sha256"
    )
  }
  conn <- acquisition_catalog_connection(catalog)
  model_row <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_models WHERE model_id = ?",
    list(model_id)
  )
  if (is.null(model_row)) {
    if (is.null(model_artifact_path) || is.null(model_config_id)) {
      acquisition_campaign_abort(
        "The immutable model must be registered before submission, or a model artifact and config must be supplied."
      )
    }
    if (!is.null(expected_artifact_sha256)) {
      if (!file.exists(model_artifact_path) || isTRUE(file.info(model_artifact_path)$isdir) ||
        !identical(acquisition_catalog_sha256(model_artifact_path), expected_artifact_sha256)) {
        acquisition_campaign_abort(
          "The submitted model artifact changed after preview.",
          class = "planscanR_error_acquisition_immutable"
        )
      }
    }
    acquisition_catalog_register_model(
      catalog,
      model_id = model_id,
      artifact_path = model_artifact_path,
      config_id = model_config_id,
      input_snapshot_id = model_input_snapshot_id,
      metadata = model_metadata,
      created_by = actor$id
    )
    model_row <- acquisition_catalog_one(
      conn,
      "SELECT * FROM acquisition_models WHERE model_id = ?",
      list(model_id)
    )
  }
  if (is.null(model_row)) {
    acquisition_campaign_abort("The immutable model registration could not be read back.")
  }
  registered_snapshot <- as.character(model_row$input_snapshot_id[[1L]])
  if (!identical(registered_snapshot, model_input_snapshot_id)) {
    acquisition_campaign_abort(
      "The submission snapshot does not match the registered model input snapshot.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  registered_config <- as.character(model_row$config_id[[1L]])
  if (!is.null(model_config_id) && !identical(registered_config, model_config_id)) {
    acquisition_campaign_abort(
      "The supplied model configuration does not match the registered model.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  managed_path <- as.character(model_row$artifact_path[[1L]])
  registered_sha <- as.character(model_row$artifact_sha256[[1L]])
  if (!is.null(expected_artifact_sha256) && !identical(registered_sha, expected_artifact_sha256)) {
    acquisition_campaign_abort(
      "The submitted model artifact hash does not match the preview.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  if (!file.exists(managed_path) || isTRUE(file.info(managed_path)$isdir)) {
    acquisition_campaign_abort(
      "The registered model artifact is missing.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  if (!identical(acquisition_catalog_sha256(managed_path), registered_sha)) {
    acquisition_campaign_abort(
      "The registered model artifact hash no longer matches its immutable reference.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  if (!is.null(model_artifact_path)) {
    if (!file.exists(model_artifact_path) || isTRUE(file.info(model_artifact_path)$isdir)) {
      acquisition_campaign_abort(
        "The supplied model artifact is unavailable.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
    if (!identical(acquisition_catalog_sha256(model_artifact_path), registered_sha)) {
      acquisition_campaign_abort(
        "The supplied model artifact does not match the registered immutable model.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
  }
  result <- acquisition_campaign_submit(
    catalog = catalog,
    selection = selection,
    model_id = model_id,
    input_snapshot_id = campaign_snapshot_id,
    threshold = threshold,
    policy_version = policy_version,
    priority_rules = priority_rules,
    submitted_by = actor$id,
    campaign_id = campaign_id,
    metadata = utils::modifyList(
      metadata %||% list(),
      list(
        model_input_snapshot_id = registered_snapshot,
        model_input_snapshot_version = model_input_snapshot_version,
        prepared_snapshot_id = campaign_snapshot_id,
        prepared_snapshot_version = prepared_snapshot_version,
        prepared_snapshot_fingerprint = prepared_snapshot_fingerprint,
        model_artifact_sha256 = registered_sha,
        expected_artifact_sha256 = expected_artifact_sha256 %||% registered_sha
      )
    )
  )
  result$submitted_by <- actor$id
  result
}

acquisition_campaign_panel_preview_args <- function(preview) {
  if (is.function(preview)) {
    preview <- preview()
  }
  if (!is.list(preview)) {
    return(NULL)
  }
  if (is.function(preview$resolve_fn)) {
    preview <- preview$resolve_fn()
  }
  if (!is.list(preview)) {
    return(NULL)
  }
  preview
}

acquisition_campaign_panel_is_active <- function(active, isolate = FALSE) {
  if (is.null(active)) {
    return(TRUE)
  }
  value <- if (is.function(active)) {
    if (isTRUE(isolate)) shiny::isolate(active()) else active()
  } else {
    active
  }
  isTRUE(value)
}

acquisition_campaign_preview_items_page <- function(
  preview,
  search = NULL,
  sort_by = NULL,
  sort_desc = FALSE,
  page = 1L,
  page_size = 25L
) {
  items <- if (is.list(preview)) preview$items else NULL
  if (!is.data.frame(items)) {
    items <- data.frame(stringsAsFactors = FALSE)
  }
  if (nrow(items) > 0L && !"title" %in% names(items)) items$title <- NA_character_
  visible <- c(
    "title", "country", "portal", "native_project_id", "source_ref_id",
    "probability", "selected", "energy_kind", "priority", "priority_rule_id",
    "priority_explanation", "exclusion_reason"
  )
  items <- items[, intersect(visible, names(items)), drop = FALSE]
  items <- curation_app_query_rows(items, search = search, sort_by = sort_by, sort_desc = sort_desc)
  curation_app_page_rows(items, page = page, page_size = page_size)
}

acquisition_campaign_preview_display_table <- function(data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (nrow(data) == 0L) {
    return(data)
  }
  if ("title" %in% names(data)) {
    data$title <- curation_app_display_title(data$title)
  }
  if ("country" %in% names(data)) data$country <- toupper(as.character(data$country))
  if ("energy_kind" %in% names(data)) {
    data$energy_kind <- curation_app_display_label(data$energy_kind)
  }
  if ("selected" %in% names(data)) {
    # Summary rows carry numeric counts; only item rows carry the logical
    # selection flag that should be rendered as a state label.
    if (is.logical(data$selected)) {
      selected <- data$selected
      data$selected <- ifelse(is.na(selected), "Unknown", ifelse(selected, "Selected", "Excluded"))
    }
  }
  if ("priority_rule_id" %in% names(data)) {
    missing <- is.na(data$priority_rule_id) | !nzchar(trimws(as.character(data$priority_rule_id)))
    data$priority_rule_id <- as.character(data$priority_rule_id)
    data$priority_rule_id[missing] <- "Default priority"
  }
  if ("exclusion_reason" %in% names(data)) {
    missing <- is.na(data$exclusion_reason) | !nzchar(trimws(as.character(data$exclusion_reason)))
    data$exclusion_reason[missing] <- "None"
  }
  if ("probability" %in% names(data)) {
    probability <- suppressWarnings(as.numeric(data$probability))
    data$probability <- ifelse(
      is.na(probability), "Unknown", sprintf("%.0f%%", 100 * probability)
    )
  }
  names(data) <- vapply(names(data), acquisition_dashboard_display_name, character(1))
  names(data)[names(data) == "Title"] <- "Project title"
  names(data)[names(data) == "Priority rule id"] <- "Priority rule"
  names(data)[names(data) == "Priority explanation"] <- "Why this priority"
  names(data)[names(data) == "Exclusion reason"] <- "Reason excluded"
  data
}

#' Build the optional Acquisition campaign preview panel.
#' @param id Shiny module namespace.
#' @return A Shiny tag list.
#' @export
acquisition_campaign_panel_ui <- function(id = NULL) {
  ns <- if (is.null(id)) identity else shiny::NS(id)
  shiny::tags$div(
    class = "acquisition-campaign-preview",
    id = ns("campaign-preview"),
    shiny::tags$style(htmltools::HTML(
      ".acquisition-campaign-preview{max-width:100%;}",
      ".acquisition-campaign-preview table{max-width:100%;}",
      ".acquisition-campaign-preview .shiny-table{display:block;overflow-x:auto;}",
      ".acquisition-campaign-preview details{margin:8px 0;}",
      ".acquisition-campaign-preview summary{cursor:pointer;font-weight:600;padding:6px 0;}",
      ".acquisition-campaign-preview .curation-help{display:inline-flex;position:relative;align-items:center;justify-content:center;",
      "width:1.25rem;height:1.25rem;margin-left:.25rem;border:1px solid currentColor;border-radius:50%;",
      "font-size:.8rem;font-weight:600;cursor:help;}",
      ".acquisition-campaign-preview .curation-help:focus{outline:3px solid rgba(14,60,98,.35);outline-offset:2px;}",
      ".acquisition-campaign-preview .curation-help-text{display:none;position:absolute;z-index:1080;top:calc(100% + .35rem);left:0;",
      "width:18rem;padding:.55rem .7rem;border:1px solid #adb5bd;border-radius:.35rem;background:#fff;color:#212529;",
      "font-size:.875rem;font-weight:400;line-height:1.35;box-shadow:0 .25rem .75rem rgba(0,0,0,.15);}",
      ".acquisition-campaign-preview .curation-help:hover .curation-help-text,.acquisition-campaign-preview .curation-help:focus .curation-help-text{display:block;position:fixed;left:1rem;right:auto;top:auto;bottom:1rem;width:18rem;max-width:calc(100vw - 2rem);}",
      "@media (max-width:576px){.acquisition-campaign-preview .curation-help-text{position:fixed;left:1rem;right:1rem;top:auto;bottom:1rem;width:auto;}}"
    )),
    shiny::h2("Acquisition preview"),
    shiny::helpText("Preview is read-only; attachment and file totals stay unknown until discovery."),
    htmltools::div(
      class = "d-flex align-items-end flex-wrap gap-2",
      shiny::numericInput(ns("campaign-threshold"), "Selection threshold", value = 0.5, min = 0, max = 1, step = 0.01),
      curation_app_help(
        "Selection threshold",
        "Projects at or above this probability are selected for the preview. Changing it recalculates the read-only preview only."
      )
    ),
    shiny::uiOutput(ns("campaign-status")),
    shiny::tableOutput(ns("campaign-summary")),
    htmltools::tags$details(
      htmltools::tags$summary(
        "Advanced preview controls",
        curation_app_help(
          "Preview controls",
          "Search and sorting change the displayed page only. Preview never creates queue jobs."
        )
      ),
      htmltools::div(
        class = "curation-table-controls",
        style = "display:flex;align-items:flex-end;flex-wrap:wrap;gap:8px;margin:6px 0 10px;",
        shiny::textInput(ns("campaign-search"), "Search preview items", value = "", width = "260px"),
        shiny::selectInput(
          ns("campaign-sort"), "Sort by",
          choices = c("Source" = "source_ref_id", "Title" = "title", "Country" = "country", "Portal" = "portal", "Probability" = "probability", "Priority" = "priority"),
          selected = "source_ref_id"
        ),
        shiny::checkboxInput(ns("campaign-sort-desc"), "Descending", FALSE),
        shiny::selectInput(ns("campaign-page-size"), "Rows per page", choices = c(25, 50, 100, 250), selected = 25),
        shiny::numericInput(ns("campaign-page"), "Page", value = 1L, min = 1L, step = 1L),
        shiny::actionButton(ns("campaign-page-prev"), "Previous page", class = "btn-outline-secondary btn-sm"),
        shiny::actionButton(ns("campaign-page-next"), "Next page", class = "btn-outline-secondary btn-sm"),
        shiny::textOutput(ns("campaign-page-info"), inline = TRUE)
      )
    ),
    shiny::tableOutput(ns("campaign-items")),
    htmltools::div(
      class = "d-flex align-items-center flex-wrap gap-2",
      shiny::actionButton(ns("campaign-submit"), "Submit acquisition campaign"),
      shiny::helpText("Submission creates durable queue work for the selected projects and is restricted to administrators."),
      curation_app_help(
        "Submit acquisition campaign",
        "This is the explicit transition from a read-only preview to queued acquisition work. Confirm the target and next monitor step before submitting."
      )
    ),
    shiny::textOutput(ns("campaign-submit-result"))
  )
}

#' Run the optional Acquisition campaign preview/submission module.
#' @param id Shiny module namespace.
#' @param catalog Acquisition catalog.
#' @param preview Static preview arguments or a function returning them.
#' @param actor_resolver Trusted actor resolver.
#' @param request Optional request context.
#' @param submit_fn Optional policy adapter; defaults to the generic authenticated submitter.
#' @param active Optional function or reactive value indicating whether the
#'   Acquisition tab is active. Standalone panels are active by default.
#' @return Invisibly, `NULL`.
#' @export
acquisition_campaign_panel_server <- function(
  id,
  catalog,
  preview,
  actor_resolver,
  request = list(),
  submit_fn = acquisition_campaign_submit_authenticated,
  active = NULL
) {
  acquisition_dashboard_require_ui()
  acquisition_catalog_connection(catalog)
  acquisition_controls_check_resolver(actor_resolver)
  if (!is.function(submit_fn)) acquisition_campaign_abort("{.arg submit_fn} must be a function.")
  shiny::moduleServer(id, function(input, output, session) {
    preview_cache <- shiny::reactiveVal(NULL)
    preview_args_cache <- shiny::reactiveVal(NULL)
    args <- shiny::reactive({
      resolved <- acquisition_campaign_panel_preview_args(preview)
      if (is.null(resolved)) {
        return(NULL)
      }
      required <- c("selection", "model_id", "input_snapshot_id", "threshold", "policy_version", "priority_rules")
      if (!all(required %in% names(resolved))) {
        return(NULL)
      }
      resolved$threshold <- suppressWarnings(as.numeric(input$`campaign-threshold` %||% resolved$threshold))
      probability_columns <- intersect(
        c("probability", "select_prob", "selection_probability", "score"),
        names(resolved$selection)
      )
      if (length(probability_columns) > 0L && "selected" %in% names(resolved$selection)) {
        resolved$selection$selected <- vapply(seq_len(nrow(resolved$selection)), function(index) {
          probability <- acquisition_campaign_probability(
            acquisition_campaign_column_value(resolved$selection, index, probability_columns)
          )
          if (is.na(probability)) {
            isTRUE(resolved$selection$selected[[index]])
          } else {
            probability >= resolved$threshold
          }
        }, logical(1))
      }
      resolved
    })
    current_preview <- shiny::reactive({
      if (!acquisition_campaign_panel_is_active(active)) {
        return(preview_cache())
      }
      resolved <- args()
      if (is.null(resolved)) {
        return(NULL)
      }
      if (identical(preview_args_cache(), resolved)) {
        return(preview_cache())
      }
      value <- tryCatch(do.call(acquisition_campaign_preview, c(list(catalog = catalog), resolved)), error = identity)
      preview_args_cache(resolved)
      preview_cache(value)
      value
    })
    campaign_page <- shiny::reactiveVal(1L)
    campaign_page_size <- shiny::reactive({
      curation_app_page_spec(page_size = input$`campaign-page-size`)$page_size
    })
    campaign_page_data <- shiny::reactive({
      acquisition_campaign_preview_items_page(
        current_preview(),
        search = input$`campaign-search`,
        sort_by = input$`campaign-sort`,
        sort_desc = isTRUE(input$`campaign-sort-desc`),
        page = campaign_page(), page_size = campaign_page_size()
      )
    })
    set_campaign_page <- function(value) {
      value <- max(1L, suppressWarnings(as.integer(value[[1L]] %||% 1L)))
      campaign_page(value)
      shiny::updateNumericInput(session, "campaign-page", value = value)
    }
    shiny::observeEvent(
      list(input$`campaign-threshold`, input$`campaign-search`, input$`campaign-sort`, input$`campaign-sort-desc`, input$`campaign-page-size`),
      set_campaign_page(1L),
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$`campaign-page`, set_campaign_page(input$`campaign-page`), ignoreInit = TRUE)
    shiny::observeEvent(input$`campaign-page-prev`, set_campaign_page(campaign_page() - 1L), ignoreInit = TRUE)
    shiny::observeEvent(input$`campaign-page-next`,
      {
        page_data <- campaign_page_data()
        set_campaign_page(min(attr(page_data, "page") + 1L, attr(page_data, "page_count")))
      },
      ignoreInit = TRUE
    )
    output$`campaign-page-info` <- shiny::renderText({
      rows <- campaign_page_data()
      sprintf("Page %d of %d (%d items)", attr(rows, "page"), attr(rows, "page_count"), attr(rows, "total_count"))
    })
    output$`campaign-status` <- shiny::renderUI({
      if (!acquisition_campaign_panel_is_active(active)) {
        return(shiny::helpText("Acquisition preview is paused while the Acquisition tab is inactive."))
      }
      value <- current_preview()
      if (is.null(value)) {
        return(shiny::helpText("Acquisition preview is unavailable until a model and policy are supplied."))
      }
      if (inherits(value, "error")) {
        return(shiny::tags$p(conditionMessage(value), class = "text-danger"))
      }
      shiny::tags$p("No jobs created: this preview is read-only.")
    })
    output$`campaign-summary` <- shiny::renderTable(
      {
        value <- current_preview()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        acquisition_campaign_preview_display_table(data.frame(
          model = value$model_id,
          model_snapshot = value$model_input_snapshot_id %||% value$input_snapshot_id,
          prepared_snapshot = value$prepared_snapshot_id %||% value$input_snapshot_id,
          model_hash = value$artifact_sha256,
          threshold = value$threshold,
          policy = value$policy_version,
          projects = value$counts$projects,
          selected = value$counts$selected_projects,
          jobs = value$counts$jobs,
          attachments = "unknown until discovery",
          stringsAsFactors = FALSE
        ))
      },
      striped = TRUE
    )
    output$`campaign-items` <- shiny::renderTable(
      {
        acquisition_campaign_preview_display_table(campaign_page_data())
      },
      striped = TRUE
    )
    output$`campaign-submit-result` <- shiny::renderText({
      value <- input$`campaign-submit`
      if (is.null(value) || value < 1L) "No campaign submitted." else "Submit an explicit campaign to create durable work."
    })
    shiny::observeEvent(input$`campaign-submit`,
      {
        if (!acquisition_campaign_panel_is_active(active)) {
          output$`campaign-submit-result` <- shiny::renderText("Campaign submission is unavailable while the Acquisition tab is inactive.")
          return()
        }
        resolved <- args()
        if (is.null(resolved)) {
          output$`campaign-submit-result` <- shiny::renderText("Campaign submission is unavailable.")
          return()
        }
        displayed <- current_preview()
        if (is.null(displayed) || inherits(displayed, "error")) {
          output$`campaign-submit-result` <- shiny::renderText(
            if (inherits(displayed, "error")) conditionMessage(displayed) else "Campaign submission is unavailable."
          )
          return()
        }
        resolved$expected_artifact_sha256 <- displayed$artifact_sha256 %||%
          resolved$expected_artifact_sha256 %||% NULL
        result <- tryCatch(
          do.call(submit_fn, c(
            list(catalog = catalog),
            resolved,
            list(
              actor_resolver = actor_resolver,
              request = acquisition_dashboard_request(request, session)
            )
          )),
          error = identity
        )
        if (inherits(result, "error")) {
          output$`campaign-submit-result` <- shiny::renderText(conditionMessage(result))
        } else {
          output$`campaign-submit-result` <- shiny::renderText(
            paste0("Campaign ", result$campaign_id, " submitted by ", result$submitted_by %||% "authenticated admin", ".")
          )
        }
      },
      ignoreInit = TRUE
    )
    invisible(NULL)
  })
}
