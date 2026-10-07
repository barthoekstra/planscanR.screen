# ACQ-2a: immutable campaign submissions and explainable queue priority.
#
# Campaign policy is deliberately caller supplied. This file records the
# policy and creates/reuses the durable source-discovery jobs from ACQ-1b; it
# does not know a preferred country list or a BIOGAIN-specific energy policy.

acquisition_campaign_abort <- function(
  message,
  class = "planscanR_error_acquisition_campaign"
) {
  acquisition_catalog_abort(message, class = class)
}

acquisition_campaign_unblob <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]])) {
    return(NULL)
  }
  hex <- as.character(value[[1L]])
  if (nchar(hex) %% 2L != 0L || !grepl("^[0-9a-fA-F]*$", hex)) {
    return(NULL)
  }
  bytes <- tryCatch(
    as.raw(strtoi(
      substring(hex, seq(1L, nchar(hex), by = 2L), seq(2L, nchar(hex), by = 2L)),
      base = 16L
    )),
    error = function(error) NULL
  )
  if (is.null(bytes)) {
    return(NULL)
  }
  tryCatch(unserialize(bytes), error = function(error) NULL)
}

acquisition_campaign_optional_character <- function(value, arg) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  acquisition_catalog_scalar_character(as.character(value[[1L]]), arg)
}

acquisition_campaign_probability <- function(value, arg = "probability") {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(value[[1L]]))
  if (length(value) != 1L || !is.finite(value) || value < 0 || value > 1) {
    acquisition_campaign_abort(paste0("{.arg ", arg, "} must be between 0 and 1."))
  }
  value
}

acquisition_campaign_confidence <- function(value) {
  acquisition_campaign_probability(value, "energy_confidence")
}

acquisition_campaign_column_value <- function(data, row, candidates, default = NULL) {
  name <- candidates[candidates %in% base::names(data)][1L]
  if (is.na(name) || is.null(name)) {
    return(default)
  }
  value <- data[[name]][[row]]
  if (is.null(value) || length(value) == 0L) {
    return(default)
  }
  if (length(value) == 1L && is.na(value)) {
    return(default)
  }
  value
}

acquisition_campaign_character_vector <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(character(0))
  }
  if (is.factor(value)) value <- as.character(value)
  value <- as.character(unlist(value, use.names = FALSE))
  value <- trimws(value)
  value[!is.na(value) & nzchar(value)]
}

acquisition_campaign_energy_tags <- function(value) {
  raw <- acquisition_campaign_character_vector(value)
  lowered <- tolower(raw)
  canonical <- character(0)
  if (any(lowered %in% c("wind", "wind energy", "wind_power", "wind power"))) {
    canonical <- c(canonical, "wind")
  }
  if (any(lowered %in% c("solar", "solar energy", "photovoltaic", "pv", "solar_power", "solar power"))) {
    canonical <- c(canonical, "solar")
  }
  if (any(lowered %in% c("mixed", "wind/solar", "solar/wind"))) {
    canonical <- c(canonical, "wind", "solar")
  }
  unique(canonical)
}

acquisition_campaign_energy_kind <- function(tags, manual = NULL) {
  manual <- acquisition_campaign_character_vector(manual)
  if (length(manual) > 0L) {
    manual_kind <- tolower(manual[[1L]])
    if (manual_kind %in% c("wind", "solar", "mixed", "unknown")) {
      if (manual_kind == "mixed") {
        return("mixed")
      }
      if (manual_kind == "unknown") {
        return("unknown")
      }
      return(manual_kind)
    }
    acquisition_campaign_abort(
      "{.arg manual_energy_tag} must be `wind`, `solar`, `mixed`, or `unknown`."
    )
  }
  tags <- unique(tags)
  if (all(c("wind", "solar") %in% tags)) {
    return("mixed")
  }
  if (identical(tags, "wind")) {
    return("wind")
  }
  if (identical(tags, "solar")) {
    return("solar")
  }
  "unknown"
}

acquisition_campaign_energy_for_match <- function(kind, tags) {
  if (identical(kind, "mixed")) {
    return(c("wind", "solar", "mixed"))
  }
  if (identical(kind, "unknown")) {
    return("unknown")
  }
  unique(c(kind, tags))
}

acquisition_campaign_normalise_rules <- function(priority_rules) {
  if (!is.list(priority_rules) || length(priority_rules) == 0L) {
    acquisition_campaign_abort("{.arg priority_rules} must be a non-empty list of ordered rules.")
  }
  out <- vector("list", length(priority_rules))
  ids <- character(length(priority_rules))
  for (i in seq_along(priority_rules)) {
    rule <- priority_rules[[i]]
    if (!is.list(rule)) {
      acquisition_campaign_abort("Each priority rule must be a list.")
    }
    rule_id <- rule$rule_id %||% rule$id %||% paste0("rule-", i)
    rule_id <- acquisition_catalog_scalar_character(as.character(rule_id), "priority_rules$rule_id")
    if (rule_id %in% ids) {
      acquisition_campaign_abort("Priority rule IDs must be unique.")
    }
    ids[[i]] <- rule_id
    countries <- rule$countries %||% rule$country %||% "*"
    countries <- tolower(acquisition_campaign_character_vector(countries))
    if (length(countries) == 0L) countries <- "*"
    energy <- rule$energy %||% rule$energies %||% rule$energy_tags %||% "*"
    energy <- tolower(acquisition_campaign_character_vector(energy))
    if (length(energy) == 0L) energy <- "*"
    allowed_energy <- c("wind", "solar", "mixed", "unknown", "*")
    if (any(!energy %in% allowed_energy)) {
      acquisition_campaign_abort(
        paste0("Priority rule `", rule_id, "` has an unsupported energy tag.")
      )
    }
    priority <- rule$priority %||% (length(priority_rules) - i + 1L)
    priority <- acquisition_catalog_scalar_integer(priority, "priority_rules$priority", lower = -1000000L, upper = 1000000L)
    explanation <- rule$explanation %||% rule$description %||% paste0("Matched priority rule ", rule_id, ".")
    explanation <- acquisition_catalog_scalar_character(as.character(explanation), "priority_rules$explanation")
    name <- rule$name %||% rule$label %||% rule_id
    name <- acquisition_catalog_scalar_character(as.character(name), "priority_rules$name")
    out[[i]] <- list(
      rule_id = rule_id,
      name = name,
      countries = unique(countries),
      energy = unique(energy),
      priority = priority,
      explanation = explanation,
      ordinal = i
    )
  }
  out
}

acquisition_campaign_rule_matches <- function(rule, country, energy_kind, energy_tags) {
  country_ok <- "*" %in% rule$countries || tolower(country) %in% rule$countries
  if (!country_ok) {
    return(FALSE)
  }
  if ("*" %in% rule$energy) {
    return(TRUE)
  }
  any(acquisition_campaign_energy_for_match(energy_kind, energy_tags) %in% rule$energy)
}

acquisition_campaign_match_rule <- function(rules, country, energy_kind, energy_tags) {
  index <- which(vapply(
    rules,
    acquisition_campaign_rule_matches,
    logical(1),
    country = country,
    energy_kind = energy_kind,
    energy_tags = energy_tags
  ))
  if (length(index) == 0L) {
    return(list(
      rule_id = "unmatched",
      name = "Unmatched",
      priority = 0L,
      explanation = paste0(
        "No supplied priority rule matched country `", country,
        "` and energy `", energy_kind, "; queued at fallback priority 0."
      ),
      ordinal = NA_integer_
    ))
  }
  rules[[index[[1L]]]]
}

acquisition_campaign_items_from_selection <- function(catalog, selection, threshold = NULL) {
  source_rows <- acquisition_campaign_source_rows(catalog, selection)
  source_ids <- source_rows$source_ref_id
  lapply(seq_len(nrow(selection)), function(i) {
    probability <- acquisition_campaign_column_value(selection, i, c("probability", "select_prob", "selection_probability", "score"))
    probability <- acquisition_campaign_probability(probability)
    manual_override <- acquisition_campaign_column_value(
      selection,
      i,
      c("manual_override", "selection_override", "decision_override")
    )
    override_text <- if (is.null(manual_override) || length(manual_override) == 0L || is.na(manual_override[[1L]])) {
      NA_character_
    } else {
      tolower(trimws(as.character(manual_override[[1L]])))
    }
    selected <- if (!is.na(override_text)) {
      if (override_text %in% c("keep", "selected", "include", "yes", "true")) TRUE else if (override_text %in% c("drop", "deselected", "exclude", "no", "false")) FALSE else acquisition_campaign_abort("Manual selection overrides must be keep/drop values.")
    } else if ("selected" %in% names(selection)) {
      isTRUE(selection$selected[[i]])
    } else if (!is.null(threshold) && !is.na(probability)) {
      probability >= threshold
    } else {
      TRUE
    }
    raw_tags <- acquisition_campaign_column_value(selection, i, c("energy_tags", "energy_tag", "energy"), character())
    manual <- acquisition_campaign_column_value(selection, i, c("manual_energy_tag", "energy_override"))
    tags <- acquisition_campaign_energy_tags(raw_tags)
    kind <- acquisition_campaign_energy_kind(tags, manual)
    if (kind == "wind") tags <- "wind"
    if (kind == "solar") tags <- "solar"
    if (kind == "mixed") tags <- c("wind", "solar")
    if (kind == "unknown") tags <- character(0)
    evidence <- acquisition_campaign_column_value(selection, i, c("energy_evidence", "energy_source"))
    title <- acquisition_campaign_column_value(selection, i, c("title", "project_title"))
    list(
      item_key = source_ids[[i]],
      source_ref_id = source_ids[[i]],
      country = as.character(source_rows$country[[i]]),
      portal = as.character(source_rows$portal[[i]]),
      native_project_id = as.character(source_rows$native_project_id[[i]]),
      title = if (is.null(title) || length(title) == 0L || is.na(title[[1L]])) NA_character_ else as.character(title[[1L]]),
      selected = selected,
      probability = probability,
      energy_tags = unique(tags),
      energy_kind = kind,
      energy_evidence = evidence,
      energy_confidence = acquisition_campaign_column_value(selection, i, c("energy_confidence", "confidence")),
      manual_energy_tag = if (is.null(manual) || length(manual) == 0L || is.na(manual[[1L]])) NA_character_ else as.character(manual[[1L]]),
      manual_override = override_text
    )
  })
}

acquisition_campaign_validate_items <- function(catalog, items) {
  countries <- DBI::dbGetQuery(
    catalog$conn,
    paste0(
      "SELECT source_ref_id, country FROM acquisition_sources WHERE source_ref_id IN (",
      paste(rep("?", length(items)), collapse = ","), ")"
    ),
    params = lapply(items, `[[`, "source_ref_id")
  )
  if (nrow(countries) != length(items)) {
    acquisition_campaign_abort("Every selected source_ref_id must already exist in the acquisition catalog.")
  }
  country_map <- setNames(as.character(countries$country), countries$source_ref_id)
  for (i in seq_along(items)) {
    items[[i]]$country <- unname(country_map[[items[[i]]$source_ref_id]])
    items[[i]]$probability <- acquisition_campaign_probability(items[[i]]$probability)
    items[[i]]$energy_confidence <- acquisition_campaign_confidence(items[[i]]$energy_confidence)
  }
  items
}

acquisition_campaign_insert_rules <- function(conn, campaign_id, revision, rules) {
  now <- acquisition_catalog_now()
  for (rule in rules) {
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_priority_rules (campaign_id, revision, rule_id, ordinal, name,",
        "countries_blob, energy_blob, priority, explanation, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        campaign_id,
        revision,
        rule$rule_id,
        rule$ordinal,
        rule$name,
        acquisition_catalog_blob(rule$countries),
        acquisition_catalog_blob(rule$energy),
        rule$priority,
        rule$explanation,
        now
      )
    )
  }
  invisible(TRUE)
}

acquisition_campaign_rules_from_rows <- function(rows) {
  if (nrow(rows) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(rows)), function(i) {
    list(
      rule_id = as.character(rows$rule_id[[i]]),
      name = as.character(rows$name[[i]]),
      countries = acquisition_campaign_unblob(rows$countries_blob[[i]]) %||% "*",
      energy = acquisition_campaign_unblob(rows$energy_blob[[i]]) %||% "*",
      priority = as.integer(rows$priority[[i]]),
      explanation = as.character(rows$explanation[[i]]),
      ordinal = as.integer(rows$ordinal[[i]])
    )
  })
}

acquisition_campaign_job_association <- function(conn, job_id, campaign_id, item_key, rule) {
  existing <- acquisition_catalog_one(
    conn,
    "SELECT campaign_id, state FROM acquisition_jobs WHERE job_id = ?",
    list(job_id)
  )
  if (is.null(existing)) {
    return(invisible(FALSE))
  }
  current <- existing$campaign_id[[1L]]
  if (!is.na(current) && nzchar(current)) {
    return(invisible(FALSE))
  }
  if (as.character(existing$state[[1L]]) %in% c("queued", "retry_wait")) {
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET campaign_id = ?, campaign_item_key = ?, priority = ?,",
        "priority_rule_id = ?, priority_explanation = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(
        campaign_id,
        item_key,
        rule$priority,
        rule$rule_id,
        rule$explanation,
        acquisition_catalog_now(),
        job_id
      )
    )
  } else {
    # Claiming a running job records campaign provenance while preserving the
    # priority that was used when its lease was granted.
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET campaign_id = ?, campaign_item_key = ?,",
        "priority_rule_id = ?, priority_explanation = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(
        campaign_id,
        item_key,
        rule$rule_id,
        rule$explanation,
        acquisition_catalog_now(),
        job_id
      )
    )
  }
  invisible(TRUE)
}

#' Submit an immutable acquisition campaign selection.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param selection Data frame with `source_ref_id` (or `country`, `portal`,
#'   and `native_project_id`); optional `selected`, probability, energy
#'   evidence, confidence, and manual tag columns.
#' @param model_id Registered immutable model reference.
#' @param input_snapshot_id Immutable input snapshot identity.
#' @param threshold Selection probability threshold recorded as evidence.
#' @param policy_version Caller-supplied policy version.
#' @param priority_rules Ordered caller-supplied country/energy rules.
#' @param submitted_by Optional trusted actor identity.
#' @param campaign_id Optional stable campaign ID. A deterministic ID is used
#'   when omitted, so replaying the same submission is idempotent.
#' @param metadata Optional additional immutable submission metadata.
#' @return A list with campaign ID, selection ID, status, and item outcomes.
#' @export
acquisition_campaign_submit <- function(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  submitted_by = NULL,
  campaign_id = NULL,
  metadata = NULL
) {
  acquisition_catalog_connection(catalog)
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  input_snapshot_id <- acquisition_catalog_scalar_character(input_snapshot_id, "input_snapshot_id")
  policy_version <- acquisition_catalog_scalar_character(policy_version, "policy_version")
  threshold <- suppressWarnings(as.numeric(threshold))
  if (length(threshold) != 1L || is.na(threshold) || !is.finite(threshold) || threshold < 0 || threshold > 1) {
    acquisition_campaign_abort("{.arg threshold} must be between 0 and 1.")
  }
  submitted_by <- acquisition_campaign_optional_character(submitted_by, "submitted_by")
  rules <- acquisition_campaign_normalise_rules(priority_rules)
  items <- acquisition_campaign_validate_items(catalog, acquisition_campaign_items_from_selection(catalog, selection, threshold = threshold))
  # The row order is not an identity. Stable ordering lets a replay with the
  # same selected source set reuse the immutable campaign reference.
  items <- items[order(vapply(items, `[[`, character(1), "item_key"))]
  payload <- list(
    model_id = model_id,
    input_snapshot_id = input_snapshot_id,
    threshold = threshold,
    policy_version = policy_version,
    rules = rules,
    items = lapply(items, function(item) {
      item$country <- NULL
      item
    }),
    submitted_by = submitted_by,
    metadata = metadata
  )
  campaign_id <- campaign_id %||% paste0("campaign-", acquisition_catalog_digest(payload))
  selection_id <- paste0("selection-", acquisition_catalog_digest(campaign_id))
  selection_metadata <- list(
    threshold = threshold,
    policy_version = policy_version,
    priority_rules = rules,
    items = items,
    metadata = metadata
  )
  registered_selection <- acquisition_catalog_register_selection(
    catalog,
    selection_id = selection_id,
    input_snapshot_id = input_snapshot_id,
    model_id = model_id,
    metadata = selection_metadata,
    submitted_by = submitted_by
  )
  payload_fingerprint <- acquisition_catalog_digest(c(payload, list(campaign_id = campaign_id)))
  result <- acquisition_catalog_transaction(catalog, function(conn) {
    existing <- acquisition_catalog_one(conn, "SELECT * FROM acquisition_campaigns WHERE campaign_id = ?", list(campaign_id))
    if (!is.null(existing)) {
      if (!identical(as.character(existing$payload_fingerprint[[1L]]), payload_fingerprint)) {
        acquisition_campaign_abort(
          "An immutable campaign submission cannot be replaced.",
          class = "planscanR_error_acquisition_immutable"
        )
      }
      replay_items <- acquisition_campaign_items_from_db(conn, campaign_id)
      replay_items$work_status[replay_items$selected == 1L] <- "reused"
      return(list(
        campaign_id = campaign_id,
        selection_id = selection_id,
        status = "replayed",
        items = replay_items
      ))
    }
    model <- acquisition_catalog_one(conn, "SELECT model_id FROM acquisition_models WHERE model_id = ?", list(model_id))
    if (is.null(model)) acquisition_campaign_abort("The campaign model must be registered before submission.")
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_campaigns (campaign_id, selection_id, model_id, input_snapshot_id, threshold,",
        "policy_version, submitted_by, submitted_at, payload_fingerprint, priority_revision, metadata_blob)",
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 0, ?)"
      ),
      params = list(
        campaign_id,
        selection_id,
        model_id,
        input_snapshot_id,
        threshold,
        policy_version,
        submitted_by,
        acquisition_catalog_now(),
        payload_fingerprint,
        if (is.null(metadata)) NA_character_ else acquisition_catalog_blob(metadata)
      )
    )
    acquisition_campaign_insert_rules(conn, campaign_id, 0L, rules)
    item_rows <- vector("list", length(items))
    for (i in seq_along(items)) {
      item <- items[[i]]
      match <- acquisition_campaign_match_rule(rules, item$country, item$energy_kind, item$energy_tags)
      job <- NULL
      work_status <- "not_selected"
      if (isTRUE(item$selected)) {
        job <- acquisition_queue_enqueue_connection(
          conn,
          kind = "discover",
          target_type = "source",
          target_ref_id = item$source_ref_id,
          priority = match$priority
        )
        work_status <- if (identical(job$status, "inserted")) "enqueued" else "reused"
        acquisition_campaign_job_association(conn, job$job_id, campaign_id, item$item_key, match)
      }
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO acquisition_campaign_items (campaign_id, item_key, source_ref_id, selected, probability,",
          "energy_kind, energy_tags_blob, energy_evidence_blob, energy_confidence, manual_energy_tag, manual_override, priority,",
          "priority_rule_id, priority_explanation, priority_revision, job_id, work_status, created_at, updated_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0, ?, ?, ?, ?)"
        ),
        params = list(
          campaign_id,
          item$item_key,
          item$source_ref_id,
          as.integer(item$selected),
          item$probability,
          item$energy_kind,
          acquisition_catalog_blob(item$energy_tags),
          if (is.null(item$energy_evidence)) NA_character_ else acquisition_catalog_blob(item$energy_evidence),
          item$energy_confidence,
          item$manual_energy_tag,
          item$manual_override,
          match$priority,
          match$rule_id,
          match$explanation,
          if (is.null(job)) NA_character_ else job$job_id,
          work_status,
          acquisition_catalog_now(),
          acquisition_catalog_now()
        )
      )
      item_rows[[i]] <- list(
        item_key = item$item_key,
        source_ref_id = item$source_ref_id,
        job_id = if (is.null(job)) NA_character_ else job$job_id,
        work_status = work_status,
        priority = match$priority,
        priority_rule_id = match$rule_id,
        priority_explanation = match$explanation
      )
    }
    list(campaign_id = campaign_id, selection_id = selection_id, status = "inserted", items = item_rows)
  })
  result$selection_status <- registered_selection$status
  result
}

acquisition_campaign_items_from_db <- function(conn, campaign_id) {
  DBI::dbGetQuery(
    conn,
    paste(
      "SELECT i.*, j.state AS queue_state, j.priority AS queue_priority, j.campaign_id AS job_campaign_id",
      "FROM acquisition_campaign_items i LEFT JOIN acquisition_jobs j ON j.job_id = i.job_id",
      "WHERE i.campaign_id = ? ORDER BY i.item_key"
    ),
    params = list(campaign_id)
  )
}

#' List submitted acquisition campaigns.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Optional campaign filter.
#' @return A data frame of immutable campaign rows.
#' @export
acquisition_campaigns <- function(catalog, campaign_id = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  if (is.null(campaign_id)) {
    return(DBI::dbGetQuery(conn, "SELECT * FROM acquisition_campaigns ORDER BY submitted_at, campaign_id"))
  }
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  DBI::dbGetQuery(conn, "SELECT * FROM acquisition_campaigns WHERE campaign_id = ?", params = list(campaign_id))
}

#' List campaign items and their queue state.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @return A data frame with source, energy, rule, and queue provenance.
#' @export
acquisition_campaign_items <- function(catalog, campaign_id) {
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  acquisition_campaign_items_from_db(acquisition_catalog_connection(catalog), campaign_id)
}

#' List all priority-rule revisions for a campaign.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @return A data frame of ordered rule revisions.
#' @export
acquisition_campaign_priority_history <- function(catalog, campaign_id) {
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  DBI::dbGetQuery(
    acquisition_catalog_connection(catalog),
    "SELECT * FROM acquisition_priority_rules WHERE campaign_id = ? ORDER BY revision, ordinal",
    params = list(campaign_id)
  )
}

#' List recorded reprioritization operations.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @return A data frame of reprioritization events.
#' @export
acquisition_campaign_reprioritizations <- function(catalog, campaign_id) {
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  DBI::dbGetQuery(
    acquisition_catalog_connection(catalog),
    "SELECT * FROM acquisition_campaign_reprioritizations WHERE campaign_id = ? ORDER BY revision",
    params = list(campaign_id)
  )
}

#' Reprioritize pending campaign jobs without changing active leases.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @param priority_rules New ordered caller-supplied rules.
#' @param changed_by Optional trusted actor identity.
#' @return A list with revision and counts of updated and active jobs.
#' @export
acquisition_campaign_reprioritize <- function(catalog, campaign_id, priority_rules, changed_by = NULL) {
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  changed_by <- acquisition_campaign_optional_character(changed_by, "changed_by")
  rules <- acquisition_campaign_normalise_rules(priority_rules)
  acquisition_catalog_transaction(catalog, function(conn) {
    campaign <- acquisition_catalog_one(conn, "SELECT * FROM acquisition_campaigns WHERE campaign_id = ?", list(campaign_id))
    if (is.null(campaign)) acquisition_campaign_abort("The campaign does not exist.")
    revision <- as.integer(campaign$priority_revision[[1L]]) + 1L
    payload_fingerprint <- acquisition_catalog_digest(list(campaign_id = campaign_id, revision = revision, rules = rules, changed_by = changed_by))
    existing_event <- acquisition_catalog_one(
      conn,
      "SELECT revision FROM acquisition_campaign_reprioritizations WHERE payload_fingerprint = ?",
      list(payload_fingerprint)
    )
    if (!is.null(existing_event)) {
      return(list(
        campaign_id = campaign_id,
        revision = as.integer(existing_event$revision[[1L]]),
        status = "replayed",
        updated = 0L,
        active = 0L
      ))
    }
    items <- DBI::dbGetQuery(
      conn,
      paste(
        "SELECT i.*, s.country FROM acquisition_campaign_items i",
        "JOIN acquisition_sources s ON s.source_ref_id = i.source_ref_id",
        "WHERE i.campaign_id = ? ORDER BY i.item_key"
      ),
      params = list(campaign_id)
    )
    updated <- 0L
    active <- 0L
    for (i in seq_len(nrow(items))) {
      tags <- acquisition_campaign_unblob(items$energy_tags_blob[[i]]) %||% character(0)
      match <- acquisition_campaign_match_rule(rules, items$country[[i]], items$energy_kind[[i]], tags)
      job_id <- items$job_id[[i]]
      jobs <- DBI::dbGetQuery(
        conn,
        paste(
          "SELECT job_id, state FROM acquisition_jobs",
          "WHERE campaign_id = ? AND campaign_item_key = ? ORDER BY job_id"
        ),
        params = list(campaign_id, items$item_key[[i]])
      )
      if (nrow(jobs) == 0L && !is.na(job_id)) {
        # Keep discovery jobs submitted before campaign-item propagation
        # visible to the revision audit.
        jobs <- DBI::dbGetQuery(
          conn,
          "SELECT job_id, state FROM acquisition_jobs WHERE campaign_id = ? AND job_id = ?",
          params = list(campaign_id, job_id)
        )
      }
      for (j in seq_len(nrow(jobs))) {
        state <- as.character(jobs$state[[j]])
        if (identical(state, "running")) active <- active + 1L
        if (state %in% c("queued", "retry_wait")) {
          changed <- DBI::dbExecute(
            conn,
            paste(
              "UPDATE acquisition_jobs SET priority = ?, priority_rule_id = ?, priority_explanation = ?,",
              "updated_at = ? WHERE job_id = ? AND campaign_id = ? AND state IN ('queued', 'retry_wait')"
            ),
            params = list(
              match$priority,
              match$rule_id,
              match$explanation,
              acquisition_catalog_now(),
              jobs$job_id[[j]],
              campaign_id
            )
          )
          updated <- updated + as.integer(changed)
        }
      }
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_campaign_items SET priority = ?, priority_rule_id = ?, priority_explanation = ?,",
          "priority_revision = ?, updated_at = ? WHERE campaign_id = ? AND item_key = ?"
        ),
        params = list(match$priority, match$rule_id, match$explanation, revision, acquisition_catalog_now(), campaign_id, items$item_key[[i]])
      )
    }
    acquisition_campaign_insert_rules(conn, campaign_id, revision, rules)
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_campaign_reprioritizations (reprioritization_id, campaign_id, revision, rules_blob,",
        "changed_by, changed_at, payload_fingerprint) VALUES (?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        paste0("reprioritize-", payload_fingerprint),
        campaign_id,
        revision,
        acquisition_catalog_blob(rules),
        changed_by,
        acquisition_catalog_now(),
        payload_fingerprint
      )
    )
    DBI::dbExecute(
      conn,
      "UPDATE acquisition_campaigns SET priority_revision = ? WHERE campaign_id = ?",
      params = list(revision, campaign_id)
    )
    list(campaign_id = campaign_id, revision = revision, status = "updated", updated = updated, active = active)
  })
}

#' Explain campaign priority and current queue state.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @return A data frame with energy evidence, matched rule, and queue state.
#' @export
acquisition_campaign_explain <- function(catalog, campaign_id) {
  campaign_id <- acquisition_catalog_scalar_character(campaign_id, "campaign_id")
  rows <- acquisition_campaign_items(catalog, campaign_id)
  if (nrow(rows) == 0L) {
    return(rows)
  }
  rows$energy_tags <- lapply(rows$energy_tags_blob, acquisition_campaign_unblob)
  rows$energy_evidence <- lapply(rows$energy_evidence_blob, acquisition_campaign_unblob)
  rows$active_priority <- rows$queue_priority
  rows$priority_changed <- rows$priority_revision > 0L
  rows
}

#' Alias for `acquisition_campaign_explain()`.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Campaign ID.
#' @return A data frame explaining campaign priorities.
#' @export
acquisition_campaign_priority_explain <- function(catalog, campaign_id) {
  acquisition_campaign_explain(catalog, campaign_id)
}
