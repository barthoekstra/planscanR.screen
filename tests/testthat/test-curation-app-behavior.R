# ACQ-C1f: headless behavior at the injected curation-app boundaries.
#
# These tests use synthetic records, a file-backed SQLite review repository,
# and an in-memory C3 artifact-store double.  No BIOGAIN runtime or corpus is
# required.

curation_app_behavior_skip <- function() {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools", "DBI", "RSQLite")) {
    skip_if_not_installed(package)
  }
}

curation_app_behavior_config <- function(
  project_id = "app-project",
  corpus_id = "app-corpus",
  config_id = "app.v1",
  selection_marker = NULL,
  selection_trace = NULL,
  selection_controls = NULL,
  selection_fn = NULL
) {
  labels <- c(keep = "keep", drop = "drop")
  attr(labels, "relevant") <- "keep"
  select_fn <- if (!is.null(selection_fn)) {
    selection_fn
  } else if (is.null(selection_marker)) {
    function(records, controls) {
      records$selected <- rep(TRUE, nrow(records))
      records
    }
  } else {
    function(records, controls) {
      if (!is.null(selection_trace)) selection_trace$controls <- controls
      records$selected <- rep(
        identical(controls$policy_marker, selection_marker),
        nrow(records)
      )
      records
    }
  }
  curation_config(
    project_id = project_id,
    corpus_id = corpus_id,
    config_id = config_id,
    topics = c(topic = "topic records"),
    labels = labels,
    lexicon = list(topic = "topic"),
    select_fn = select_fn,
    selection_controls = if (!is.null(selection_controls)) {
      selection_controls
    } else if (is.null(selection_marker)) {
      list()
    } else {
      list(policy_marker = selection_marker)
    },
    funnel_stages = list(
      indexed = list(
        key = "indexed",
        label = "Indexed",
        fn = function(records, context) rep(TRUE, nrow(records))
      )
    ),
    branding = list(title = "Curation app fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-app-artifacts-")),
    translator = NULL
  )
}

curation_app_behavior_records <- function() {
  tibble::tibble(
    country = c("nl", "de", "nl", "de", "nl", "de"),
    document_id = c("shared", "shared", "browse-2", "random-1", "random-2", "random-3"),
    title = rep("Topic record", 6L),
    summary = rep("A synthetic topic record for the curation app.", 6L),
    url = paste0("https://fixture.invalid/", seq_len(6L))
  )
}

curation_app_behavior_provider <- function(records = curation_app_behavior_records()) {
  curation_records_provider(
    read_fn = function(corpus_id, countries) {
      if (is.null(countries)) {
        return(records)
      }
      records[records$country %in% countries, , drop = FALSE]
    }
  )
}

curation_app_behavior_artifacts <- function(active_model = NULL) {
  state <- new.env(parent = emptyenv())
  state$calls <- list()
  state$snapshot <- NULL
  state$sample <- NULL
  state$sample_revision <- 0L

  record_call <- function(name, fields = list()) {
    state$calls <- c(state$calls, list(c(list(name = name), fields)))
  }

  empty_samples <- function() {
    data.frame(
      sample_id = character(0),
      reviewer_id = character(0),
      revision = integer(0),
      stringsAsFactors = FALSE
    )
  }

  store <- curation_artifact_store(
    paths = list(root = tempfile("curation-app-c3-")),
    snapshot_read_fn = function(scope, version, actor) {
      record_call("snapshot_read", list(actor_id = actor$id, version = version))
      state$snapshot
    },
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      record_call(
        "snapshot_write",
        list(actor_id = actor$id, expected_revision = expected_revision)
      )
      state$snapshot <- c(snapshot, list(revision = expected_revision + 1L))
      list(ok = TRUE, revision = expected_revision + 1L)
    },
    sample_list_fn = function(scope, reviewer_id, actor) {
      record_call(
        "sample_list",
        list(actor_id = actor$id, reviewer_id = reviewer_id)
      )
      if (is.null(state$sample)) {
        return(empty_samples())
      }
      data.frame(
        sample_id = as.character(state$sample$sample_id),
        reviewer_id = as.character(state$sample$reviewer_id),
        revision = as.integer(state$sample_revision),
        stringsAsFactors = FALSE
      )
    },
    sample_read_fn = function(scope, sample_id, actor) {
      record_call(
        "sample_read",
        list(actor_id = actor$id, sample_id = sample_id)
      )
      if (is.null(state$sample)) {
        return(NULL)
      }
      list(
        sample_id = state$sample$sample_id,
        reviewer_id = state$sample$reviewer_id,
        revision = state$sample_revision,
        payload = state$sample
      )
    },
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
      record_call(
        "sample_write",
        list(
          actor_id = actor$id,
          reviewer_id = reviewer_id,
          expected_revision = expected_revision,
          sample = sample
        )
      )
      if (!identical(as.integer(expected_revision), as.integer(state$sample_revision))) {
        stop("sample artifact revision conflict")
      }
      state$sample <- sample
      state$sample_revision <- as.integer(expected_revision) + 1L
      list(
        ok = TRUE,
        sample_id = sample$sample_id,
        revision = state$sample_revision
      )
    },
    model_list_fn = function(scope, actor) {
      record_call("model_list", list(actor_id = actor$id))
      data.frame(model_id = character(0), revision = integer(0), stringsAsFactors = FALSE)
    },
    model_read_fn = function(scope, model_id, actor) {
      record_call("model_read", list(actor_id = actor$id, model_id = model_id))
      NULL
    },
    model_write_fn = function(scope, model, provenance, actor, expected_revision) {
      record_call(
        "model_write",
        list(actor_id = actor$id, expected_revision = expected_revision)
      )
      list(ok = TRUE, revision = expected_revision + 1L)
    },
    model_publish_fn = function(scope, model_id, actor, expected_active_revision) {
      record_call(
        "model_publish",
        list(actor_id = actor$id, expected_active_revision = expected_active_revision)
      )
      list(ok = TRUE, active_revision = expected_active_revision + 1L)
    },
    active_model_fn = function(scope, actor) {
      record_call("active_model", list(actor_id = actor$id))
      active_model
    }
  )

  list(store = store, state = state)
}

curation_app_behavior_model <- function(config) {
  structure(
    list(
      topics = config$topics,
      labels = config$labels,
      threshold = 0.5,
      include = character(0),
      workflow = NULL,
      oof = tibble::tibble(
        document_id = c("shared", "shared"),
        country = c("nl", "de"),
        truth = c("keep", "drop"),
        .pred_keep = c(0.9, 0.1)
      )
    ),
    class = "planscanR_selection_model"
  )
}

curation_app_behavior_context <- function(
  actor_id,
  repository,
  artifacts,
  config = curation_app_behavior_config(),
  records = curation_app_behavior_records(),
  roles = "reviewer"
) {
  actor <- curation_actor(actor_id, roles = roles, display_name = actor_id)
  list(
    config = config,
    records_provider = curation_app_behavior_provider(records),
    review_repository = repository,
    artifact_store = artifacts$store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = function(session) list(session = session, actor_id = actor_id)
  )
}

curation_app_behavior_server <- function(context) {
  function(input, output, session) {
    planscanR.screen:::curation_app_server(input, output, session, context)
  }
}

curation_app_behavior_pulse <- function(session, input_id, value) {
  do.call(
    session$setInputs,
    stats::setNames(list(value), input_id)
  )
  session$flushReact()
  invisible(value)
}

curation_app_behavior_prime <- function(session, input_id, value) {
  # Shiny observers in curation_app_server use ignoreInit = TRUE.  Prime the
  # mock input once so the following pulse represents exactly one user event.
  curation_app_behavior_pulse(session, input_id, value)
}

curation_app_behavior_display_payload <- function(data, reviews, reviewer_id, table_id) {
  rows <- curation_app_review_display_rows(data, reviews, reviewer_id)
  list(
    table_id = table_id,
    render_id = curation_app_review_display_token(data, reviews, reviewer_id),
    rows = rows,
    nonce = 1L
  )
}

curation_app_behavior_cleanup_db <- function(path, repository) {
  repository$close()
  unlink(c(path, paste0(path, "-wal"), paste0(path, "-shm")), force = TRUE)
}

test_that("curation app leaves a stale same-actor session unchanged", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)

  config <- curation_app_behavior_config()
  actor <- curation_actor("alice", roles = "reviewer")
  original <- data.frame(
    country = "nl", document_id = "shared", reviewer_id = "alice",
    decision = "keep", source = "browse", note = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_identical(
    curation_review_upsert(repository, config$identity, original, actor, 0L)$revision,
    1L
  )
  artifacts <- curation_app_behavior_artifacts()
  context <- curation_app_behavior_context("alice", repository, artifacts)

  # Both sessions load revision 1 before either actor writes.  Running the
  # complete server against two mock sessions keeps the stale local frame
  # independent while sharing the real SQLite repository.
  session_a <- shiny::MockShinySession$new()
  session_b <- shiny::MockShinySession$new()
  on.exit(session_a$close(), add = TRUE)
  on.exit(session_b$close(), add = TRUE)
  server <- curation_app_behavior_server(context)
  shiny:::withMockContext(
    session_a,
    server(input = session_a$input, output = session_a$output, session = session_a)
  )
  shiny:::withMockContext(
    session_b,
    server(input = session_b$input, output = session_b$output, session = session_b)
  )
  loaded <- curation_review_read(
    repository,
    config$identity,
    curation_review_key("nl", "shared", "alice"),
    actor
  )
  expect_identical(loaded$revision, 1L)

  shiny:::withMockContext(session_a, {
    curation_app_behavior_prime(
      session_a,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session_a,
      "row_decision",
      list(
        id = "shared", country = "nl", decision = "drop", source = "browse", revision = 1L, nonce = 1L,
        reviewer_id = "spoofed-input"
      )
    )
  })
  changed <- curation_review_read(
    repository,
    config$identity,
    curation_review_key("nl", "shared", "alice"),
    actor
  )
  expect_identical(changed$decision, "drop")
  expect_identical(changed$revision, 2L)

  shiny:::withMockContext(session_b, {
    curation_app_behavior_prime(
      session_b,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session_b,
      "row_decision",
      list(
        id = "shared", country = "nl", decision = "keep", source = "browse", revision = 1L, nonce = 1L,
        reviewer_id = "spoofed-input"
      )
    )
  })

  after <- curation_review_read(
    repository,
    config$identity,
    curation_review_key("nl", "shared", "alice"),
    actor
  )
  expect_identical(after$decision, "drop")
  expect_identical(after$source, "browse")
  expect_identical(after$revision, 2L)
})

test_that("curation app keeps country/native IDs and actors isolated", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  artifacts <- curation_app_behavior_artifacts()

  alice <- curation_app_behavior_context("alice", repository, artifacts)
  shiny::testServer(curation_app_behavior_server(alice), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(
        id = "shared", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 1L,
        reviewer_id = "bob"
      )
    )
  })

  bob <- curation_app_behavior_context("bob", repository, artifacts)
  shiny::testServer(curation_app_behavior_server(bob), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "de", decision = "drop", source = "browse", revision = 0L, nonce = 3L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = "unsure", source = "browse", revision = 0L, nonce = 5L)
    )
  })

  actor <- curation_actor("alice", roles = "reviewer")
  rows <- curation_review_list(repository, alice$config$identity, actor = actor)
  rows$identity <- paste(rows$country, rows$document_id, rows$reviewer_id, sep = "::")
  expect_setequal(
    rows$identity,
    c("nl::shared::alice", "de::shared::bob", "nl::shared::bob")
  )
  expect_identical(
    rows$decision[rows$identity == "nl::shared::alice"],
    "keep"
  )
  expect_identical(
    rows$decision[rows$identity == "de::shared::bob"],
    "drop"
  )
  expect_identical(
    rows$decision[rows$identity == "nl::shared::bob"],
    "unsure"
  )
  expect_false(any(rows$reviewer_id == "spoofed-input"))
})

test_that("admins can edit another reviewer's row through an explicit target CAS path", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  config <- curation_app_behavior_config()
  target <- data.frame(
    country = "nl", document_id = "shared", reviewer_id = "bob",
    decision = "keep", source = "random", note = "sample provenance",
    sidecar_path = "/tmp/random-shared.json",
    stringsAsFactors = FALSE
  )
  bob <- curation_actor("bob", roles = "reviewer")
  expect_identical(curation_review_upsert(repository, config$identity, target, bob, 0L)$revision, 1L)
  artifacts <- curation_app_behavior_artifacts()
  admin_context <- curation_app_behavior_context(
    "alice", repository, artifacts, config,
    curation_app_behavior_records()[1:2, , drop = FALSE], roles = "admin"
  )

  shiny::testServer(curation_app_behavior_server(admin_context), {
    curation_app_behavior_prime(session, "admin_row_decision", list())
    curation_app_behavior_pulse(
      session,
      "admin_row_decision",
      list(
        country = "nl", document_id = "shared", reviewer = "bob",
        decision = "drop", revision = 1L, nonce = 1L
      )
    )
    changed <- curation_review_read(
      repository, config$identity,
      curation_review_key("nl", "shared", "bob"),
      curation_actor("alice", roles = "admin")
    )
    expect_identical(changed$decision, "drop")
    expect_identical(changed$source, "random")
    expect_identical(changed$note, "sample provenance")
    expect_identical(changed$sidecar_path, "/tmp/random-shared.json")
    expect_identical(changed$revision, 2L)

    # A queued click from the old row must be rejected and refresh the
    # canonical target state rather than overwriting the newer decision.
    curation_app_behavior_pulse(
      session,
      "admin_row_decision",
      list(
        country = "nl", document_id = "shared", reviewer = "bob",
        decision = "keep", revision = 1L, nonce = 2L
      )
    )
  })
  after_conflict <- curation_review_read(
    repository, config$identity,
    curation_review_key("nl", "shared", "bob"),
    curation_actor("alice", roles = "admin")
  )
  expect_identical(after_conflict$decision, "drop")
  expect_identical(after_conflict$revision, 2L)

  reviewer_context <- curation_app_behavior_context(
    "alice", repository, artifacts, config,
    curation_app_behavior_records()[1:2, , drop = FALSE], roles = "reviewer"
  )
  shiny::testServer(curation_app_behavior_server(reviewer_context), {
    curation_app_behavior_prime(session, "admin_row_decision", list())
    curation_app_behavior_pulse(
      session,
      "admin_row_decision",
      list(
        country = "nl", document_id = "shared", reviewer = "bob",
        decision = "keep", revision = 2L, nonce = 3L
      )
    )
  })
  reviewer_forged <- curation_review_read(
    repository, config$identity,
    curation_review_key("nl", "shared", "bob"),
    curation_actor("alice", roles = "reviewer")
  )
  expect_identical(reviewer_forged$decision, "drop")
  expect_identical(reviewer_forged$revision, 2L)

  roles <- "admin"
  revoked_context <- curation_app_behavior_context(
    "alice", repository, artifacts, config,
    curation_app_behavior_records()[1:2, , drop = FALSE], roles = roles
  )
  revoked_context$actor_resolver <- curation_actor_resolver(function(request) {
    curation_actor("alice", roles = roles)
  })
  shiny::testServer(curation_app_behavior_server(revoked_context), {
    curation_app_behavior_prime(session, "admin_row_decision", list())
    roles <<- "reviewer"
    curation_app_behavior_pulse(
      session,
      "admin_row_decision",
      list(
        country = "nl", document_id = "shared", reviewer = "bob",
        decision = "keep", revision = 2L, nonce = 4L
      )
    )
  })
  revoked <- curation_review_read(
    repository, config$identity,
    curation_review_key("nl", "shared", "bob"),
    curation_actor("alice", roles = "reviewer")
  )
  expect_identical(revoked$decision, "drop")
  expect_identical(revoked$revision, 2L)
})

test_that("curation app isolates config scopes, controls, and requests", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)

  trace_a <- new.env(parent = emptyenv())
  trace_b <- new.env(parent = emptyenv())
  config_a <- curation_app_behavior_config(
    project_id = "project-a",
    corpus_id = "corpus-a",
    config_id = "config.a",
    selection_marker = "alpha",
    selection_trace = trace_a
  )
  config_b <- curation_app_behavior_config(
    project_id = "project-b",
    corpus_id = "corpus-b",
    config_id = "config.b",
    selection_marker = "beta",
    selection_trace = trace_b
  )
  records_a <- curation_app_behavior_records()[1L, , drop = FALSE]
  records_a$document_id <- "config-a-record"
  records_b <- curation_app_behavior_records()[1:2, , drop = FALSE]
  records_b$document_id <- c("config-b-record-1", "config-b-record-2")

  context_a <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts(), config_a, records_a
  )
  context_b <- curation_app_behavior_context(
    "bob", repository, curation_app_behavior_artifacts(), config_b, records_b
  )

  shiny::testServer(curation_app_behavior_server(context_a), {
    session$setInputs(countries = "nl", stage_filter = "all")
    session$flushReact()
    expect_identical(output$vb_total, "1")
    expect_identical(output$vb_selected, "1 (100.0%)")
    expect_identical(trace_a$controls$policy_marker, "alpha")
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "config-a-record", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 1L)
    )
  })

  shiny::testServer(curation_app_behavior_server(context_b), {
    session$setInputs(countries = c("nl", "de"), stage_filter = "all")
    session$flushReact()
    expect_identical(output$vb_total, "2")
    expect_identical(output$vb_selected, "2 (100.0%)")
    expect_identical(trace_b$controls$policy_marker, "beta")
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "config-b-record-1", country = "nl", decision = "drop", source = "browse", revision = 0L, nonce = 1L)
    )
  })

  actor_a <- curation_actor("alice", roles = "reviewer")
  actor_b <- curation_actor("bob", roles = "reviewer")
  row_a <- curation_review_read(
    repository, config_a$identity,
    curation_review_key("nl", "config-a-record", "alice"), actor_a
  )
  row_b <- curation_review_read(
    repository, config_b$identity,
    curation_review_key("nl", "config-b-record-1", "bob"), actor_b
  )
  expect_identical(row_a$decision, "keep")
  expect_identical(row_a$config_id, "config.a")
  expect_identical(row_b$decision, "drop")
  expect_identical(row_b$config_id, "config.b")
  expect_identical(
    curation_review_list(repository, config_a$identity, actor = actor_a)$document_id,
    "config-a-record"
  )
  expect_identical(
    curation_review_list(repository, config_b$identity, actor = actor_b)$document_id,
    "config-b-record-1"
  )
})

test_that("curation app keeps persisted review columns aligned across writes", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  context <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts(),
    records = curation_app_behavior_records()[1:2, , drop = FALSE]
  )
  config <- context$config
  actor <- curation_actor("alice", roles = "reviewer")
  rows <- data.frame(
    country = c("nl", "de"), document_id = c("shared", "shared"),
    reviewer_id = c("alice", "alice"), decision = c("keep", "drop"),
    source = c("browse", "browse"), note = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_identical(
    curation_review_bulk_upsert(repository, config$identity, rows, actor, c(0L, 0L))$revision,
    c(1L, 1L)
  )

  shiny::testServer(curation_app_behavior_server(context), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = "unsure", source = "browse", revision = 1L, nonce = 1L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = NULL, source = "browse", revision = 2L, nonce = 2L)
    )
    session$setInputs(
      countries = c("nl", "de"), stage_filter = "all",
      review_tbl__reactable__selected = 2L
    )
    current_reviews <- curation_review_list(repository, config$identity, actor = actor)
    displayed <- curation_app_behavior_display_payload(
      curation_app_behavior_records()[1:2, , drop = FALSE],
      current_reviews,
      "alice",
      "review_tbl"
    )
    displayed$rows <- Filter(
      function(row) identical(as.character(row$country), "de") && identical(as.character(row$document_id), "shared"),
      displayed$rows
    )
    session$setInputs(review_bulk_display = displayed)
    session$flushReact()
    curation_app_behavior_pulse(session, "mark_keep", 1L)
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = "keep", source = "browse", revision = 3L, nonce = 3L)
    )
  })

  updated <- curation_review_read(
    repository, config$identity,
    curation_review_key("nl", "shared", "alice"), actor
  )
  other <- curation_review_read(
    repository, config$identity,
    curation_review_key("de", "shared", "alice"), actor
  )
  expect_identical(updated$decision, "keep")
  expect_identical(updated$revision, 4L)
  expect_identical(other$decision, "keep")
  expect_identical(other$source, "browse")
  expect_identical(other$revision, 2L)
})

test_that("displayed review revisions prevent stale single-row overwrites", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  context <- curation_app_behavior_context(
    "alice",
    repository,
    curation_app_behavior_artifacts(),
    records = curation_app_behavior_records()[1:2, , drop = FALSE]
  )
  actor <- curation_actor("alice", roles = "reviewer")
  key <- curation_review_key("nl", "shared", "alice")
  original <- data.frame(
    country = "nl", document_id = "shared", reviewer_id = "alice",
    decision = "keep", source = "browse", stringsAsFactors = FALSE
  )
  expect_identical(
    curation_review_upsert(repository, context$config$identity, original, actor, 0L)$revision,
    1L
  )

  shiny::testServer(curation_app_behavior_server(context), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(
        id = "prime", country = "nl", decision = "keep", source = "browse",
        revision = 0L, nonce = 0L
      )
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(
        id = "shared", country = "nl", decision = "drop", source = "browse",
        revision = 1L, nonce = 1L
      )
    )
    external <- data.frame(
      country = "nl", document_id = "shared", reviewer_id = "alice",
      decision = "unsure", source = "browse", stringsAsFactors = FALSE
    )
    expect_identical(
      curation_review_upsert(repository, context$config$identity, external, actor, 2L)$revision,
      3L
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(
        id = "shared", country = "nl", decision = "keep", source = "browse",
        revision = 2L, nonce = 2L
      )
    )
  })

  canonical <- curation_review_read(repository, context$config$identity, key, actor)
  expect_identical(canonical$decision, "unsure")
  expect_identical(canonical$revision, 3L)
})

test_that("browser mutations reject missing or stale rendered-row mappings", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  artifacts <- curation_app_behavior_artifacts()
  records <- curation_app_behavior_records()[1:2, , drop = FALSE]
  context <- curation_app_behavior_context("alice", repository, artifacts, records = records)
  actor <- curation_actor("alice", roles = "reviewer")
  original <- data.frame(
    country = "nl", document_id = "shared", reviewer_id = "alice",
    decision = "keep", source = "browse", stringsAsFactors = FALSE
  )
  expect_identical(curation_review_upsert(repository, context$config$identity, original, actor, 0L)$revision, 1L)

  shiny::testServer(curation_app_behavior_server(context), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = "drop", source = "browse", nonce = 1L)
    )
    current <- curation_review_list(repository, context$config$identity, actor = actor)
    missing_revision <- curation_app_behavior_display_payload(records, current, "alice", "review_tbl")
    missing_revision$rows <- list(list(country = "nl", document_id = "shared"))
    session$setInputs(review_bulk_display = missing_revision)
    session$flushReact()
    curation_app_behavior_pulse(session, "mark_keep", 1L)

    stale_render <- curation_app_behavior_display_payload(records, current, "alice", "review_tbl")
    stale_render$render_id <- "render-from-another-table"
    stale_render$rows <- Filter(
      function(row) identical(as.character(row$country), "nl") && identical(as.character(row$document_id), "shared"),
      stale_render$rows
    )
    session$setInputs(review_bulk_display = stale_render)
    session$flushReact()
    curation_app_behavior_pulse(session, "mark_keep", 2L)
  })

  unchanged <- curation_review_read(
    repository, context$config$identity,
    curation_review_key("nl", "shared", "alice"), actor
  )
  expect_identical(unchanged$decision, "keep")
  expect_identical(unchanged$revision, 1L)
})

test_that("curation app renders an empty provider without model columns errors", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  context <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts(),
    records = curation_app_empty_records()
  )

  shiny::testServer(curation_app_behavior_server(context), {
    session$setInputs(countries = character(0), stage_filter = "all")
    session$flushReact()
    expect_identical(output$vb_total, "0")
    expect_identical(output$vb_selected, "—")
  })
})

test_that("curation app treats a cleared trim-label input as an empty set", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  trace <- new.env(parent = emptyenv())
  config <- curation_app_behavior_config(
    selection_controls = list(use_trim = TRUE, nonrenewable = "keep"),
    selection_fn = function(records, controls) {
      trace$controls <- controls
      records$selected <- rep(length(controls$nonrenewable) == 0L, nrow(records))
      records
    }
  )
  context <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts(), config,
    curation_app_behavior_records()[1L, , drop = FALSE]
  )

  server <- function(input, output, session) {
    session$setInputs(
      countries = "nl", stage_filter = "all", use_trim = TRUE,
      nonrenewable = "keep"
    )
    planscanR.screen:::curation_app_server(input, output, session, context)
  }
  shiny::testServer(server, {
    expect_identical(output$vb_selected, "0 (0.0%)")
    session$setInputs(use_trim = FALSE)
    session$flushReact()
    expect_identical(trace$controls$nonrenewable, character(0))
    expect_identical(output$vb_selected, "1 (100.0%)")
    session$setInputs(use_trim = TRUE, nonrenewable = NULL)
    session$flushReact()
    expect_identical(trace$controls$nonrenewable, character(0))
  })
})

test_that("curation app constructs when the learner registry is empty", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  context <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts()
  )

  testthat::local_mocked_bindings(
    curation_selection_learner_choices = function() character(0),
    .package = "planscanR.screen"
  )
  app <- do.call(planscanR.screen::curation_app, context)
  expect_s3_class(app, "shiny.appobj")
})

test_that("curation app renders heuristic and model country performance", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  config <- curation_app_behavior_config()
  actor_ids <- c("alice", "bob")
  for (country in c("nl", "de")) {
    decision <- if (country == "nl") "keep" else "drop"
    for (reviewer_id in actor_ids) {
      curation_review_upsert(
        repository,
        config$identity,
        data.frame(
          country = country,
          document_id = "shared",
          reviewer_id = reviewer_id,
          decision = decision,
          source = "random",
          note = NA_character_,
          stringsAsFactors = FALSE
        ),
        curation_actor(reviewer_id, roles = "reviewer"),
        0L
      )
    }
  }
  model <- curation_app_behavior_model(config)
  artifacts <- curation_app_behavior_artifacts(active_model = model)
  context <- curation_app_behavior_context(
    "alice", repository, artifacts, config,
    curation_app_behavior_records()[1:2, , drop = FALSE]
  )

  shiny::testServer(curation_app_behavior_server(context), {
    session$setInputs(countries = c("nl", "de"), stage_filter = "all")
    session$flushReact()
    rendered <- jsonlite::fromJSON(output$perf_by_country, simplifyVector = FALSE)
    widget_data <- rendered$x$tag$attribs$data
    expect_setequal(
      unique(unlist(widget_data$method, use.names = FALSE)),
      c("Heuristic", "Model")
    )
    expect_setequal(
      unique(unlist(widget_data$country, use.names = FALSE)),
      c("all", "de", "nl")
    )
  })
})

test_that("curation app excludes unreviewed records from disagreement rows", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  config <- curation_app_behavior_config()
  for (reviewer_id in c("alice", "bob")) {
    curation_review_upsert(
      repository,
      config$identity,
      data.frame(
        country = "nl",
        document_id = "shared",
        reviewer_id = reviewer_id,
        decision = "drop",
        source = "browse",
        note = NA_character_,
        stringsAsFactors = FALSE
      ),
      curation_actor(reviewer_id, roles = "reviewer"),
      0L
    )
  }
  context <- curation_app_behavior_context(
    "alice", repository, curation_app_behavior_artifacts(), config,
    curation_app_behavior_records()[1:2, , drop = FALSE]
  )

  shiny::testServer(curation_app_behavior_server(context), {
    session$setInputs(countries = c("nl", "de"), stage_filter = "disagree")
    session$flushReact()
    expect_identical(output$review_info, "1 rows")
  })
})

test_that("curation app forwards browse, random bulk, stepper, and clear revisions", {
  curation_app_behavior_skip()
  db <- tempfile(fileext = ".sqlite")
  repository <- curation_review_repository_sqlite(db)
  on.exit(curation_app_behavior_cleanup_db(db, repository), add = TRUE)
  artifacts <- curation_app_behavior_artifacts()
  context <- curation_app_behavior_context("alice", repository, artifacts)
  config <- context$config
  actor <- curation_actor("alice", roles = "reviewer")

  shiny::testServer(curation_app_behavior_server(context), {
    curation_app_behavior_prime(
      session,
      "row_decision",
      list(id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L)
    )
    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 1L)
    )
    browse <- curation_review_read(
      repository,
      config$identity,
      curation_review_key("nl", "shared", "alice"),
      actor
    )
    expect_identical(browse$decision, "keep")
    expect_identical(browse$source, "browse")
    expect_identical(browse$revision, 1L)

    curation_app_behavior_pulse(
      session,
      "row_decision",
      list(id = "shared", country = "nl", decision = NULL, source = "browse", revision = 1L, nonce = 3L)
    )
    cleared <- curation_review_read(
      repository,
      config$identity,
      curation_review_key("nl", "shared", "alice"),
      actor
    )
    expect_true(isTRUE(cleared$deleted))
    expect_true(is.na(cleared$decision))
    expect_identical(cleared$revision, 2L)

    session$setInputs(
      countries = c("nl", "de"), rnd_n = 10, rnd_seed = 42, rnd_view = "table"
    )
    session$flushReact()
    curation_app_behavior_pulse(session, "rnd_draw", 1L)
    sample_writes <- Filter(
      function(call) identical(call$name, "sample_write"),
      artifacts$state$calls
    )
    expect_length(sample_writes, 1L)
    expect_identical(sample_writes[[1L]]$expected_revision, 0L)
    queue <- artifacts$state$sample$queue
    expect_true(nrow(queue) > 0L)
    action_indices <- which(
      queue$country == "nl" & queue$document_id == "shared"
    )
    expect_length(action_indices, 1L)
    action_index <- action_indices[[1L]]

    current_reviews <- curation_review_list(repository, config$identity, actor = actor)
    displayed <- curation_app_behavior_display_payload(queue, current_reviews, "alice", "random_tbl")
    displayed$rows <- Filter(
      function(row) identical(as.character(row$country), "nl") && identical(as.character(row$document_id), "shared"),
      displayed$rows
    )
    session$setInputs(
      random_tbl__reactable__selected = action_index,
      review_bulk_display = displayed
    )
    session$flushReact()
    curation_app_behavior_pulse(session, "rnd_keep", 1L)
    bulk_key <- curation_review_key("nl", "shared", "alice")
    bulk <- curation_review_read(repository, config$identity, bulk_key, actor)
    expect_identical(bulk$source, "random")
    expect_identical(bulk$revision, 3L)

    if (action_index > 1L) {
      for (step in seq_len(action_index - 1L)) {
        curation_app_behavior_pulse(session, "sr_next", step)
      }
    }
    curation_app_behavior_pulse(
      session,
      "sr_decision",
      list(
        country = "nl", document_id = "shared", reviewer = "alice",
        decision = "keep", revision = 3L, nonce = 1L
      )
    )
    stepper <- curation_review_read(repository, config$identity, bulk_key, actor)
    expect_identical(stepper$source, "random")
    expect_identical(stepper$revision, 4L)
    expect_identical(stepper$decision, "keep")
  })
})
