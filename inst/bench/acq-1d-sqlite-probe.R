#!/usr/bin/env Rscript

# Disposable ACQ-1d evidence probe. The parent starts this file again as
# independent Rscript workers; no package store or production cache is used.

argv <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  index <- match(name, argv)
  if (is.na(index) || index == length(argv)) default else argv[[index + 1L]]
}
mode <- arg("--mode", "run")
if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE)) {
  stop("DBI and RSQLite are required", call. = FALSE)
}

now_ms <- function() as.numeric(Sys.time()) * 1000
is_busy <- function(error) {
  grepl("locked|busy", conditionMessage(error), ignore.case = TRUE)
}
new_token <- function(worker, attempt) {
  paste(worker, Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS6"),
    attempt, sample.int(1e9, 1L), sep = "-")
}

open_db <- function(path, setup = FALSE) {
  connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  DBI::dbExecute(connection, "PRAGMA foreign_keys = ON")
  if (setup) {
    DBI::dbExecute(connection, "PRAGMA journal_mode = WAL")
    DBI::dbExecute(connection, "PRAGMA synchronous = NORMAL")
  }
  DBI::dbExecute(connection, "PRAGMA busy_timeout = 5000")
  connection
}

close_db <- function(connection) {
  if (DBI::dbIsValid(connection)) try(DBI::dbDisconnect(connection), silent = TRUE)
}

init_db <- function(path, jobs = 0L) {
  connection <- open_db(path, setup = TRUE)
  on.exit(close_db(connection), add = TRUE)
  DBI::dbExecute(connection, paste(
    "CREATE TABLE jobs (job_id INTEGER PRIMARY KEY,",
    "state TEXT NOT NULL CHECK(state IN ('queued','running','succeeded')),",
    "attempt INTEGER NOT NULL DEFAULT 0, lease_token TEXT, lease_until REAL,",
    "claimed_by TEXT, completed_by TEXT, completed_token TEXT)"))
  DBI::dbExecute(connection, paste(
    "CREATE TABLE attempts (job_id INTEGER REFERENCES jobs, attempt INTEGER,",
    "worker TEXT NOT NULL, lease_token TEXT NOT NULL UNIQUE, claimed_at REAL,",
    "PRIMARY KEY(job_id, attempt))"))
  DBI::dbExecute(connection, "CREATE TABLE crash_markers (worker TEXT PRIMARY KEY, marker TEXT)")
  for (job_id in seq_len(jobs)) DBI::dbExecute(
    connection, "INSERT INTO jobs(job_id, state) VALUES (?, 'queued')",
    params = list(as.integer(job_id)))
}

wait_for <- function(test, seconds = 60) {
  deadline <- Sys.time() + seconds
  repeat {
    if (isTRUE(test())) return(invisible(TRUE))
    if (Sys.time() > deadline) stop("probe wait timed out", call. = FALSE)
    Sys.sleep(0.02)
  }
}

# One short BEGIN IMMEDIATE transaction is the claim protocol under test.
claim <- function(connection, worker, expired = FALSE, lease_seconds = 30) {
  started <- now_ms()
  for (busy_retries in 0:20) {
    began <- FALSE
    result <- tryCatch({
      DBI::dbExecute(connection, "BEGIN IMMEDIATE")
      began <- TRUE
      Sys.sleep(0.008)
      if (expired) {
        candidate <- DBI::dbGetQuery(connection, paste(
          "SELECT job_id, attempt FROM jobs WHERE state = 'queued' OR",
          "(state = 'running' AND lease_until < ?) ORDER BY job_id LIMIT 1"),
          params = list(as.numeric(Sys.time())))
      } else {
        candidate <- DBI::dbGetQuery(connection,
          "SELECT job_id, attempt FROM jobs WHERE state = 'queued' ORDER BY job_id LIMIT 1")
      }
      if (!nrow(candidate)) {
        DBI::dbRollback(connection)
        began <- FALSE
        return(NULL)
      }
      job_id <- as.integer(candidate$job_id[[1L]])
      attempt <- as.integer(candidate$attempt[[1L]]) + 1L
      lease_token <- new_token(worker, attempt)
      DBI::dbExecute(connection, paste(
        "UPDATE jobs SET state='running', attempt=?, lease_token=?,",
        "lease_until=?, claimed_by=?, completed_by=NULL, completed_token=NULL",
        "WHERE job_id=?"), params = list(attempt, lease_token,
          as.numeric(Sys.time()) + lease_seconds, worker, job_id))
      DBI::dbExecute(connection, paste(
        "INSERT INTO attempts(job_id, attempt, worker, lease_token, claimed_at)",
        "VALUES (?, ?, ?, ?, ?)"), params = list(job_id, attempt, worker,
          lease_token, as.numeric(Sys.time())))
      DBI::dbCommit(connection)
      began <- FALSE
      list(job_id = job_id, attempt = attempt, token = lease_token,
        busy_retries = busy_retries, claim_ms = now_ms() - started)
    }, error = function(error) {
      if (began) try(DBI::dbRollback(connection), silent = TRUE)
      error
    })
    if (!inherits(result, "error")) return(result)
    if (!is_busy(result)) stop(result)
    Sys.sleep(min(0.02, 0.001 * 2^min(busy_retries, 5L)))
  }
  stop("SQLite remained busy after bounded retries", call. = FALSE)
}

complete <- function(connection, job, worker) {
  for (busy_retries in 0:20) {
    result <- tryCatch(DBI::dbExecute(connection, paste(
      "UPDATE jobs SET state='succeeded', completed_by=?, completed_token=?",
      "WHERE job_id=? AND state='running' AND lease_token=?"),
      params = list(worker, job$token, job$job_id, job$token)), error = identity)
    if (!inherits(result, "error")) return(as.integer(result))
    if (!is_busy(result)) stop(result)
    Sys.sleep(min(0.02, 0.001 * 2^min(busy_retries, 5L)))
  }
  stop("SQLite remained busy completing a claim", call. = FALSE)
}

worker <- function() {
  connection <- open_db(arg("--db"))
  on.exit(close_db(connection), add = TRUE)
  worker_id <- arg("--worker")
  writeLines(as.character(Sys.getpid()), arg("--ready"))
  wait_for(function() file.exists(arg("--release")))
  observations <- data.frame(worker = character(), job_id = integer(),
    attempt = integer(), claim_ms = numeric(), busy_retries = integer())
  repeat {
    job <- claim(connection, worker_id)
    if (is.null(job)) break
    Sys.sleep(0.003)
    if (complete(connection, job, worker_id) != 1L) stop("claim completion was fenced")
    observations <- rbind(observations, data.frame(
      worker = worker_id, job_id = job$job_id, attempt = job$attempt,
      claim_ms = job$claim_ms, busy_retries = job$busy_retries))
  }
  write.table(observations, arg("--metrics"), sep = "\t", row.names = FALSE, quote = FALSE)
  writeLines("done", arg("--done"))
}

crash_worker <- function() {
  connection <- open_db(arg("--db"))
  on.exit(close_db(connection), add = TRUE)
  job <- claim(connection, "crash-worker", lease_seconds = 0.4)
  writeLines(job$token, arg("--token"))
  DBI::dbExecute(connection, "BEGIN IMMEDIATE")
  DBI::dbExecute(
    connection,
    "INSERT INTO crash_markers VALUES (?, 'uncommitted')",
    params = list("crash-worker")
  )
  writeLines(as.character(Sys.getpid()), arg("--ready"))
  repeat Sys.sleep(1)
}

launch <- function(script, child_mode, params, stdout, stderr) {
  child_args <- unlist(Map(c, names(params), lapply(params, shQuote)))
  system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), "--mode", child_mode, child_args),
    stdout = stdout,
    stderr = stderr,
    wait = FALSE
  )
}

kill_ready <- function(path) {
  for (file in list.files(path, full.names = TRUE)) {
    pid <- suppressWarnings(as.integer(readLines(file, n = 1L)))
    if (!is.na(pid)) try(tools::pskill(pid, signal = 9L), silent = TRUE)
  }
}

scenario <- function(script, root, workers, jobs = 24L) {
  directory <- file.path(root, paste0("workers-", workers))
  dir.create(directory, recursive = TRUE)
  database <- file.path(directory, "probe.sqlite")
  init_db(database, jobs)
  ready <- file.path(directory, "ready"); done <- file.path(directory, "done")
  errors <- file.path(directory, "errors"); metrics <- file.path(directory, "metrics")
  for (path in c(ready, done, errors, metrics)) dir.create(path)
  release <- file.path(directory, "release")
  for (index in seq_len(workers)) {
    worker_id <- paste0("w", index)
    launch(script, "worker", c(
      "--db" = database, "--worker" = worker_id,
      "--ready" = file.path(ready, worker_id), "--release" = release,
      "--done" = file.path(done, worker_id), "--error" = file.path(errors, worker_id),
      "--metrics" = file.path(metrics, paste0(worker_id, ".tsv"))),
      file.path(directory, paste0(worker_id, ".out")),
      file.path(directory, paste0(worker_id, ".err")))
  }
  on.exit(kill_ready(ready), add = TRUE)
  wait_for(function() length(list.files(ready)) == workers || length(list.files(errors)))
  if (length(list.files(errors))) stop("contention worker failed before release")
  started <- now_ms()
  writeLines("go", release)
  wait_for(function() length(list.files(done)) == workers || length(list.files(errors)))
  if (length(list.files(errors))) stop("contention worker failed; inspect .err")
  observations <- do.call(rbind, lapply(list.files(metrics, full.names = TRUE),
    read.delim, stringsAsFactors = FALSE))
  connection <- open_db(database)
  on.exit(close_db(connection), add = TRUE)
  jobs_after <- DBI::dbGetQuery(connection, "SELECT job_id, state, attempt FROM jobs")
  attempts <- DBI::dbGetQuery(connection, "SELECT job_id, lease_token FROM attempts")
  counts <- table(attempts$job_id)
  stopifnot(
    nrow(jobs_after) == jobs,
    nrow(attempts) == jobs,
    all(jobs_after$state == "succeeded"),
    all(jobs_after$attempt == 1L),
    length(unique(attempts$lease_token)) == jobs,
    sum(as.integer(counts) > 1L) == 0L
  )
  data.frame(workers = workers, jobs = jobs, elapsed_ms = now_ms() - started,
    claims = nrow(observations), completed = sum(jobs_after$state == "succeeded"),
    unique_tokens = length(unique(attempts$lease_token)),
    duplicate_claims = sum(as.integer(counts) > 1L), max_attempt = max(jobs_after$attempt),
    claim_p50_ms = unname(stats::quantile(observations$claim_ms, 0.50)),
    claim_p95_ms = unname(stats::quantile(observations$claim_ms, 0.95)),
    claim_max_ms = max(observations$claim_ms), busy_retries = sum(observations$busy_retries))
}

recovery <- function(script, root) {
  directory <- file.path(root, "recovery")
  dir.create(directory, recursive = TRUE)
  database <- file.path(directory, "probe.sqlite")
  init_db(database)
  connection <- open_db(database)
  DBI::dbExecute(
    connection,
    "INSERT INTO jobs VALUES (9001, 'queued', 0, NULL, NULL, NULL, NULL, NULL)"
  )
  close_db(connection)
  ready <- file.path(directory, "ready")
  token_file <- file.path(directory, "token")
  launch(
    script,
    "crash",
    c("--db" = database, "--ready" = ready, "--token" = token_file),
    file.path(directory, "worker.out"),
    file.path(directory, "worker.err")
  )
  victim_pid <- NA_integer_
  on.exit(
    if (!is.na(victim_pid)) try(tools::pskill(victim_pid, signal = 9L), silent = TRUE),
    add = TRUE
  )
  wait_for(function() file.exists(ready))
  old_token <- readLines(token_file, n = 1L)
  victim_pid <- as.integer(readLines(ready, n = 1L))
  tools::pskill(victim_pid, signal = 9L)
  victim_pid <- NA_integer_
  Sys.sleep(0.2)
  connection <- open_db(database)
  on.exit(close_db(connection), add = TRUE)
  marker_rows <- DBI::dbGetQuery(
    connection,
    "SELECT count(*) AS n FROM crash_markers"
  )$n[[1L]]
  lease_until <- DBI::dbGetQuery(
    connection,
    "SELECT lease_until FROM jobs WHERE job_id = 9001"
  )$lease_until[[1L]]
  wait_for(function() as.numeric(Sys.time()) > lease_until, seconds = 5)
  fresh <- claim(connection, "reclaimer", expired = TRUE)
  stale_rows <- DBI::dbExecute(
    connection,
    paste(
      "UPDATE jobs SET state='succeeded', completed_token=?",
      "WHERE job_id=9001 AND state='running' AND lease_token=?"
    ),
    params = list(old_token, old_token)
  )
  fresh_rows <- DBI::dbExecute(
    connection,
    paste(
      "UPDATE jobs SET state='succeeded', completed_token=?",
      "WHERE job_id=9001 AND state='running' AND lease_token=?"
    ),
    params = list(fresh$token, fresh$token)
  )
  final <- DBI::dbGetQuery(
    connection,
    "SELECT state, attempt FROM jobs WHERE job_id=9001"
  )
  tokens <- DBI::dbGetQuery(
    connection,
    "SELECT lease_token FROM attempts WHERE job_id=9001"
  )$lease_token
  stopifnot(
    marker_rows == 0L,
    fresh$attempt == 2L,
    stale_rows == 0L,
    fresh_rows == 1L,
    final$state[[1L]] == "succeeded",
    final$attempt[[1L]] == 2L,
    length(unique(tokens)) == 2L
  )
  data.frame(
    marker_rows_after_kill = marker_rows,
    reopened = 1L,
    reclaimed = as.integer(fresh$attempt == 2L),
    stale_completion_rows = stale_rows,
    fresh_completion_rows = fresh_rows,
    final_state = final$state,
    attempts = final$attempt,
    unique_tokens = length(unique(tokens))
  )
}

run_probe <- function() {
  output <- arg(
    "--output",
    file.path(getwd(), "inst/bench/acq-1d-sqlite-evidence.tsv")
  )
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  script <- normalizePath(sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(), value = TRUE)[[1L]]
  ))
  root <- tempfile("acq-1d-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  runtime <- file.path(root, "runtime.sqlite")
  init_db(runtime)
  connection <- open_db(runtime)
  sqlite <- DBI::dbGetQuery(connection, "SELECT sqlite_version() AS v")$v[[1L]]
  journal <- DBI::dbGetQuery(connection, "PRAGMA journal_mode")$journal_mode[[1L]]
  close_db(connection)
  scenarios <- do.call(
    rbind,
    lapply(c(1L, 2L, 4L), scenario, script = script, root = root)
  )
  crash <- recovery(script, root)
  metric <- function(name, value, unit, details = "-") data.frame(
    metric = name,
    value = as.character(value),
    unit = unit,
    details = details
  )
  evidence <- rbind(
    metric("run_at_utc", format(Sys.time(), tz = "UTC", usetz = TRUE), "timestamp"),
    metric("runtime.R_version", paste(R.version$major, R.version$minor, sep = "."), "version"),
    metric("runtime.DBI_version", packageVersion("DBI"), "version"),
    metric("runtime.RSQLite_version", packageVersion("RSQLite"), "version"),
    metric("runtime.sqlite_version", sqlite, "version"),
    metric("runtime.journal_mode", journal, "mode"),
    metric("runtime.wal_reset_floor_ok", numeric_version(sqlite) >= numeric_version("3.51.3"), "boolean"),
    do.call(rbind, lapply(seq_len(nrow(scenarios)), function(index) {
      row <- scenarios[index, ]
      prefix <- paste0("contention.", row$workers, "workers.")
      rbind(
        metric(paste0(prefix, "jobs"), row$jobs, "count"),
        metric(paste0(prefix, "elapsed_ms"), sprintf("%.2f", row$elapsed_ms), "ms"),
        metric(paste0(prefix, "claims"), row$claims, "count"),
        metric(paste0(prefix, "completed"), row$completed, "count"),
        metric(paste0(prefix, "unique_tokens"), row$unique_tokens, "count"),
        metric(paste0(prefix, "duplicate_claims"), row$duplicate_claims, "count"),
        metric(paste0(prefix, "max_attempt"), row$max_attempt, "count"),
        metric(paste0(prefix, "claim_p50_ms"), sprintf("%.2f", row$claim_p50_ms), "ms"),
        metric(paste0(prefix, "claim_p95_ms"), sprintf("%.2f", row$claim_p95_ms), "ms"),
        metric(paste0(prefix, "claim_max_ms"), sprintf("%.2f", row$claim_max_ms), "ms"),
        metric(paste0(prefix, "busy_retries"), row$busy_retries, "count")
      )
    })),
    metric("recovery.marker_rows_after_kill", crash$marker_rows_after_kill, "count"),
    metric("recovery.sqlite_reopened_after_kill", crash$reopened, "boolean"),
    metric("recovery.reclaimed", crash$reclaimed, "boolean"),
    metric("recovery.stale_completion_rows", crash$stale_completion_rows, "count"),
    metric("recovery.fresh_completion_rows", crash$fresh_completion_rows, "count"),
    metric("recovery.final_state", crash$final_state, "state"),
    metric("recovery.attempts", crash$attempts, "count"),
    metric("recovery.unique_tokens", crash$unique_tokens, "count")
  )
  write.table(evidence, output, sep = "\t", row.names = FALSE, quote = FALSE)
  cat(output, "\n", sep = "")
}

if (mode == "worker") {
  tryCatch(
    worker(),
    error = function(error) {
      writeLines(conditionMessage(error), arg("--error"))
      quit("no", 1L)
    }
  )
} else if (mode == "crash") {
  crash_worker()
} else {
  run_probe()
}
