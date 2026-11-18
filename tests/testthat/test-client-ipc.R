# Parent-side IPC handling with a scripted fake worker (no subprocess).

FakeWorker <- R6::R6Class("FakeWorker", public = list(
  sent = list(),
  inbox = list(),
  alive = TRUE,
  on_message = NULL,
  initialize = function(on_message = NULL) {
    self$on_message <- on_message
    FakeWorker$last <- self
  },
  start = function() invisible(self),
  is_alive = function() self$alive,
  pid = function() 99999L,
  pending_bytes = function() 0L,
  send = function(type, payload = list(), id = NULL) {
    if (!self$alive) stop("Worker is not running")
    self$sent[[length(self$sent) + 1L]] <- list(type = type, payload = payload)
    if (identical(type, "Start") && length(FakeWorker$on_start)) {
      self$inbox <- c(self$inbox, FakeWorker$on_start)
    }
    invisible(self)
  },
  push = function(type, payload = list()) {
    msg <- if (is.list(type)) type else list(type = type, payload = payload)
    self$inbox[[length(self$inbox) + 1L]] <- msg
  },
  drain = function(timeout_ms = 0) {
    msgs <- self$inbox
    self$inbox <- list()
    for (m in msgs) self$on_message(m)
    invisible(msgs)
  },
  wait_for_type = function(type, timeout_s = 5) {
    for (m in self$drain()) if (identical(m$type, type)) return(m)
    NULL
  },
  shutdown = function(timeout_s = 2) {
    self$sent[[length(self$sent) + 1L]] <- list(type = "Shutdown", payload = list())
    self$alive <- FALSE
  },
  sent_types = function() vapply(self$sent, function(m) m$type, character(1))
))
FakeWorker$last <- NULL
FakeWorker$on_start <- list()

snapshot_msg <- function(flags, selector = list(state = "s", version = 1L)) {
  names(flags) <- vapply(flags, function(f) f$key, character(1))
  list(type = "StoreSnapshot", payload = list(flags = flags, segments = list(), selector = selector))
}

local_fake_client <- function(on_start = list(snapshot_msg(list(simple_flag("f"))),
                                              list(type = "Ready", payload = list(initialized = TRUE))),
                              ..., .env = parent.frame()) {
  FakeWorker$on_start <- on_start
  local_mocked_bindings(ProcessWorker = FakeWorker, .package = "launchdarklyServer", .env = .env)
  client <- local_ld_client("sdk-key-123456", base_uri = "http://fake", ..., .env = .env)
  list(client = client, worker = FakeWorker$last)
}

test_that("Start carries the full config and Ready + snapshot initialize the client", {
  fc <- local_fake_client()
  expect_true(fc$client$is_initialized())
  start <- fc$worker$sent[[1]]
  expect_equal(start$type, "Start")
  expect_equal(start$payload$sdk_key, "sdk-key-123456")
  expect_setequal(names(start$payload), names(ld_config("k")$to_list()))
  expect_true(fc$client$variation("f", ld_context("u"), FALSE))
})

test_that("Ready without data leaves the client uninitialized", {
  fc <- local_fake_client(on_start = list(list(type = "Ready", payload = list(initialized = FALSE))))
  expect_false(fc$client$is_initialized())
  expect_equal(fc$client$variation_detail("f", ld_context("u"), "d")$reason$errorKind,
               "CLIENT_NOT_READY")
  fc$worker$push(snapshot_msg(list(simple_flag("f"))))
  expect_true(fc$client$is_initialized())
  expect_true(fc$client$variation("f", ld_context("u"), FALSE))
})

test_that("Ready with fail_safe turns the data source off and serves defaults", {
  fc <- local_fake_client(on_start = list(list(type = "Ready", payload = list(fail_safe = TRUE))))
  expect_equal(fc$client$data_source_status(), "off")
  expect_equal(fc$client$variation("f", ld_context("u"), "d"), "d")
})

test_that("Status messages update data_source_status", {
  fc <- local_fake_client()
  for (s in c("valid", "interrupted", "off")) {
    fc$worker$push("Status", list(state = s))
    expect_equal(fc$client$data_source_status(), s)
  }
})

test_that("evaluations send Events records with prerequisites first", {
  prereq <- simple_flag("p", trackEvents = TRUE)
  main <- simple_flag("f", prerequisites = list(list(key = "p", variation = 1L)))
  fc <- local_fake_client(on_start = list(snapshot_msg(list(prereq, main)),
                                          list(type = "Ready", payload = list(initialized = TRUE))))
  expect_true(fc$client$variation("f", ld_context("u"), FALSE))
  ev <- Filter(function(m) m$type == "Events", fc$worker$sent)
  expect_length(ev, 1)
  recs <- ev[[1]]$payload$events
  expect_equal(vapply(recs, function(r) r$key, character(1)), c("p", "f"))
  expect_equal(recs[[1]]$prereqOf, "f")
  expect_null(recs[[1]]$default)
  expect_false(recs[[2]]$default)
})

test_that("no Events are sent when send_events is FALSE or the context is invalid", {
  fc <- local_fake_client(send_events = FALSE)
  fc$client$variation("f", ld_context("u"), FALSE)
  fc$client$track("e", ld_context("u"))
  fc$client$identify(ld_context("u"))
  expect_false(any(fc$worker$sent_types() %in% c("Events", "Track", "Identify")))

  fc2 <- local_fake_client()
  fc2$client$variation("f", context_create(""), FALSE)
  fc2$client$variation(NA_character_, ld_context("u"), FALSE)
  expect_false("Events" %in% fc2$worker$sent_types())
})

test_that("track and identify forward serialized contexts and timestamps", {
  fc <- local_fake_client()
  fc$client$track("buy", ld_context("u"), data = list(a = 1), metric_value = 2)
  fc$client$identify(ld_context("v"))
  tr <- Filter(function(m) m$type == "Track", fc$worker$sent)[[1]]$payload
  expect_equal(tr$event_name, "buy")
  expect_equal(tr$context, list(key = "u", kind = "user"))
  expect_equal(tr$metric_value, 2)
  expect_true(is.numeric(tr$creation_date))
  id <- Filter(function(m) m$type == "Identify", fc$worker$sent)[[1]]$payload
  expect_equal(id$context$key, "v")
})

test_that("Log and Fatal messages are surfaced with the key redacted", {
  fc <- local_fake_client()
  fc$worker$push("Log", list(level = "warn", message = "using sdk-key-123456"))
  expect_message(fc$client$is_initialized(), "[ld:warn] using <redacted>", fixed = TRUE)
  fc$worker$push("Fatal", list(message = "died with sdk-key-123456"))
  expect_warning(fc$client$is_initialized(), "died with <redacted>", fixed = TRUE)
})

test_that("unknown message types are ignored", {
  fc <- local_fake_client()
  fc$worker$push("SomethingNew", list(x = 1))
  expect_no_error(expect_true(fc$client$is_initialized()))
})

test_that("value listeners do not fire for unrelated changes (S5)", {
  fc <- local_fake_client()
  fired <- list()
  fc$client$flag_tracker()$add_flag_value_change_listener(
    "f", ld_context("u"), function(ev) fired[[length(fired) + 1L]] <<- ev
  )
  fc$worker$push(snapshot_msg(list(simple_flag("f", version = 2L, salt = "other"))))
  fc$client$is_initialized()
  expect_length(fired, 0)
  fc$worker$push(snapshot_msg(list(simple_flag("f", value_on = "new", version = 3L))))
  fc$client$is_initialized()
  expect_length(fired, 1)
  expect_true(fired[[1]]$oldValue)
  expect_equal(fired[[1]]$newValue, "new")
})

test_that("a dead worker is restarted after a backoff with a warning", {
  fc <- local_fake_client()
  first <- fc$worker
  first$alive <- FALSE
  expect_warning(fc$client$is_initialized(), "restarting")
  expect_true(fc$client$variation("f", ld_context("u"), FALSE))
  expect_identical(worker_of(fc$client), first)
  fc$client$.__enclos_env__$private$restart_at_ <- 0
  expect_no_warning(fc$client$is_initialized())
  expect_false(identical(worker_of(fc$client), first))
  expect_equal(FakeWorker$last$sent[[1]]$type, "Start")
})

test_that("close sends Shutdown once", {
  fc <- local_fake_client()
  fc$client$close()
  fc$client$close()
  expect_equal(sum(fc$worker$sent_types() == "Shutdown"), 1L)
})

test_that("ProcessWorker buffers writes the pipe cannot take and caps the backlog (S3)", {
  w <- ProcessWorker$new()
  priv <- w$.__enclos_env__$private
  accept <- 10L
  fake_proc <- list(
    is_alive = function() TRUE,
    write_input = function(bytes) if (length(bytes) > accept) bytes[-seq_len(accept)] else raw(0)
  )
  priv$proc_ <- fake_proc
  priv$max_pending_ <- 200
  w$send("Track", list(n = 1))
  expect_gt(w$pending_bytes(), 0)
  before <- w$pending_bytes()
  w$flush_input()
  expect_equal(w$pending_bytes(), max(0, before - accept))
  expect_warning(for (i in 1:20) w$send("Track", list(n = i)), "IPC buffer full")
  expect_gt(w$dropped_messages(), 0)
  expect_lte(w$pending_bytes(), 200)
  accept <- 1e6L
  w$flush_input()
  expect_equal(w$pending_bytes(), 0)
})
