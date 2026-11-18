# Worker process lifecycle against a real worker and the mock service.

test_that("close() stops the worker and is idempotent", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock)
  pid <- worker_of(client)$pid()
  expect_true(process_alive(pid))
  client$close()
  expect_true(wait_for(function() !process_alive(pid), timeout = 5))
  expect_no_error(client$close())
  expect_equal(client$variation("f", ld_context("u"), "d"), TRUE)
})

test_that("garbage-collecting an unclosed client stops its worker", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- ld_client(ld_config("sdk-key-123456", base_uri = mock$url, events_uri = mock$url,
                                stream = FALSE, poll_interval = 60, send_events = FALSE),
                      start_wait = 10)
  pid <- worker_of(client)$pid()
  expect_true(process_alive(pid))
  rm(client)
  invisible(gc())
  expect_true(wait_for(function() !process_alive(pid), timeout = 5))
})

test_that("a killed worker is restarted and the last snapshot keeps serving", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock, send_events = FALSE)
  expect_true(client$variation("f", ld_context("u"), FALSE))
  pid <- worker_of(client)$pid()
  tools::pskill(pid)
  expect_true(wait_for(function() !process_alive(pid), timeout = 5))
  expect_warning(client$is_initialized(), "exited unexpectedly; restarting")
  expect_equal(client$data_source_status(), "interrupted")
  expect_true(client$variation("f", ld_context("u"), FALSE))
  expect_true(wait_for(function() {
    client$is_initialized()
    w <- worker_of(client)
    w$is_alive() && !identical(w$pid(), pid)
  }, timeout = 10))
  expect_true(wait_for(function() identical(client$data_source_status(), "valid"), timeout = 10))
})

test_that("start_wait bounds construction when the service is unreachable", {
  skip_on_cran()
  t <- system.time(client <- local_ld_client("k", base_uri = "http://127.0.0.1:1", stream = FALSE,
                                             send_events = FALSE, start_wait = 0.5))
  expect_lt(t[["elapsed"]], 5)
  expect_false(client$is_initialized())
  expect_true(worker_of(client)$is_alive())
})

test_that("flag change listeners fire when the worker delivers an update", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f", version = 1L)), state = "s1"),
              poll_ok(list(simple_flag("f", value_on = "x", version = 2L)), state = "s2"))
  client <- local_polling_client(mock, poll_interval = 0.2, send_events = FALSE)
  changed <- character()
  values <- list()
  client$flag_tracker()$add_flag_change_listener(function(k) changed <<- c(changed, k))
  client$flag_tracker()$add_flag_value_change_listener("f", ld_context("u"),
                                                       function(ev) values[[length(values) + 1L]] <<- ev)
  # Listeners run when IPC is drained, i.e. during API calls (S6).
  expect_true(wait_for(function() {
    client$is_initialized()
    "f" %in% changed
  }))
  expect_length(values, 1)
  expect_true(values[[1]]$oldValue)
  expect_equal(values[[1]]$newValue, "x")
})

test_that("listeners fire without API calls in an idle process (S6)", {
  skip("S6: IPC is drained only inside API calls; idle Shiny/Plumber hosts need `later` integration")
})
