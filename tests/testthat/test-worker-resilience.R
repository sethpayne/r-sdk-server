# The worker must survive anything the network hands it (D4) and stop
# retrying when the SDK key is rejected (S1).

bad_bodies <- list(
  garbage = "this is not json",
  scalar = "30",
  array = "[1,2,3]",
  empty_object = "{}",
  events_scalar = '{"events": 5}',
  bad_event = '{"events": [{"event": 7, "data": "x"}]}',
  bad_put = '{"events": [{"event": "server-intent", "data": {"payloads": [{"intentCode": "xfer-full"}]}},{"event": "put-object", "data": {"kind": "flag"}}]}'
)

for (nm in names(bad_bodies)) {
  local({
    body <- bad_bodies[[nm]]
    test_that(sprintf("worker survives a malformed poll body (%s) and recovers", nm), {
      mock <- local_mock_ld()
      mock$script("/sdk/poll",
                  list(status = 200L, headers = list(`Content-Type` = "application/json"), body = body),
                  list(status = 200L, headers = list(`Content-Type` = "application/json"), body = body),
                  poll_ok(list(simple_flag("f"))))
      client <- local_polling_client(mock, poll_interval = 0.2, start_wait = 5)
      pid <- worker_of(client)$pid()
      expect_true(wait_for(function() client$variation("f", ld_context("u"), FALSE), timeout = 10))
      expect_true(client$is_initialized())
      expect_identical(worker_of(client)$pid(), pid)
      expect_equal(client$data_source_status(), "valid")
    })
  })
}

test_that("polling recovers from server errors without a restart", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", list(status = 500L), list(status = 503L), poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock, poll_interval = 0.2, start_wait = 5)
  pid <- worker_of(client)$pid()
  expect_true(wait_for(function() client$variation("f", ld_context("u"), FALSE)))
  expect_identical(worker_of(client)$pid(), pid)
})

test_that("an uninitialized client reports interrupted while the server fails", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", list(status = 500L))
  client <- local_polling_client(mock, poll_interval = 0.2, start_wait = 2)
  expect_false(client$is_initialized())
  expect_true(wait_for(function() identical(client$data_source_status(), "interrupted")))
  expect_equal(client$variation("f", ld_context("u"), "d"), "d")
})

for (code in c(401L, 403L)) {
  local({
    status <- code
    test_that(sprintf("HTTP %d stops polling and turns the data source off (S1)", status), {
      mock <- local_mock_ld()
      mock$script("/sdk/poll", list(status = status, body = "denied"))
      client <- local_polling_client(mock, poll_interval = 0.1, start_wait = 3)
      expect_true(wait_for(function() identical(client$data_source_status(), "off")))
      n <- length(mock$requests("/sdk/poll"))
      Sys.sleep(1)
      expect_equal(length(mock$requests("/sdk/poll")), n)
      expect_lte(n, 2)
      expect_equal(client$variation("f", ld_context("u"), "d"), "d")
    })
  })
}

test_that("a 401 on the primary falls back to the configured fallback poll URI", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", list(status = 401L))
  mock$script("/alt/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock, poll_interval = 0.2, start_wait = 5,
                                 fallback_poll_uri = paste0(mock$url, "/alt"))
  expect_true(wait_for(function() client$variation("f", ld_context("u"), FALSE)))
  expect_gte(length(mock$requests("/alt/sdk/poll")), 1)
})

test_that("X-LD-FD-Fallback puts the client in fail-safe without throwing", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", list(status = 200L, headers = list(`X-LD-FD-Fallback` = "true",
                                                             `Content-Type` = "application/json"),
                                body = fdv2_poll_body()))
  client <- local_polling_client(mock, start_wait = 5)
  expect_equal(client$data_source_status(), "off")
  expect_equal(client$variation("f", ld_context("u"), "d"), "d")
})

test_that("a slow server does not block construction beyond start_wait", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", c(poll_ok(list(simple_flag("f"))), list(delay = 3)))
  t <- system.time(client <- local_polling_client(mock, start_wait = 0.5, read_timeout = 10))
  expect_lt(t[["elapsed"]], 2.5)
  expect_false(client$is_initialized())
  expect_true(wait_for(function() client$is_initialized(), timeout = 10))
  expect_true(client$variation("f", ld_context("u"), FALSE))
})

test_that("streaming delivers updates and survives a malformed SSE event", {
  mock <- local_mock_ld()
  good <- fdv2_sse_body(list(simple_flag("f")))
  broken <- paste0("event: put-object\ndata: {not json\n\n",
                   "event: payload-transferred\ndata: 5\n\n")
  mock$script("/sdk/stream",
              list(status = 200L, headers = list(`Content-Type` = "text/event-stream"), body = broken),
              list(status = 200L, headers = list(`Content-Type` = "text/event-stream"), body = good))
  mock$script("/sdk/poll", list(status = 500L))
  client <- local_ld_client("sdk-key-123456", base_uri = mock$url, stream_uri = mock$url,
                            events_uri = mock$url, stream = TRUE, skip_poll_init = TRUE,
                            initial_reconnect_delay = 0.1, poll_interval = 0.2,
                            send_events = FALSE, start_wait = 5)
  pid <- worker_of(client)$pid()
  expect_true(wait_for(function() client$variation("f", ld_context("u"), FALSE), timeout = 15))
  expect_identical(worker_of(client)$pid(), pid)
})
