# Outgoing HTTP request contract (headers, paths, query) against the mock.

http_cfg <- function(mock, ...) {
  utils::modifyList(ld_config("sdk-key-123456", base_uri = mock$url, stream_uri = mock$url,
                              events_uri = mock$url, read_timeout = 5)$to_list(), list(...))
}

test_that("poll requests carry auth, user agent, accept and tags", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  cfg <- http_cfg(mock, application_id = "my-app", application_version = "1.2.3")
  res <- fdv2_poll_once(cfg)
  expect_true(res$ok)
  r <- mock$requests("/sdk/poll")[[1]]
  expect_equal(r$method, "GET")
  expect_equal(req_header(r, "Authorization"), "sdk-key-123456")
  expect_equal(req_header(r, "User-Agent"), LD_USER_AGENT)
  expect_match(LD_USER_AGENT, "/[0-9]+\\.[0-9]+")
  expect_equal(req_header(r, "Accept"), "application/json")
  expect_equal(req_header(r, "X-LaunchDarkly-Tags"), "application-id/my-app application-version/1.2.3")
  expect_equal(r$query, "")
})

test_that("invalid tag values are omitted rather than sent", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok())
  fdv2_poll_once(http_cfg(mock, application_id = "bad id!", application_version = strrep("v", 65)))
  expect_null(req_header(mock$requests("/sdk/poll")[[1]], "X-LaunchDarkly-Tags"))
})

test_that("basis and filter are URL-encoded on poll and stream", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok())
  mock$script("/sdk/stream", list(status = 200L, headers = list(`Content-Type` = "text/event-stream"),
                                  body = fdv2_sse_body()))
  cfg <- http_cfg(mock, payload_filter_key = "my filter&x")
  sel <- list(state = "(p:abc:1)/=&", version = 1L)
  fdv2_poll_once(cfg, sel)
  fdv2_stream_once(cfg, sel, until_first_payload = TRUE)
  for (path in c("/sdk/poll", "/sdk/stream")) {
    r <- mock$requests(path)[[1]]
    q <- httr2::url_parse(paste0("http://x/?", r$query))$query
    expect_equal(q$basis, "(p:abc:1)/=&", label = path)
    expect_equal(q$filter, "my filter&x", label = path)
    expect_false(grepl("&x", sub("filter=[^&]*", "", r$query), fixed = TRUE), label = path)
  }
  expect_equal(req_header(mock$requests("/sdk/stream")[[1]], "Authorization"), "sdk-key-123456")
})

test_that("custom base URIs with trailing slashes are joined without '//'", {
  mock <- local_mock_ld()
  mock$script("/alt/sdk/poll", poll_ok())
  res <- fdv2_poll_once(http_cfg(mock), base_uri = paste0(mock$url, "/alt/"))
  expect_true(res$ok)
  expect_length(mock$requests("/alt/sdk/poll"), 1)
})

test_that("event posts carry schema, payload id and JSON body", {
  mock <- local_mock_ld()
  cfg <- http_cfg(mock, application_id = "my-app")
  ev <- list(list(kind = "identify", creationDate = 1, context = list(kind = "user", key = "u")))
  res <- ld_post_events(cfg, ev, retry_delay = 0)
  expect_true(res$ok)
  r <- mock$requests("/bulk")[[1]]
  expect_equal(r$method, "POST")
  expect_equal(req_header(r, "Authorization"), "sdk-key-123456")
  expect_equal(req_header(r, "Content-Type"), "application/json")
  expect_equal(req_header(r, "X-LaunchDarkly-Event-Schema"), "4")
  expect_match(req_header(r, "X-LaunchDarkly-Payload-ID"),
               "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
  expect_equal(req_header(r, "X-LaunchDarkly-Tags"), "application-id/my-app")
  expect_equal(from_json(r$body), ev)
})

test_that("event delivery retries once with the same payload id (S4)", {
  mock <- local_mock_ld()
  mock$script("/bulk", list(status = 503L), list(status = 202L))
  res <- ld_post_events(http_cfg(mock), list(list(kind = "identify")), retry_delay = 0)
  expect_true(res$ok)
  rs <- mock$requests("/bulk")
  expect_length(rs, 2)
  expect_equal(req_header(rs[[1]], "X-LaunchDarkly-Payload-ID"),
               req_header(rs[[2]], "X-LaunchDarkly-Payload-ID"))
  expect_equal(rs[[1]]$body, rs[[2]]$body)
})

test_that("event delivery gives up after one retry and flags 401 as unrecoverable", {
  mock <- local_mock_ld()
  mock$script("/bulk", list(status = 500L))
  res <- ld_post_events(http_cfg(mock), list(list(kind = "identify")), retry_delay = 0)
  expect_false(res$ok)
  expect_false(isTRUE(res$unrecoverable))
  expect_length(mock$requests("/bulk"), 2)

  mock$reset()
  mock$script("/bulk", list(status = 401L))
  res <- ld_post_events(http_cfg(mock), list(list(kind = "identify")), retry_delay = 0)
  expect_false(res$ok)
  expect_true(res$unrecoverable)
  expect_length(mock$requests("/bulk"), 1)
})

test_that("recoverable HTTP statuses follow the LaunchDarkly convention", {
  expect_true(all(vapply(c(400L, 408L, 429L, 500L, 503L), .ld_http_error_recoverable, logical(1))))
  expect_false(any(vapply(c(401L, 403L, 404L, 413L), .ld_http_error_recoverable, logical(1))))
})

test_that("no events are posted when send_events is FALSE", {
  mock <- local_mock_ld()
  res <- ld_post_events(http_cfg(mock, send_events = FALSE), list(list(kind = "identify")))
  expect_length(mock$requests("/bulk"), 0)
})

test_that("a polling client sends the SDK key and the basis on later polls", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f")), state = "(p:s1:1)"))
  client <- local_polling_client(mock, poll_interval = 0.2)
  expect_true(client$is_initialized())
  expect_true(wait_for(function() length(mock$requests("/sdk/poll")) >= 3))
  rs <- mock$requests("/sdk/poll")
  expect_true(all(vapply(rs, function(r) identical(req_header(r, "Authorization"), "sdk-key-123456"),
                         logical(1))))
  expect_equal(rs[[1]]$query, "")
  expect_match(rs[[length(rs)]]$query, "basis=", fixed = TRUE)
})
