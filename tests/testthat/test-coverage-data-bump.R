# Extra offline coverage for data_system DoD group (store / FDv2).

test_that("streaming helpers cover header/SSE edge branches", {
  # Character (non-raw) headers — must be one CRLF-delimited string
  ph <- launchdarklyServer:::.ld_parse_raw_http_headers(paste(
    c(
      "HTTP/1.1 200 OK",
      "NotAHeader",
      "X-LD-EnvID: env-z",
      "X-LD-FD-Fallback: TRUE",
      ""
    ),
    collapse = "\r\n"
  ))
  expect_equal(ph$status, 200L)
  expect_equal(ph$headers[["x-ld-envid"]], "env-z")

  expect_true(is.na(launchdarklyServer:::.ld_parse_raw_http_headers(raw(0))$status))
  expect_true(is.na(launchdarklyServer:::.ld_parse_raw_http_headers(charToRaw("Garbage\r\n\r\n"))$status))

  ev <- launchdarklyServer:::.ld_sse_parse_block("data: {\"a\":1}")
  expect_equal(ev$type, "message")
})

test_that("changeset parser covers skip/invalid delete/transferred", {
  skip_null <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(data = list()),
    list(event = "payload-transferred", data = list(state = "s", version = 1))
  )))
  expect_equal(skip_null$intent, "xfer-changes")

  bad_del <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "delete-object", data = "nope")
  )))
  expect_match(bad_del$error %||% "", "delete-object")

  bad_pt <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "payload-transferred", data = list(version = 1))
  )))
  expect_match(bad_pt$error %||% "", "payload transferred")
})

test_that("store upsert fills missing key on object", {
  store <- FeatureStore$new()
  expect_true(store$upsert("flag", "fk", 3L, list(version = 3L, on = TRUE)))
  obj <- store$get_flag("fk")
  expect_equal(obj$key, "fk")
  expect_equal(obj$version, 3L)
})

test_that("poll once covers HTTP 4xx fallback header and payload parse error", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/sdk/poll", function(req, res) {
    res$set_status(403L)
    res$set_header("X-LD-FD-Fallback", "true")
    res$send("")
  })
  p <- webfakes::new_app_process(app)
  on.exit(p$stop(), add = TRUE)
  r <- launchdarklyServer:::fdv2_poll_once(list(
    sdk_key = "k", base_uri = sub("/$", "", p$url()), read_timeout = 3
  ))
  expect_false(isTRUE(r$ok))
  expect_equal(r$status, 403L)
  expect_true(isTRUE(r$fallback_to_fdv1))

  app2 <- webfakes::new_app()
  app2$get("/sdk/poll", function(req, res) {
    res$set_header("Content-Type", "application/json")
    res$send("{}")
  })
  p2 <- webfakes::new_app_process(app2)
  on.exit(p2$stop(), add = TRUE)
  r2 <- launchdarklyServer:::fdv2_poll_once(list(
    sdk_key = "k", base_uri = sub("/$", "", p2$url()), read_timeout = 3
  ))
  expect_false(isTRUE(r2$ok))
  expect_true(nzchar(r2$error %||% ""))
})

test_that("stream body parser covers 4xx, fallback, goodbye, and incomplete", {
  skip_if_not_installed("webfakes")

  app <- webfakes::new_app()
  app$get("/sdk/stream", function(req, res) {
    res$set_status(401L)$send("")
  })
  p <- webfakes::new_app_process(app)
  on.exit(p$stop(), add = TRUE)
  req <- httr2::request(paste0(sub("/$", "", p$url()), "/sdk/stream")) |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  parsed <- launchdarklyServer:::.ld_stream_parse_body_response(resp)
  expect_false(isTRUE(parsed$ok))
  expect_equal(parsed$status, 401L)

  app2 <- webfakes::new_app()
  app2$get("/sdk/stream", function(req, res) {
    res$set_header("X-LD-FD-Fallback", "true")
    res$send("event: ping\ndata: {}\n\n")
  })
  p2 <- webfakes::new_app_process(app2)
  on.exit(p2$stop(), add = TRUE)
  resp2 <- httr2::req_perform(
    httr2::request(paste0(sub("/$", "", p2$url()), "/sdk/stream")) |>
      httr2::req_error(is_error = function(resp) FALSE)
  )
  parsed2 <- launchdarklyServer:::.ld_stream_parse_body_response(resp2)
  expect_true(isTRUE(parsed2$fallback_to_fdv1))

  app3 <- webfakes::new_app()
  app3$get("/sdk/stream", function(req, res) {
    res$set_header("Content-Type", "text/event-stream")
    res$send("event: goodbye\ndata: {\"reason\":\"x\"}\n\n")
  })
  p3 <- webfakes::new_app_process(app3)
  on.exit(p3$stop(), add = TRUE)
  resp3 <- httr2::req_perform(httr2::request(paste0(sub("/$", "", p3$url()), "/sdk/stream")))
  parsed3 <- launchdarklyServer:::.ld_stream_parse_body_response(resp3)
  expect_equal(parsed3$error, "goodbye")

  app4 <- webfakes::new_app()
  app4$get("/sdk/stream", function(req, res) {
    res$set_header("Content-Type", "text/event-stream")
    res$send("event: server-intent\ndata: {\"payloads\":[{\"intentCode\":\"xfer-full\"}]}\n")
  })
  p4 <- webfakes::new_app_process(app4)
  on.exit(p4$stop(), add = TRUE)
  resp4 <- httr2::req_perform(httr2::request(paste0(sub("/$", "", p4$url()), "/sdk/stream")))
  parsed4 <- launchdarklyServer:::.ld_stream_parse_body_response(resp4)
  expect_equal(parsed4$error, "stream incomplete")
})

test_that("fdv2_stream_once covers curl fail and fdv1 fallback header on 200", {
  skip_if_not_installed("webfakes")

  res_fail <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = "http://127.0.0.1:1", connect_timeout = 1, read_timeout = 1),
    until_first_payload = TRUE
  )
  expect_false(isTRUE(res_fail$ok))
  expect_true(nzchar(res_fail$error %||% ""))

  app <- webfakes::new_app()
  app$get("/sdk/stream", function(req, res) {
    res$set_header("X-LD-FD-Fallback", "true")
    res$set_header("Content-Type", "text/event-stream")
    res$send("event: ping\ndata: {}\n\n")
  })
  p <- webfakes::new_app_process(app)
  on.exit(p$stop(), add = TRUE)
  res_fb <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = sub("/$", "", p$url()), connect_timeout = 2, read_timeout = 3),
    until_first_payload = TRUE
  )
  expect_true(res_fb$fallback_to_fdv1)
})
