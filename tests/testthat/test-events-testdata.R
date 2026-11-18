test_that("event buffer redacts private attributes", {
  buf <- EventBuffer$new(private_attrs = c("email"))
  ctx <- context_to_list(ld_context("u", email = "a@b.c", name = "Ada"))
  buf$add_identify(ctx)
  ev <- buf$drain()
  expect_equal(length(ev), 1)
  expect_null(ev[[1]]$context$email)
  expect_equal(ev[[1]]$context$name, "Ada")
})

test_that("test data source updates client store", {
  td <- ld_test_data()
  td$flag("f", on = TRUE, variations = list("a", "b"), fallthrough = list(variation = 1L))
  client <- ld_client(ld_config("k", offline = TRUE))
  on.exit(client$close(), add = TRUE)
  td$apply_to_client(client)
  expect_equal(client$variation("f", ld_context("u"), "d"), "b")
})

test_that("flag change listener fires on test apply via tracker notify", {
  tracker <- FlagTracker$new()
  seen <- character()
  tracker$add_flag_change_listener(function(key) seen <<- c(seen, key))
  tracker$notify_store_update(list(), list(a = list(key = "a", version = 1)))
  expect_equal(seen, "a")
})

test_that("streaming parser accepts SSE body via webfakes", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/meval/v2/stream", function(req, res) {
    body <- paste(
      "event: server-intent",
      "data: {\"payload\":{\"code\":\"xfer-full\"}}",
      "",
      "event: put-object",
      "data: {\"kind\":\"flag\",\"key\":\"sf\",\"version\":1,\"object\":{\"key\":\"sf\",\"on\":true,\"variations\":[false,true],\"fallthrough\":{\"variation\":1}}}",
      "",
      "event: payload-transferred",
      "data: {\"state\":\"s\",\"version\":1}",
      "",
      sep = "\n"
    )
    res$set_header("Content-Type", "text/event-stream")
    res$send(body)
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  cfg <- list(sdk_key = "k", stream_uri = server$url(), read_timeout = 5)
  result <- fdv2_stream_once(cfg, NULL)
  expect_true(result$ok)
  expect_equal(result$changeset$selector$state, "s")
})
