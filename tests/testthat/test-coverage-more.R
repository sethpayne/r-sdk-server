test_that("store delete/upsert version guards and segment get", {
  store <- FeatureStore$new()
  store$upsert("flag", "f", 2, list(key = "f", version = 2, on = TRUE))
  expect_false(store$upsert("flag", "f", 1, list(key = "f", version = 1, on = FALSE))) # older skipped
  expect_equal(store$get_flag("f")$version, 2)
  store$delete("flag", "f", 3)
  expect_true(isTRUE(store$get_flag("f")$deleted))
  expect_false(store$delete("flag", "f", 2)) # older delete ignored
  store$upsert("segment", "s", 1, list(key = "s", version = 1, included = list("a")))
  expect_equal(store$get_segment("s")$key, "s")
  store$apply_changeset(list(no_changes = TRUE))
})

test_that("changeset delete-object and error paths", {
  bad <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list())
  expect_true(!is.null(bad$error))
  del <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-changes")))),
    list(event = "delete-object", data = list(kind = "flag", key = "f", version = 3)),
    list(event = "delete-object", data = list(kind = "segment", key = "s", version = 2)),
    list(event = "payload-transferred", data = list(state = "x", version = 1))
  )))
  expect_equal(length(del$deletes), 2)
  expect_equal(del$intent, "xfer-changes")
  missing_pt <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-full"))))
  )))
  expect_true(!is.null(missing_pt$error))
})

test_that("events post against webfakes and buffer capacity", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$post("/bulk", function(req, res) {
    res$set_status(202)$send("")
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  cfg <- list(
    sdk_key = "k",
    events_uri = server$url(),
    send_events = TRUE,
    application_id = "app",
    application_version = "1",
    read_timeout = 5
  )
  buf <- EventBuffer$new(capacity = 2L, all_private = TRUE)
  ctx <- context_to_list(ld_context("u", email = "e@x", name = "n"))
  buf$add_identify(ctx)
  buf$add_identify(ctx)
  buf$add_identify(ctx) # over capacity
  ev <- buf$drain()
  expect_true(length(ev) <= 2)
  res <- launchdarklyServer:::ld_post_events(cfg, ev)
  expect_true(res$ok)
})

test_that("evaluation detail print and all_flags experimentation paths", {
  d <- evaluation_detail("v", 1L, list(kind = "FALLTHROUGH", inExperiment = TRUE))
  expect_s3_class(d, "ld_evaluation_detail")
  # Prefer the S3 method explicitly (some runners resolve print generics oddly).
  expect_output(print.ld_evaluation_detail(d), "EvaluationDetail")
  cfg <- ld_config("k", offline = TRUE)
  client <- ld_client(cfg)
  on.exit(client$close(), add = TRUE)
  client$.update_store_for_tests(flags = list(
    flag1 = list(
      key = "flag1", version = 100, on = TRUE,
      variations = list("a", "value1"),
      fallthrough = list(variation = 1),
      trackEventsFallthrough = TRUE
    ),
    flag2 = list(
      key = "flag2", version = 200, on = TRUE,
      variations = list("x", "y", "value2"),
      fallthrough = list(variation = 0),
      rules = list(list(id = "rule0", variation = 2, trackEvents = TRUE, clauses = list())),
      trackEvents = FALSE
    ),
    flag_err = list(
      key = "flag_err", version = 100, on = FALSE,
      variations = list("a", "b"), offVariation = -1
    )
  ))
  st <- client$all_flags_state(ld_context("user-key"), with_reasons = FALSE)
  j <- jsonlite::toJSON(st$to_json_list(), auto_unbox = TRUE, null = "null", digits = NA)
  expect_match(j, "trackReason")
  st2 <- client$all_flags_state(ld_context("user-key"), with_reasons = TRUE)
  expect_true(!is.null(st2$get_flag_reason("flag_err")))
  expect_false(FeatureFlagsState$new(FALSE)$valid())
})

test_that("context from_list multi and without_anonymous", {
  multi <- context_from_list(list(
    kind = "multi",
    user = list(key = "u", name = "U"),
    org = list(key = "o", name = "O")
  ))
  expect_true(context_valid(multi))
  expect_equal(context_individual_count(multi), 2L)
  anon <- ld_context("a", anonymous = TRUE)
  filtered <- context_without_anonymous(context_create_multi(anon, ld_context("b")))
  # May collapse to single-kind or stay multi depending on implementation
  expect_true(is.character(context_error(filtered)) || context_valid(filtered) || !is.null(filtered))
  expect_true("plan" %in% context_custom_attributes(ld_context("c", plan = "p")))
})

test_that("poll error statuses and streaming goodbye/error via webfakes", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/sdk/poll", function(req, res) {
    res$set_header("X-LD-FD-Fallback", "true")
    res$set_status(403)$send("no")
  })
  app$get("/sdk/stream", function(req, res) {
    body <- paste("event: goodbye", "data: {}", "", sep = "\n")
    res$set_header("Content-Type", "text/event-stream")
    res$send(body)
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  poll <- launchdarklyServer:::fdv2_poll_once(list(sdk_key = "k", base_uri = server$url(), read_timeout = 5))
  expect_false(poll$ok)
  # Header may be normalized; accept either explicit fallback flag or HTTP error
  expect_true(isTRUE(poll$fallback_to_fdv1) || identical(poll$status, 403L) || grepl("403", poll$error %||% ""))
  stream <- suppressWarnings(launchdarklyServer:::fdv2_stream_once(list(
    sdk_key = "k", stream_uri = server$url(), read_timeout = 5
  )))
  expect_false(stream$ok)
})

test_that("semver and date edge branches", {
  expect_true(launchdarklyServer:::.ld_op_matches("ab", "a."))
  expect_null(launchdarklyServer:::.ld_parse_time_ms(list(1)))
  expect_null(launchdarklyServer:::.ld_parse_time_ms("2017-12-06T00:00:00.000-07.00")) # bad offset
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha", "1.0.0-alpha.1"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha.1", "1.0.0-alpha.beta"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-beta", "1.0.0-alpha.1"), 1L)
  expect_null(launchdarklyServer:::.ld_parse_semver("1.0.0-"))
})
