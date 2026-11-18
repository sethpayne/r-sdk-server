# Broad offline unit coverage for DoD gate (evaluator / context / data / events).
# No live network — webfakes + offline client only.

test_that("operators cover date, semver, numeric, string, and match ops", {
  expect_true(launchdarklyServer:::.ld_op_in("a", "a"))
  expect_false(launchdarklyServer:::.ld_op_in(TRUE, "a"))
  expect_true(launchdarklyServer:::.ld_op_starts_with("abcdef", "abc"))
  expect_true(launchdarklyServer:::.ld_op_ends_with("abcdef", "def"))
  expect_true(launchdarklyServer:::.ld_op_contains("abcdef", "cd"))
  expect_true(launchdarklyServer:::.ld_op_matches("abc123", "^abc"))
  expect_false(suppressWarnings(launchdarklyServer:::.ld_op_matches("abc", "["))) # invalid regex
  expect_true(launchdarklyServer:::.ld_op_lt(1, 2))
  expect_true(launchdarklyServer:::.ld_op_lte(2, 2))
  expect_true(launchdarklyServer:::.ld_op_gt(3, 2))
  expect_true(launchdarklyServer:::.ld_op_gte(2, 2))

  ms <- launchdarklyServer:::.ld_parse_time_ms("2017-12-06T00:00:00.000-07:00")
  expect_equal(ms, 1512543600000)
  expect_null(launchdarklyServer:::.ld_parse_time_ms("2017-12-06"))
  expect_null(launchdarklyServer:::.ld_parse_time_ms("bad"))
  expect_true(launchdarklyServer:::.ld_op_after(1512543600001, 1512543600000))
  expect_true(launchdarklyServer:::.ld_op_before(1512543600000, 1512543600001))
  expect_false(launchdarklyServer:::.ld_op_before(1512543600000, 1512543600000))

  expect_identical(launchdarklyServer:::.ld_semver_cmp("2", "2.0.0"), 0L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("2.0.0-rc", "2.0.0"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("2.0.0-rc.2", "2.0.0-rc.10"), -1L)
  expect_null(launchdarklyServer:::.ld_parse_semver("02.0.0"))
  expect_null(launchdarklyServer:::.ld_parse_semver("2.0.0.0"))
  expect_true(launchdarklyServer:::.ld_op_sem_eq("1.2.3", "1.2.3"))
  expect_true(launchdarklyServer:::.ld_op_sem_lt("1.2.3", "1.2.4"))
  expect_true(launchdarklyServer:::.ld_op_sem_gt("2.0.0", "1.9.9"))
  expect_false(launchdarklyServer:::.ld_match_op("nope", 1, 1))
})

test_that("attribute refs: path, literal legacy, malformed", {
  ctx <- ld_context("x", attr1 = list(prop1 = "right"), `/attr/a` = "lit")
  expect_equal(launchdarklyServer:::.ld_attr_value(ctx, "/attr1/prop1"), "right")
  expect_equal(launchdarklyServer:::.ld_attr_value(ctx, "/attr/a", literal = TRUE), "lit")
  expect_equal(launchdarklyServer:::.ld_attr_value(ctx, "attr/a", literal = TRUE), NULL)
  expect_error(launchdarklyServer:::.ld_attr_value(ctx, ""), "attribute")
  expect_error(launchdarklyServer:::.ld_attr_value(ctx, "///"), "empty")
  expect_error(launchdarklyServer:::.ld_attr_value(ctx, "/a~b"), "escape")
  expect_null(launchdarklyServer:::.ld_parse_attr_ref(NULL)$parts)
  expect_true(!is.null(launchdarklyServer:::.ld_parse_attr_ref(NULL)$error))
})

test_that("evaluator: malformed attr, cycles, bucketing, experiments", {
  get_flag <- function(k) NULL
  get_seg <- function(k) NULL

  bad <- list(
    key = "bad", on = TRUE, version = 1,
    variations = list("a", "b"),
    fallthrough = list(variation = 0),
    rules = list(list(
      id = "r", variation = 1,
      clauses = list(list(attribute = "", op = "in", values = list("x")))
    ))
  )
  d <- launchdarklyServer:::ld_evaluate_flag(bad, ld_context("u"), "def", get_flag, get_seg)
  expect_equal(d$reason$errorKind, "MALFORMED_FLAG")
  expect_equal(d$value, "def")

  # circular prereq
  a <- list(key = "a", on = TRUE, version = 1, variations = list(FALSE, TRUE),
            fallthrough = list(variation = 1),
            prerequisites = list(list(key = "b", variation = 1)))
  b <- list(key = "b", on = TRUE, version = 1, variations = list(FALSE, TRUE),
            fallthrough = list(variation = 1),
            prerequisites = list(list(key = "a", variation = 1)))
  gf <- function(k) if (k == "a") a else if (k == "b") b else NULL
  d2 <- launchdarklyServer:::ld_evaluate_flag(a, ld_context("u"), FALSE, gf, get_seg)
  expect_equal(d2$reason$errorKind, "MALFORMED_FLAG")

  # rollout bucketing
  flag <- list(
    key = "hashKey", salt = "saltyA", on = TRUE, version = 1,
    variations = list("A", "B"),
    fallthrough = list(rollout = list(
      variations = list(
        list(variation = 0, weight = 10000),
        list(variation = 1, weight = 90000)
      )
    ))
  )
  d3 <- launchdarklyServer:::ld_evaluate_flag(flag, ld_context("userKeyA"), "d", get_flag, get_seg)
  expect_true(d3$value %in% c("A", "B"))
  expect_equal(d3$reason$kind, "FALLTHROUGH")

  # non-bucketable attr type → bucket 0
  expect_null(launchdarklyServer:::.ld_bucketable_string(1.5))
  expect_null(launchdarklyServer:::.ld_bucketable_string(TRUE))
  expect_equal(launchdarklyServer:::.ld_bucketable_string(2L), "2")

  # experiment ignores bucketBy
  exp_flag <- list(
    key = "exp", salt = "s", on = TRUE, version = 1,
    variations = list("a", "b"),
    fallthrough = list(rollout = list(
      kind = "experiment",
      seed = 42,
      bucketBy = "attr1",
      variations = list(
        list(variation = 0, weight = 50000),
        list(variation = 1, weight = 50000)
      )
    ))
  )
  d4 <- launchdarklyServer:::ld_evaluate_flag(exp_flag, ld_context("u", attr1 = "x"), "d", get_flag, get_seg)
  expect_true(!is.null(d4$variation_index))
})

test_that("FeatureFlagsState preserves null values and experimentation meta", {
  s <- FeatureFlagsState$new(TRUE)
  s$add_flag("flag1", NULL, version = 100L, reason = list(kind = "ERROR", errorKind = "MALFORMED_FLAG"))
  s$add_flag("flag2", "value2", variation = 2L, version = 200L,
             reason = list(kind = "FALLTHROUGH"), track_events = TRUE, track_reason = TRUE)
  j <- jsonlite::toJSON(s$to_json_list(), auto_unbox = TRUE, null = "null", digits = NA)
  expect_match(j, '"flag1":null')
  expect_match(j, '"trackReason":true')
  expect_true(s$valid())
  expect_null(s$get_flag_value("flag1"))
  expect_equal(s$get_flag_value("flag2"), "value2")
})

test_that("store changeset upsert prefers object version", {
  store <- FeatureStore$new()
  cs <- list(
    intent = "xfer-full",
    puts = list(list(
      kind = "flag", key = "f", version = 99,
      object = list(key = "f", version = 5, on = TRUE, variations = list(FALSE, TRUE),
                    fallthrough = list(variation = 1))
    )),
    deletes = list(),
    selector = list(state = "s", version = 1)
  )
  store$apply_changeset(cs)
  expect_equal(store$get_flag("f")$version, 5)
  expect_true(store$is_initialized())
  snap <- store$to_snapshot()
  expect_equal(snap$selector$state, "s")
})

test_that("fdv2 changeset parses server-intent payloads", {
  payload <- list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-full")))),
    list(event = "put-object", data = list(
      kind = "flag", key = "f", version = 1,
      object = list(key = "f", on = TRUE, version = 1, variations = list(TRUE), fallthrough = list(variation = 0))
    )),
    list(event = "payload-transferred", data = list(state = "st", version = 2))
  ))
  cs <- launchdarklyServer:::fdv2_polling_payload_to_changeset(payload)
  expect_equal(cs$intent, "xfer-full")
  expect_equal(cs$selector$state, "st")
  none <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-none"))))
  )))
  expect_true(isTRUE(none$no_changes))
})

test_that("event buffer + tags header + post events offline path", {
  buf <- EventBuffer$new(capacity = 10L, private_attrs = c("email"))
  ctx <- context_to_list(ld_context("u", email = "a@b.c", name = "N"))
  buf$add_identify(ctx)
  buf$add_custom("click", ctx, list(x = 1), 1.5)
  buf$add_feature("f", ctx, TRUE, FALSE, version = 1L, variation = 1L,
                  reason = list(kind = "FALLTHROUGH"))
  ev <- buf$drain()
  expect_true(length(ev) >= 2)
  expect_null(ev[[1]]$context$email)

  expect_equal(
    launchdarklyServer:::.ld_tags_header_value(list(application_id = "ok", application_version = "1")),
    "application-id/ok application-version/1"
  )
  expect_equal(
    launchdarklyServer:::.ld_tags_header_value(list(application_id = "bad $", application_version = "ok")),
    "application-version/ok"
  )

  # send_events FALSE short-circuits network
  res <- launchdarklyServer:::ld_post_events(list(send_events = FALSE), ev)
  expect_true(res$ok)
})

test_that("context builders and multi-kind cover edge paths", {
  b <- context_builder("k")$kind("org")$name("Acme")$set("plan", "pro")$anonymous(FALSE)
  b$mark_private("plan")
  c <- b$build()
  expect_true(context_valid(c))
  expect_equal(context_get(c, "plan"), "pro")
  expect_equal(context_fully_qualified_key(c), "org:k")
  expect_true("plan" %in% context_private_attributes(c))

  m <- context_multi_builder()$add(c)$add(ld_context("u1"))$build()
  expect_true(context_multiple(m))
  expect_equal(context_individual_count(m), 2L)
  expect_equal(context_get_individual(m, "org")$key, "k")

  j <- context_to_json(c)
  c2 <- context_from_json(j)
  expect_true(context_valid(c2))
  expect_false(context_valid(context_from_list(list(kind = "bad kind!", key = "x"))))
})

test_that("streaming parser + polling helper with webfakes", {
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/sdk/poll", function(req, res) {
    body <- list(events = list(
      list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-full")))),
      list(event = "put-object", data = list(
        kind = "flag", key = "f", version = 1,
        object = list(key = "f", on = TRUE, version = 1, variations = list(FALSE, TRUE),
                      fallthrough = list(variation = 1))
      )),
      list(event = "payload-transferred", data = list(state = "s", version = 1))
    ))
    res$send_json(body, auto_unbox = TRUE)
  })
  app$get("/sdk/stream", function(req, res) {
    body <- paste(
      "event: server-intent",
      "data: {\"payloads\":[{\"intentCode\":\"xfer-full\"}]}",
      "",
      "event: put-object",
      "data: {\"kind\":\"flag\",\"key\":\"sf\",\"version\":1,\"object\":{\"key\":\"sf\",\"on\":true,\"variations\":[false,true],\"fallthrough\":{\"variation\":1},\"version\":1}}",
      "",
      "event: payload-transferred",
      "data: {\"state\":\"stream-s\",\"version\":1}",
      "",
      sep = "\n"
    )
    res$set_header("Content-Type", "text/event-stream")
    res$send(body)
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)

  poll <- launchdarklyServer:::fdv2_poll_once(list(sdk_key = "k", base_uri = server$url(), read_timeout = 5,
                              application_id = "app", application_version = "1"))
  expect_true(poll$ok)

  stream <- suppressWarnings(launchdarklyServer:::fdv2_stream_once(list(
    sdk_key = "k", stream_uri = server$url(), read_timeout = 5
  )))
  expect_true(stream$ok)
  expect_equal(stream$changeset$selector$state, "stream-s")
})
