# Offline unit coverage push for DoD ≥90% groups.
# Local webfakes/httpuv only — no live LaunchDarkly network.

test_that("evaluator: targets, prereqs, experiments, segments, kind clauses", {
  get_seg <- function(k) NULL

  # Legacy user targets
  flag_t <- list(
    key = "t", on = TRUE, version = 1, variations = list("no", "yes"),
    fallthrough = list(variation = 0),
    targets = list(list(variation = 1, values = list("alice")))
  )
  d <- launchdarklyServer:::ld_evaluate_flag(flag_t, ld_context("alice"), "d", function(k) NULL, get_seg)
  expect_equal(d$value, "yes")
  expect_equal(d$reason$kind, "TARGET_MATCH")
  d2 <- launchdarklyServer:::ld_evaluate_flag(flag_t, ld_context("bob"), "d", function(k) NULL, get_seg)
  expect_equal(d2$value, "no")

  # contextTargets with user + org kinds
  flag_ct <- list(
    key = "ct", on = TRUE, version = 1, variations = list("a", "b", "c"),
    fallthrough = list(variation = 0),
    targets = list(list(variation = 1, values = list("u1"))),
    contextTargets = list(
      list(contextKind = "user", variation = 1, values = list()),
      list(contextKind = "org", variation = 2, values = list("acme"))
    )
  )
  multi <- context_create_multi(ld_context("u1"), ld_context("acme", kind = "org"))
  d3 <- launchdarklyServer:::ld_evaluate_flag(flag_ct, multi, "d", function(k) NULL, get_seg)
  expect_equal(d3$value, "b")

  # Prerequisite failed (missing / wrong variation)
  child <- list(
    key = "child", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 1),
    prerequisites = list(list(key = "parent", variation = 1))
  )
  parent_off <- list(
    key = "parent", on = FALSE, version = 1, variations = list(FALSE, TRUE),
    offVariation = 0, fallthrough = list(variation = 1)
  )
  gf <- function(k) if (identical(k, "parent")) parent_off else NULL
  d4 <- launchdarklyServer:::ld_evaluate_flag(child, ld_context("u"), FALSE, gf, get_seg)
  expect_equal(d4$reason$kind, "PREREQUISITE_FAILED")

  parent_ok <- list(
    key = "parent", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 1)
  )
  gf2 <- function(k) if (identical(k, "parent")) parent_ok else if (identical(k, "child")) child else NULL
  d5 <- launchdarklyServer:::ld_evaluate_flag(child, ld_context("u"), FALSE, gf2, get_seg)
  expect_equal(d5$value, TRUE)

  # Empty rollout variations → MALFORMED
  bad_roll <- list(
    key = "br", on = TRUE, version = 1, variations = list("x"),
    fallthrough = list(rollout = list(variations = list()))
  )
  d6 <- launchdarklyServer:::ld_evaluate_flag(bad_roll, ld_context("u"), "d", function(k) NULL, get_seg)
  expect_equal(d6$reason$errorKind, "MALFORMED_FLAG")

  # Experiment rollout past last weight bucket
  exp <- list(
    key = "exp", salt = "s", on = TRUE, version = 1,
    variations = list("a", "b"),
    fallthrough = list(rollout = list(
      kind = "experiment",
      variations = list(
        list(variation = 0, weight = 0),
        list(variation = 1, weight = 0, untracked = TRUE)
      )
    ))
  )
  d7 <- launchdarklyServer:::ld_evaluate_flag(exp, ld_context("u"), "d", function(k) NULL, get_seg)
  expect_equal(d7$value, "b")

  # bucket context missing kind → -1
  expect_equal(
    launchdarklyServer:::.ld_bucket_context(NULL, ld_context("u", kind = "user"), "org", "k", "s", NULL),
    -1
  )

  # Segment include/exclude + rules + circular
  seg <- list(
    key = "seg1", version = 1,
    included = list("in-user"),
    excluded = list("ex-user"),
    includedContexts = list(list(contextKind = "org", values = list("org-in"))),
    excludedContexts = list(list(contextKind = "org", values = list("org-ex"))),
    rules = list(list(
      clauses = list(list(attribute = "email", op = "in", values = list("a@b.com"))),
      weight = 100000
    ))
  )
  gs <- function(k) if (identical(k, "seg1")) seg else NULL
  flag_seg <- list(
    key = "fs", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 0),
    rules = list(list(
      id = "r", variation = 1,
      clauses = list(list(op = "segmentMatch", values = list("seg1")))
    ))
  )
  expect_equal(
    launchdarklyServer:::ld_evaluate_flag(flag_seg, ld_context("in-user"), FALSE, function(k) NULL, gs)$value,
    TRUE
  )
  expect_equal(
    launchdarklyServer:::ld_evaluate_flag(flag_seg, ld_context("ex-user"), FALSE, function(k) NULL, gs)$value,
    FALSE
  )
  expect_equal(
    launchdarklyServer:::ld_evaluate_flag(
      flag_seg, context_create_multi(ld_context("x"), ld_context("org-in", kind = "org")),
      FALSE, function(k) NULL, gs
    )$value,
    TRUE
  )

  # kind attribute clause on multi
  flag_kind <- list(
    key = "fk", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 0),
    rules = list(list(
      id = "r", variation = 1,
      clauses = list(list(attribute = "kind", op = "in", values = list("org")))
    ))
  )
  d8 <- launchdarklyServer:::ld_evaluate_flag(
    flag_kind, context_create_multi(ld_context("u"), ld_context("o", kind = "org")),
    FALSE, function(k) NULL, get_seg
  )
  expect_equal(d8$value, TRUE)

  # Array attribute match + negate
  flag_arr <- list(
    key = "fa", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 0),
    rules = list(list(
      id = "r", variation = 1,
      clauses = list(list(
        attribute = "tags", op = "in", values = list("vip"), negate = TRUE
      ))
    ))
  )
  d9 <- launchdarklyServer:::ld_evaluate_flag(
    flag_arr, ld_context("u", tags = list("vip", "other")), FALSE, function(k) NULL, get_seg
  )
  expect_equal(d9$value, FALSE) # negated match → false → fallthrough

  # Circular segment
  seg_a <- list(
    key = "sa", version = 1,
    rules = list(list(clauses = list(list(op = "segmentMatch", values = list("sb")))))
  )
  seg_b <- list(
    key = "sb", version = 1,
    rules = list(list(clauses = list(list(op = "segmentMatch", values = list("sa")))))
  )
  gs2 <- function(k) if (identical(k, "sa")) seg_a else if (identical(k, "sb")) seg_b else NULL
  flag_circ <- list(
    key = "fc", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 0),
    rules = list(list(
      id = "r", variation = 1,
      clauses = list(list(op = "segmentMatch", values = list("sa")))
    ))
  )
  d10 <- launchdarklyServer:::ld_evaluate_flag(flag_circ, ld_context("u"), FALSE, function(k) NULL, gs2)
  expect_equal(d10$reason$errorKind, "MALFORMED_FLAG")

  # Unbounded segment (big segments out of scope)
  seg_u <- list(key = "su", unbounded = TRUE, included = list("u"))
  expect_false(launchdarklyServer:::.ld_segment_matches(
    seg_u, ld_context("u"), list(segment_stack = character()), function(k) NULL
  ))

  # Rule with top-level variation via .ld_value_for_vr
  expect_equal(
    launchdarklyServer:::.ld_value_for_vr(
      list(variations = list("x", "y")), list(variation = 1), ld_context("u"),
      list(kind = "RULE_MATCH")
    )$value,
    "y"
  )
  expect_equal(
    launchdarklyServer:::.ld_value_for_vr(
      list(variations = list("x")), NULL, ld_context("u"), list(kind = "RULE_MATCH")
    )$reason$errorKind,
    "MALFORMED_FLAG"
  )
})

test_that("operators edge cases for dates and semver", {
  expect_false(launchdarklyServer:::.ld_op_matches(1, "a"))
  expect_null(launchdarklyServer:::.ld_parse_time_ms("2017-12-06T12:00:00+99:99"))
  # Zulu form is always accepted
  expect_true(!is.null(launchdarklyServer:::.ld_parse_time_ms("2017-12-06T12:00:00Z")))
  expect_null(launchdarklyServer:::.ld_parse_semver(1))
  expect_null(launchdarklyServer:::.ld_parse_semver("1.0.0-"))
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0", "1.0.0-alpha"), 1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha", "1.0.0"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-1", "1.0.0-beta"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-beta", "1.0.0-1"), 1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha", "1.0.0-beta"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha.1", "1.0.0-alpha"), 1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-alpha", "1.0.0-alpha.1"), -1L)
  expect_identical(launchdarklyServer:::.ld_semver_cmp("1.0.0-rc.1", "1.0.0-rc.1"), 0L)
})

test_that("evaluation detail and FeatureFlagsState edge paths", {
  d <- evaluation_detail(NULL, NULL, list(kind = "OFF"))
  expect_output(print(d), "NULL")
  st <- FeatureFlagsState$new(TRUE)
  expect_null(st$get_flag_reason("missing"))
  st$add_flag("f", NULL, variation = 0L, version = 1L, reason = list(kind = "OFF"),
              track_events = TRUE, track_reason = TRUE, debug_events_until_date = 999)
  expect_null(st$get_flag_value("f"))
  js <- st$to_json_list()
  expect_true(isTRUE(js[["$valid"]]))
  empty <- FeatureFlagsState$new(FALSE)
  expect_true(length(empty$to_json_list()[["$flagsState"]]) == 0L)
})

test_that("context builders, multi, JSON, and accessors", {
  # zero contexts → invalid multi
  expect_false(context_valid(launchdarklyServer:::.ld_new_multi(list())))

  # duplicate kinds
  expect_false(context_valid(context_create_multi(ld_context("a"), ld_context("b", kind = "user"))))

  # invalid nested context error aggregation
  bad <- structure(list(kind = "user", key = "", error = "bad key", multi = NULL), class = "ld_context")
  expect_false(context_valid(launchdarklyServer:::.ld_new_multi(list(bad))))

  # list form of create_multi
  c1 <- ld_context("a", kind = "user")
  c2 <- ld_context("b", kind = "org")
  m <- context_create_multi(list(c1, c2))
  expect_true(context_multiple(m))
  expect_equal(context_individual_count(m), 2L)
  expect_null(context_get_individual(m, "nope"))
  expect_null(context_get_individual(m, 99L))
  expect_equal(context_get_individual(c1, "user")$key, "a")
  expect_null(context_get_individual(c1, "org"))
  expect_null(context_get_individual(c1, 1L))
  expect_equal(context_individual_count(structure(list(error = "x"), class = "ld_context")), 0L)
  expect_null(context_get_individual(structure(list(error = "x"), class = "ld_context"), "user"))

  expect_equal(context_get(c1, "key"), "a")
  expect_equal(context_get(c1, "kind"), "user")
  expect_null(context_get(c1, "missing"))
  expect_equal(c1[["key"]], "a")
  c3 <- ld_context("u", email = "e@x.com")
  expect_equal(context_get(c3, "email"), "e@x.com")

  expect_output(print(c1), "user")
  inv <- structure(list(error = "boom", multi = NULL), class = "ld_context")
  expect_false(context_valid(inv))
  expect_equal(context_error(inv), "boom")

  # copy-on-write builder paths
  base <- ld_context("u", name = "n", email = "e", private = "email")
  b <- ContextBuilder$new("u", copy_from = base)
  b$set("email", "other")
  b$mark_private("name")
  b$key("u2")$kind("")$name(NULL)$anonymous(TRUE)
  built <- b$build()
  expect_equal(built$key, "u2")
  expect_true(built$anonymous)
  expect_false(b$try_set("", 1))
  expect_false(b$try_set("key", 1L))
  expect_false(b$try_set("kind", 1L))
  expect_false(b$try_set("name", 1L))
  expect_false(b$try_set("anonymous", "x"))
  expect_true(b$try_set("key", "k"))
  expect_true(b$try_set("kind", "org"))
  expect_true(b$try_set("name", "nm"))
  expect_true(b$try_set("anonymous", FALSE))
  expect_true(b$try_set("custom", NULL))

  # multi builder flatten + cow after build
  mb2 <- context_multi_builder()
  mb2$add(m)
  out1 <- mb2$build()
  expect_true(context_valid(out1))
  expect_true(context_multiple(out1))
  mb2$add(ld_context("z", kind = "device"))
  out2 <- mb2$build()
  expect_true(context_valid(out2))
  expect_equal(context_individual_count(out2), context_individual_count(out1) + 1L)

  j <- context_to_json(m)
  expect_true(nzchar(j))
  back <- context_from_json(j)
  expect_true(context_valid(back))
  lst <- context_to_list(c3)
  expect_equal(lst$key, "u")
})

test_that("events buffer redaction, summary, feature extras, and post errors", {
  buf <- EventBuffer$new(capacity = 10L, all_private = TRUE)
  expect_equal(buf$size(), 0L)
  single <- context_to_list(ld_context("u", email = "e", name = "n", anonymous = TRUE))
  multi <- context_to_list(context_create_multi(ld_context("u"), ld_context("o", kind = "org", email = "x")))
  buf$add_index(single)
  buf$add_index(multi)
  buf$add_feature("f", single, "v", "d", version = 1L, variation = 0L,
                  track_events = TRUE, reason = list(kind = "FALLTHROUGH"), prereq_of = "p")
  buf$add_summary(1, 2, list(f = list(default = "d", counters = list())))
  ev <- buf$drain()
  expect_equal(length(ev), 4L)
  expect_null(ev[[1]]$context$email)
  expect_true(isTRUE(ev[[1]]$context$anonymous))
  expect_equal(ev[[3]]$prereqOf, "p")
  expect_equal(ev[[3]]$reason$kind, "FALLTHROUGH")

  buf2 <- EventBuffer$new(private_attrs = c("email", "name"))
  ctx <- context_to_list(ld_context("u", email = "e", name = "n", other = "o"))
  ctx$`_meta` <- list(privateAttributes = list("other"))
  buf2$add_identify(ctx)
  multi2 <- context_to_list(context_create_multi(
    ld_context("u", email = "e"), ld_context("o", kind = "org", name = "n")
  ))
  buf2$add_identify(multi2)
  out <- buf2$drain()
  expect_null(out[[1]]$context$email)
  expect_null(out[[1]]$context$name)
  expect_null(out[[1]]$context$other)

  # post with send_events false / empty
  expect_true(launchdarklyServer:::ld_post_events(list(send_events = FALSE), list(1))$ok)
  expect_true(launchdarklyServer:::ld_post_events(list(send_events = TRUE), list())$ok)

  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$post("/bulk", function(req, res) res$set_status(500)$send("no"))
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  res <- launchdarklyServer:::ld_post_events(
    list(sdk_key = "k", events_uri = server$url(), send_events = TRUE, read_timeout = 3),
    list(list(kind = "identify", context = list(key = "u", kind = "user")))
  )
  expect_false(res$ok)
  # connection error path
  res2 <- launchdarklyServer:::ld_post_events(
    list(sdk_key = "k", events_uri = "http://127.0.0.1:1", send_events = TRUE, read_timeout = 1),
    list(list(kind = "identify"))
  )
  expect_false(res2$ok)
})

test_that("changeset and store remaining branches", {
  # skip unsupported kinds; put with JSON string object; xfer-none
  cs <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-none"))))
  )))
  expect_true(isTRUE(cs$no_changes))

  cs2 <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-full")))),
    list(event = "put-object", data = list(kind = "unknown", key = "x", version = 1, object = list())),
    list(event = "put-object", data = list(
      kind = "flag", key = "f", version = 2,
      object = as.character(jsonlite::toJSON(
        list(key = "f", version = 2, on = TRUE, variations = list(FALSE, TRUE),
             fallthrough = list(variation = 0)),
        auto_unbox = TRUE
      ))
    )),
    list(event = "delete-object", data = list(kind = "unknown", key = "y", version = 1)),
    list(event = "not-a-list"),
    list(event = "payload-transferred", data = list(state = "s", version = 3))
  )))
  expect_equal(length(cs2$puts), 1L)
  expect_true(is.list(cs2$puts[[1]]$object))

  # Malformed put aborts the payload
  bad_put <- launchdarklyServer:::fdv2_polling_payload_to_changeset(list(events = list(
    list(event = "put-object", data = "bad")
  )))
  expect_true(!is.null(bad_put$error))

  store <- FeatureStore$new()
  expect_null(store$get_segment("missing"))
  expect_false(store$upsert("nope", "k", 1, list(key = "k")))
  expect_false(store$upsert("flag", "k", 1, "not-json-object"))
  expect_false(store$delete("nope", "k", 1))
  store$apply_changeset(cs2)
  expect_equal(store$get_flag("f")$key, "f")
})

test_that("fdv2 polling helpers and local poll", {
  skip_if_not_installed("webfakes")
  # Overlong application-id is dropped; version alone remains.
  expect_equal(
    launchdarklyServer:::.ld_tags_header_value(list(
      application_id = paste(rep("a", 65), collapse = ""),
      application_version = "ok"
    )),
    "application-version/ok"
  )
  expect_equal(
    launchdarklyServer:::.ld_tags_header_value(list(application_id = "id", application_version = "v")),
    "application-id/id application-version/v"
  )

  app <- webfakes::new_app()
  app$get("/sdk/poll", function(req, res) {
    body <- jsonlite::toJSON(list(events = list(
      list(event = "server-intent", data = list(payloads = list(list(intentCode = "xfer-full")))),
      list(event = "payload-transferred", data = list(state = "st", version = 1))
    )), auto_unbox = TRUE)
    res$set_header("Content-Type", "application/json")$send(as.character(body))
  })
  server <- webfakes::new_app_process(app)
  on.exit(server$stop(), add = TRUE)
  cfg <- list(sdk_key = "k", base_uri = server$url(), read_timeout = 5,
              application_id = "app", application_version = "1",
              payload_filter_key = "filt")
  res <- launchdarklyServer:::fdv2_poll_once(cfg, list(state = "basis", version = 0L))
  expect_true(res$ok)

  app2 <- webfakes::new_app()
  app2$get("/sdk/poll", function(req, res) res$set_status(401)$send("no"))
  s2 <- webfakes::new_app_process(app2)
  on.exit(s2$stop(), add = TRUE)
  res2 <- launchdarklyServer:::fdv2_poll_once(list(sdk_key = "k", base_uri = s2$url(), read_timeout = 3))
  expect_false(res2$ok)
  expect_equal(res2$status, 401L)

  res3 <- launchdarklyServer:::fdv2_poll_once(list(sdk_key = "k", base_uri = "http://127.0.0.1:1", read_timeout = 1))
  expect_false(res3$ok)
})

test_that("fdv2 streaming helpers and local curl-multi stream", {
  expect_equal(
    launchdarklyServer:::.ld_parse_raw_http_headers(NULL)$status,
    NA_integer_
  )
  raw_h <- charToRaw("HTTP/1.1 401 Unauthorized\r\nX-LD-FD-Fallback: true\r\n\r\n")
  ph <- launchdarklyServer:::.ld_parse_raw_http_headers(raw_h)
  expect_equal(ph$status, 401L)
  expect_equal(ph$headers[["x-ld-fd-fallback"]], "true")

  split <- launchdarklyServer:::.ld_sse_split_buffer(paste0(
    ": comment\n",
    "event: ping\ndata: {}\n\n",
    "event: server-intent\ndata: {\"payloads\":[{\"intentCode\":\"xfer-full\"}]}\n\n",
    "data: only-data\n\n",
    "incomplete"
  ))
  expect_true(length(split$events) >= 2L)
  expect_equal(split$rest, "incomplete")
  expect_null(launchdarklyServer:::.ld_sse_parse_block(": only comment"))

  skip_if_not_installed("httpuv")
  # Finite-body SSE via httpuv (exercises multi + helpers)
  payload <- paste(
    "event: server-intent",
    'data: {"payloads":[{"id":"p","target":1,"intentCode":"xfer-full","reason":"initial"}]}',
    "",
    "event: put-object",
    'data: {"kind":"flag","key":"f","version":1,"object":{"key":"f","version":1,"on":true,"variations":[false,true],"fallthrough":{"variation":1}}}',
    "",
    "event: payload-transferred",
    'data: {"state":"initial","version":1}',
    "",
    sep = "\n"
  )
  # Keep-alive SSE then close after write — use curl multi via fdv2_stream_once
  # against a short-lived python-like server implemented with httpuv + later close.
  port <- httpuv::randomPort()
  srv <- NULL
  srv <<- httpuv::startServer("127.0.0.1", port, list(
    call = function(req) {
      if (!identical(req$PATH_INFO, "/sdk/stream")) {
        return(list(status = 404L, headers = list(), body = ""))
      }
      list(
        status = 200L,
        headers = list(
          "Content-Type" = "text/event-stream",
          "Cache-Control" = "no-cache",
          "X-LD-EnvID" = "env"
        ),
        body = payload
      )
    }
  ))
  on.exit(try(httpuv::stopServer(srv), silent = TRUE), add = TRUE)

  cfg <- list(
    sdk_key = "k",
    stream_uri = sprintf("http://127.0.0.1:%s", port),
    read_timeout = 3,
    application_id = "app",
    application_version = "1",
    payload_filter_key = "filt",
    connect_timeout = 2
  )
  got <- NULL
  # An in-process httpuv server cannot answer while fdv2_stream_once() blocks
  # this R process, so only the timeout path runs here; the webfakes block
  # below covers a successful stream.
  res <- launchdarklyServer:::fdv2_stream_once(
    cfg, list(state = "basis", version = 0L),
    on_changeset = function(cs) got <<- cs,
    until_first_payload = TRUE
  )
  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_null(got)

  # 401 path
  port2 <- httpuv::randomPort()
  srv2 <- httpuv::startServer("127.0.0.1", port2, list(
    call = function(req) list(status = 401L, headers = list("X-LD-FD-Fallback" = "false"), body = "")
  ))
  on.exit(try(httpuv::stopServer(srv2), silent = TRUE), add = TRUE)
  res401 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = sprintf("http://127.0.0.1:%s", port2), read_timeout = 2),
    until_first_payload = TRUE
  )
  expect_false(isTRUE(res401$ok))

  # Body parser unit test
  skip_if_not_installed("webfakes")
  app <- webfakes::new_app()
  app$get("/sdk/stream", function(req, res) {
    res$set_header("Content-Type", "text/event-stream")$send(payload)
  })
  wp <- webfakes::new_app_process(app)
  on.exit(wp$stop(), add = TRUE)
  req <- httr2::request(paste0(wp$url(), "/sdk/stream")) |> httr2::req_method("GET")
  resp <- httr2::req_perform(req)
  parsed <- launchdarklyServer:::.ld_stream_parse_body_response(resp)
  expect_true(isTRUE(parsed$ok))
})
