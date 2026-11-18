# Analytics events: EventBuffer semantics and end-to-end delivery (D5).

eval_rec <- function(flag, ctx = ld_context("u"), default = FALSE, with_reasons = FALSE, ts = 1000,
                     prereq_of = NULL) {
  detail <- ld_evaluate_flag(flag, ctx, default, function(k) NULL, function(k) NULL)
  .ld_eval_event_record(flag$key, ctx, flag, detail, default, with_reasons, prereq_of, ts)
}

kinds_of <- function(events) vapply(events, function(e) e$kind, character(1))

test_that("an evaluation without tracking yields an index and a summary only", {
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(simple_flag("f", version = 7L)))
  out <- buf$payload()
  expect_equal(kinds_of(out), c("index", "summary"))
  s <- out[[2]]
  expect_equal(s$startDate, 1000)
  expect_equal(s$features$f$default, FALSE)
  expect_equal(s$features$f$contextKinds, list("user"))
  expect_equal(s$features$f$counters,
               list(list(count = 1L, value = TRUE, variation = 1L, version = 7L)))
  expect_equal(buf$payload(), list())
})

test_that("trackEvents produces a full feature event", {
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(simple_flag("f", trackEvents = TRUE), with_reasons = TRUE))
  fe <- Filter(function(e) e$kind == "feature", buf$payload())
  expect_length(fe, 1)
  expect_equal(fe[[1]]$key, "f")
  expect_true(fe[[1]]$value)
  expect_equal(fe[[1]]$variation, 1L)
  expect_equal(fe[[1]]$version, 1L)
  expect_equal(fe[[1]]$reason$kind, "FALLTHROUGH")
  expect_equal(fe[[1]]$context, list(key = "u", kind = "user"))
})

test_that("experiments emit feature events with reasons even without trackEvents", {
  flag <- simple_flag("f", trackEventsFallthrough = TRUE)
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(flag, with_reasons = FALSE))
  fe <- Filter(function(e) e$kind == "feature", buf$payload())
  expect_length(fe, 1)
  expect_equal(fe[[1]]$reason$kind, "FALLTHROUGH")
})

test_that("debug events are emitted only while debugEventsUntilDate is in the future", {
  now <- as.numeric(Sys.time()) * 1000
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(simple_flag("f", debugEventsUntilDate = now + 60000)))
  buf$process_eval(eval_rec(simple_flag("g", debugEventsUntilDate = now - 60000)))
  out <- buf$payload()
  dbg <- Filter(function(e) e$kind == "debug", out)
  expect_length(dbg, 1)
  expect_equal(dbg[[1]]$key, "f")
})

test_that("unknown flags are summarized with unknown = TRUE", {
  buf <- EventBuffer$new()
  detail <- list(value = "d", reason = list(kind = "ERROR", errorKind = "FLAG_NOT_FOUND"))
  buf$process_eval(.ld_eval_event_record("missing", ld_context("u"), NULL, detail, "d", FALSE,
                                         timestamp = 5))
  s <- buf$payload()[[2]]
  expect_equal(s$features$missing$counters, list(list(count = 1L, value = "d", unknown = TRUE)))
})

test_that("summary counters split by variation and version and count repeats", {
  buf <- EventBuffer$new()
  f1 <- simple_flag("f", version = 1L)
  for (i in 1:3) buf$process_eval(eval_rec(f1, ts = 1000 + i))
  buf$process_eval(eval_rec(simple_flag("f", version = 2L), ts = 2000))
  buf$process_eval(eval_rec(c(f1[setdiff(names(f1), "on")], list(on = FALSE)), ts = 500))
  s <- Filter(function(e) e$kind == "summary", buf$payload())[[1]]
  expect_equal(s$startDate, 500)
  expect_equal(s$endDate, 2000)
  counts <- vapply(s$features$f$counters, function(c) c$count, integer(1))
  expect_setequal(counts, c(3L, 1L, 1L))
})

test_that("index events are deduplicated per context and identify suppresses them", {
  buf <- EventBuffer$new()
  buf$process_identify(context_to_list(ld_context("a")), creation_date = 1)
  buf$process_eval(eval_rec(simple_flag("f"), ld_context("a")))
  buf$process_eval(eval_rec(simple_flag("f"), ld_context("b")))
  buf$process_eval(eval_rec(simple_flag("f"), ld_context("b")))
  buf$process_custom("evt", context_to_list(ld_context("c")), creation_date = 2)
  expect_equal(kinds_of(buf$payload()), c("identify", "index", "index", "custom", "summary"))
  buf$reset_context_keys()
  buf$process_eval(eval_rec(simple_flag("f"), ld_context("b")))
  expect_equal(kinds_of(buf$payload()), c("index", "summary"))
})

test_that("prerequisite evaluations carry prereqOf", {
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(simple_flag("p", trackEvents = TRUE), prereq_of = "f"))
  fe <- Filter(function(e) e$kind == "feature", buf$payload())
  expect_equal(fe[[1]]$prereqOf, "f")
})

test_that("excludeFromSummaries flags are not summarized", {
  buf <- EventBuffer$new()
  buf$process_eval(eval_rec(simple_flag("f", excludeFromSummaries = TRUE)))
  expect_equal(kinds_of(buf$payload()), "index")
})

test_that("the buffer drops new events at capacity and counts them", {
  buf <- EventBuffer$new(capacity = 2L)
  for (i in 1:5) buf$add_custom(paste0("e", i), context_to_list(ld_context("u")), creation_date = i)
  expect_equal(buf$size(), 2L)
  expect_equal(buf$dropped_count(), 3L)
  expect_equal(vapply(buf$drain(), function(e) e$key, character(1)), c("e1", "e2"))
})

test_that("omit_anonymous_contexts drops anonymous identify and index events", {
  buf <- EventBuffer$new(omit_anonymous_contexts = TRUE)
  anon <- ld_context("anon", anonymous = TRUE)
  buf$process_identify(context_to_list(anon), creation_date = 1)
  buf$process_eval(eval_rec(simple_flag("f"), anon))
  expect_equal(kinds_of(buf$payload()), "summary")
})

test_that("variation, identify and track reach /bulk end to end (D5)", {
  mock <- local_mock_ld()
  flags <- list(simple_flag("tracked", trackEvents = TRUE), simple_flag("plain"))
  mock$script("/sdk/poll", poll_ok(flags))
  client <- local_polling_client(mock)
  expect_true(client$is_initialized())
  ctx <- ld_context("u", email = "e@x", private = "email")
  expect_true(client$variation("tracked", ctx, FALSE))
  expect_true(client$variation("plain", ctx, FALSE))
  expect_equal(client$variation("missing", ctx, "d"), "d")
  client$identify(ld_context("other"))
  client$track("purchase", ctx, data = list(sku = "x"), metric_value = 9.5)
  client$flush()
  expect_true(wait_for(function() length(bulk_events(mock)) > 0))
  ev <- bulk_events(mock)
  expect_setequal(unique(kinds_of(ev)), c("index", "feature", "identify", "custom", "summary"))
  fe <- Filter(function(e) e$kind == "feature", ev)
  expect_equal(vapply(fe, function(e) e$key, character(1)), "tracked")
  expect_null(fe[[1]]$context$email)
  expect_equal(fe[[1]]$context[["_meta"]]$redactedAttributes, list("email"))
  s <- Filter(function(e) e$kind == "summary", ev)[[1]]
  expect_setequal(names(s$features), c("tracked", "plain", "missing"))
  cu <- Filter(function(e) e$kind == "custom", ev)[[1]]
  expect_equal(cu$data, list(sku = "x"))
  expect_equal(cu$metricValue, 9.5)
  r <- mock$requests("/bulk")[[1]]
  expect_false(is.null(req_header(r, "X-LaunchDarkly-Payload-ID")))
})

test_that("an unrecoverable /bulk response disables further event posts", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  mock$script("/bulk", list(status = 401L))
  client <- local_polling_client(mock)
  client$identify(ld_context("u"))
  suppressMessages(client$flush())
  expect_true(wait_for(function() length(mock$requests("/bulk")) >= 1))
  client$identify(ld_context("v"))
  client$flush()
  Sys.sleep(1)
  suppressMessages(client$is_initialized())
  expect_length(mock$requests("/bulk"), 1)
})

test_that("send_events = FALSE sends nothing", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock, send_events = FALSE)
  client$variation("f", ld_context("u"), FALSE)
  client$identify(ld_context("u"))
  client$flush()
  Sys.sleep(0.7)
  expect_length(mock$requests("/bulk"), 0)
})

test_that("close() flushes pending events", {
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock)
  client$identify(ld_context("u"))
  client$close()
  expect_true(wait_for(function() length(bulk_events(mock)) >= 1, timeout = 5))
  expect_equal(bulk_events(mock)[[1]]$kind, "identify")
})
