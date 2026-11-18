# Public API contract: no method throws; bad input yields the default and an
# error reason (server SDKs must never break the host application).

hostile_keys <- list("", NA_character_, NULL, c("a", "b"), 123, list("x"))
hostile_contexts <- list(
  invalid = context_create(""),
  null = NULL,
  string = "user-key",
  list = list(key = "u", kind = "user")
)

exercise_api <- function(client) {
  ctx <- ld_context("u")
  for (k in hostile_keys) {
    expect_no_error(client$variation(k, ctx, "d"))
    expect_equal(client$variation(k, ctx, "d"), "d")
    d <- client$variation_detail(k, ctx, "d")
    expect_equal(d$value, "d")
    expect_equal(d$reason$kind, "ERROR")
  }
  for (nm in names(hostile_contexts)) {
    c <- hostile_contexts[[nm]]
    expect_no_error(client$variation("flag", c, "d"))
    expect_equal(client$variation("flag", c, "d"), "d", label = nm)
    expect_no_error(client$all_flags_state(c))
    expect_false(client$all_flags_state(c)$valid(), label = nm)
    expect_no_error(client$track("evt", c))
    expect_no_error(client$identify(c))
    expect_equal(client$secure_mode_hash(c), "", label = nm)
  }
  expect_no_error(client$track(NA_character_, ctx))
  expect_no_error(client$track(c("a", "b"), ctx))
  expect_no_error(client$track("evt", ctx, data = function() 1))
  expect_no_error(client$variation("flag", ctx, default = function() 1))
  expect_no_error(client$flush())
  expect_no_error(client$is_initialized())
}

test_that("offline client never throws", {
  client <- local_offline_client(list(simple_flag("flag")))
  exercise_api(client)
  expect_equal(client$variation_detail("missing", ld_context("u"), "d")$reason$errorKind,
               "FLAG_NOT_FOUND")
  expect_equal(client$variation_detail("flag", context_create(""), "d")$reason$errorKind,
               "USER_NOT_SPECIFIED")
})

test_that("an uninitialized client returns defaults with CLIENT_NOT_READY", {
  skip_on_cran()
  client <- local_ld_client("k", base_uri = "http://127.0.0.1:1", stream = FALSE,
                            send_events = FALSE, start_wait = 0.3)
  expect_false(client$is_initialized())
  expect_equal(client$variation_detail("flag", ld_context("u"), "d")$reason$errorKind,
               "CLIENT_NOT_READY")
  exercise_api(client)
})

test_that("a closed client never throws", {
  client <- local_offline_client(list(simple_flag("flag")))
  client$close()
  expect_no_error(client$close())
  exercise_api(client)
})

test_that("evaluation errors surface as EXCEPTION, not as R errors", {
  client <- local_offline_client(list(simple_flag("flag")))
  client$add_hook(LdHook$new())
  local_mocked_bindings(ld_evaluate_flag = function(...) stop("boom"))
  d <- expect_no_error(client$variation_detail("flag", ld_context("u"), "d"))
  expect_equal(d$value, "d")
  expect_equal(d$reason$errorKind, "EXCEPTION")
})

test_that("all_flags_state is invalid, not an error, when evaluation fails", {
  client <- local_offline_client(list(simple_flag("flag")))
  local_mocked_bindings(ld_evaluate_flag = function(...) stop("boom"))
  st <- expect_no_error(client$all_flags_state(ld_context("u")))
  expect_false(st$valid())
})

test_that("hooks observe evaluations but cannot replace the context (S7)", {
  client <- local_offline_client(list(c(simple_flag("flag"), list(
    targets = list(list(values = list("alice"), variation = 0L))
  ))))
  Swap <- R6::R6Class("Swap", inherit = LdHook, public = list(
    seen = NULL,
    before_evaluation = function(series_context) {
      series_context$context <- ld_context("alice")
      series_context
    },
    after_evaluation = function(series_context, detail) {
      self$seen <- detail$reason$kind
      detail
    }
  ))
  hook <- Swap$new("swap")
  client$add_hook(hook)
  expect_true(client$variation("flag", ld_context("bob"), "d"))
  expect_equal(hook$seen, "FALLTHROUGH")
})

test_that("hook errors are isolated from the evaluation", {
  client <- local_offline_client(list(simple_flag("flag")))
  Bad <- R6::R6Class("Bad", inherit = LdHook, public = list(
    before_evaluation = function(series_context) stop("before"),
    after_evaluation = function(series_context, detail) stop("after")
  ))
  client$add_hook(Bad$new())
  expect_true(client$variation("flag", ld_context("u"), FALSE))
})

test_that("ld_config warns on unknown or mistyped options and keeps defaults", {
  expect_warning(cfg <- ld_config("k", poll_intervall = 5), "Unknown config option")
  expect_warning(cfg <- ld_config("k", poll_interval = "fast"), "non-negative number")
  expect_equal(cfg$poll_interval, 30)
  expect_warning(cfg <- ld_config("k", stream = "yes"), "TRUE or FALSE")
  expect_true(cfg$stream)
  expect_warning(ld_config("k", to_list = 1), "Unknown config option")
  expect_no_warning(cfg <- ld_config("k", poll_interval = 0.5, offline = TRUE))
  expect_equal(cfg$poll_interval, 0.5)
})

test_that("config to_list carries every option the worker reads", {
  worker_src <- readLines(system.file("worker", "run.R", package = "launchdarklyServer"))
  worker_src <- sub("#.*$", "", worker_src)
  used <- unique(unlist(regmatches(worker_src, gregexpr("config\\$[a-z_]+", worker_src))))
  used <- sub("config\\$", "", used)
  sent <- names(ld_config("k")$to_list())
  expect_equal(setdiff(used, sent), character())
})
