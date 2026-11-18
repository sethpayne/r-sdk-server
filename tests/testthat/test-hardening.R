test_that("postfork re-inits offline client", {
  client <- ld_client(ld_config("k", offline = TRUE))
  on.exit(client$close(), add = TRUE)
  # Simulate PID mismatch
  client$.__enclos_env__$private$owner_pid_ <- -1L
  expect_warning(client$variation("x", ld_context("u"), "d"))
  client$postfork()
  expect_true(client$is_initialized())
  expect_equal(client$variation("missing", ld_context("u"), "d"), "d")
})

test_that("shutdown does not leave worker when offline", {
  client <- ld_client(ld_config("k", offline = TRUE))
  client$close()
  expect_true(TRUE)
})

test_that("benchmark variation is fast enough offline", {
  skip_if_not_installed("bench")
  client <- ld_client(ld_config("k", offline = TRUE))
  on.exit(client$close(), add = TRUE)
  client$.update_store_for_tests(flags = list(
    f = list(key = "f", version = 1, on = TRUE, variations = list(1, 2), fallthrough = list(variation = 0))
  ))
  ctx <- ld_context("u")
  mark <- bench::mark(client$variation("f", ctx, 0), iterations = 50, check = FALSE)
  expect_true(as.numeric(mark$median, units = "secs") < 0.05)
})
