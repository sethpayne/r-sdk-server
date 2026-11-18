# Fork safety (S2): a forked child must not use the parent's worker pipe.
#
# processx's SIGCHLD handler can reap mcparallel children before parallel does,
# so R may print "Error while shutting down parallel" at exit; it is harmless.

skip_if_no_fork <- function() {
  skip_on_os("windows")
  skip_on_cran()
  skip_if_not(capabilities("fifo"))
}

test_that("a forked child gets defaults and never writes to the parent's worker", {
  skip_if_no_fork()
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f", trackEvents = TRUE))))
  client <- local_polling_client(mock)
  expect_true(client$variation("f", ld_context("u"), FALSE))
  w <- worker_of(client)
  pid <- w$pid()

  job <- parallel::mcparallel({
    warned <- FALSE
    val <- withCallingHandlers(
      client$variation_detail("f", ld_context("child"), "d"),
      warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
    )
    suppressWarnings({
      client$track("child-event", ld_context("child"))
      client$identify(ld_context("child"))
      client$flush()
    })
    list(value = val$value, kind = val$reason$errorKind, warned = warned,
         pending = worker_of(client)$pending_bytes())
  })
  res <- parallel::mccollect(job, wait = TRUE, timeout = 30)[[1]]
  expect_equal(res$value, "d")
  expect_equal(res$kind, "CLIENT_NOT_READY")
  expect_true(res$warned)
  expect_equal(res$pending, 0)
  expect_true(process_alive(pid))

  client$flush()
  expect_true(wait_for(function() length(bulk_events(mock)) > 0))
  keys <- unlist(lapply(bulk_events(mock), function(e) c(e$key, e$context$key)))
  expect_false("child" %in% keys)
  expect_false("child-event" %in% keys)
})

test_that("postfork() gives the child its own worker and leaves the parent's alone", {
  skip_if_no_fork()
  mock <- local_mock_ld()
  mock$script("/sdk/poll", poll_ok(list(simple_flag("f"))))
  client <- local_polling_client(mock, send_events = FALSE)
  parent_pid <- worker_of(client)$pid()

  job <- parallel::mcparallel({
    client$postfork()
    out <- list(value = client$variation("f", ld_context("u"), FALSE),
                worker = worker_of(client)$pid())
    client$close()
    out
  })
  res <- parallel::mccollect(job, wait = TRUE, timeout = 60)[[1]]
  expect_true(res$value)
  expect_false(identical(res$worker, parent_pid))
  expect_true(process_alive(parent_pid))
  expect_true(client$variation("f", ld_context("u"), FALSE))
})
