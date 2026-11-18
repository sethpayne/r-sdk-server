# Offline curl-multi streaming coverage via local Python SSE servers.

test_that("fdv2_stream_once covers ok/multi/goodbye/error/badjson/401/403", {
  # Local loopback only (no live LD). webfakes subprocess avoids same-process
  # httpuv deadlock and removes the Python sidecar that broke on macOS CI.
  skip_if_not_installed("webfakes")

  # Success + tags + basis + filter
  s <- .ld_start_local_sse("ok")
  on.exit(s$stop(), add = TRUE)
  cs <- NULL
  res <- launchdarklyServer:::fdv2_stream_once(
    list(
      sdk_key = "k", stream_uri = s$url, read_timeout = 3,
      application_id = "app", application_version = "1",
      payload_filter_key = "filt", connect_timeout = 2
    ),
    selector = list(state = "basis", version = 0L),
    on_changeset = function(x) cs <<- x,
    until_first_payload = TRUE
  )
  expect_true(isTRUE(res$ok))
  expect_equal(cs$selector$state, "initial")
  s$stop()

  # Multi-payload with until_first_payload = FALSE + should_stop after 2
  s2 <- .ld_start_local_sse("multi")
  on.exit(s2$stop(), add = TRUE)
  n <- 0L
  res2 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s2$url, stream_read_timeout = 5, connect_timeout = 2),
    on_changeset = function(x) n <<- n + 1L,
    should_stop = function() n >= 2L,
    until_first_payload = FALSE
  )
  expect_true(n >= 1L)
  s2$stop()

  # Goodbye
  s3 <- .ld_start_local_sse("goodbye")
  on.exit(s3$stop(), add = TRUE)
  res3 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s3$url, read_timeout = 3, connect_timeout = 2),
    until_first_payload = TRUE
  )
  expect_equal(res3$error, "goodbye")
  s3$stop()

  # Error then successful payload (discard in-flight)
  s4 <- .ld_start_local_sse("error_then_ok")
  on.exit(s4$stop(), add = TRUE)
  res4 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s4$url, read_timeout = 4, connect_timeout = 2),
    until_first_payload = TRUE
  )
  expect_true(isTRUE(res4$ok))
  s4$stop()

  # Malformed JSON
  s5 <- .ld_start_local_sse("badjson")
  on.exit(s5$stop(), add = TRUE)
  res5 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s5$url, read_timeout = 3, connect_timeout = 2),
    until_first_payload = TRUE
  )
  expect_equal(res5$error, "malformed SSE JSON")
  s5$stop()

  # 401
  s6 <- .ld_start_local_sse("401")
  on.exit(s6$stop(), add = TRUE)
  res6 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s6$url, read_timeout = 3, connect_timeout = 2),
    until_first_payload = TRUE
  )
  expect_false(isTRUE(res6$ok))
  expect_match(res6$error, "HTTP 401", fixed = TRUE)
  s6$stop()

  # 403 + fdv1 fallback header
  s7 <- .ld_start_local_sse("403fb")
  on.exit(s7$stop(), add = TRUE)
  res7 <- launchdarklyServer:::fdv2_stream_once(
    list(sdk_key = "k", stream_uri = s7$url, read_timeout = 3, connect_timeout = 2),
    until_first_payload = TRUE
  )
  expect_true(res7$fallback_to_fdv1)
  s7$stop()
})

test_that("evaluator remaining branches: off null, nested prereq stack, segment excludeContexts", {
  get_seg <- function(k) NULL

  d_off <- launchdarklyServer:::ld_evaluate_flag(
    list(key = "o", on = FALSE, variations = list("a"), offVariation = NULL),
    ld_context("u"), "def", function(k) NULL, get_seg
  )
  expect_equal(d_off$reason$kind, "OFF")
  expect_equal(d_off$value, "def") # NULL value filled with default

  # Nested prereq success pops stack (child of child)
  leaf <- list(key = "leaf", on = TRUE, version = 1, variations = list(FALSE, TRUE),
               fallthrough = list(variation = 1))
  mid <- list(
    key = "mid", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 1),
    prerequisites = list(list(key = "leaf", variation = 1))
  )
  top <- list(
    key = "top", on = TRUE, version = 1, variations = list(FALSE, TRUE),
    fallthrough = list(variation = 1),
    prerequisites = list(list(key = "mid", variation = 1))
  )
  gf <- function(k) switch(k, leaf = leaf, mid = mid, top = top, NULL)
  expect_equal(launchdarklyServer:::ld_evaluate_flag(top, ld_context("u"), FALSE, gf, get_seg)$value, TRUE)

  # User targets without matching user context kind in multi without user
  flag_t <- list(
    key = "t", on = TRUE, version = 1, variations = list("no", "yes"),
    fallthrough = list(variation = 0),
    targets = list(list(variation = 1, values = list("alice")))
  )
  only_org <- ld_context("alice", kind = "org")
  expect_equal(
    launchdarklyServer:::ld_evaluate_flag(flag_t, only_org, "d", function(k) NULL, get_seg)$value,
    "no"
  )

  # contextTargets org match
  flag_ct <- list(
    key = "ct", on = TRUE, version = 1, variations = list("a", "b"),
    fallthrough = list(variation = 0),
    contextTargets = list(list(contextKind = "org", variation = 1, values = list("acme")))
  )
  expect_equal(
    launchdarklyServer:::ld_evaluate_flag(
      flag_ct, ld_context("acme", kind = "org"), "d", function(k) NULL, get_seg
    )$value,
    "b"
  )

  # Variation index path without rollout
  expect_equal(
    launchdarklyServer:::.ld_variation_index_for_context(
      list(), list(variation = 2L), ld_context("u")
    )$index,
    2L
  )
  expect_null(
    launchdarklyServer:::.ld_variation_index_for_context(list(), list(), ld_context("u"))$index
  )

  # bucketBy non-bucketable → clause_value null → bucket 0 path via empty string handling
  expect_null(launchdarklyServer:::.ld_bucketable_string(NULL))
  expect_null(launchdarklyServer:::.ld_bucketable_string(c("a", "b")))
  expect_null(launchdarklyServer:::.ld_parse_attr_ref(list(1, 2))$parts)

  # Attr deep path missing
  expect_null(launchdarklyServer:::.ld_attr_value(ld_context("u", a = list(b = 1)), "/a/c"))

  # Segment excludedContexts + rule weight bucket
  seg <- list(
    key = "s", version = 1,
    excludedContexts = list(list(contextKind = "org", values = list("nope"))),
    rules = list(list(
      clauses = list(list(attribute = "key", op = "in", values = list("u"))),
      weight = 100000,
      rolloutContextKind = "user"
    ))
  )
  expect_true(launchdarklyServer:::.ld_segment_matches(
    seg, ld_context("u"), list(segment_stack = character()), function(k) NULL
  ))
  expect_false(launchdarklyServer:::.ld_segment_matches(
    seg, context_create_multi(ld_context("u"), ld_context("nope", kind = "org")),
    list(segment_stack = character()), function(k) NULL
  ))

  # Clause missing context returns FALSE
  expect_false(launchdarklyServer:::.ld_clause_matches(
    list(attribute = "email", op = "in", values = list("x"), contextKind = "org"),
    ld_context("u"), list(), function(k) NULL
  ))

  # Array no match + negate false
  expect_false(launchdarklyServer:::.ld_clause_matches(
    list(attribute = "tags", op = "in", values = list("z")),
    ld_context("u", tags = list("a", "b")), list(), function(k) NULL
  ))

  # match_clause_by_kind false
  expect_false(launchdarklyServer:::.ld_match_clause_by_kind(
    list(op = "in", values = list("device")),
    ld_context("u")
  ))

  # segmentMatch with missing segment
  expect_false(launchdarklyServer:::.ld_clause_matches(
    list(op = "segmentMatch", values = list("missing")),
    ld_context("u"), list(), function(k) NULL
  ))

  # segment rule weight miss
  expect_false(launchdarklyServer:::.ld_segment_rule_matches(
    list(clauses = list(), weight = 0, bucketBy = "key"),
    ld_context("u"), list(segment_stack = character()), function(k) NULL, "s", "salt"
  ))
  expect_true(launchdarklyServer:::.ld_segment_rule_matches(
    list(clauses = list()),
    ld_context("u"), list(segment_stack = character()), function(k) NULL, "s", "salt"
  ))
})
