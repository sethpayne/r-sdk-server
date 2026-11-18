make_client_with_flags <- function(flags, segments = list()) {
  cfg <- ld_config("k", offline = TRUE)
  client <- ld_client(cfg)
  client$.update_store_for_tests(flags = flags, segments = segments)
  client
}

test_that("targets, rules, prereqs, segments, operators", {
  flags <- list(
    "off-flag" = list(
      key = "off-flag", version = 1, on = FALSE,
      variations = list("a", "b"), offVariation = 1
    ),
    "target-flag" = list(
      key = "target-flag", version = 1, on = TRUE,
      variations = list("no", "yes"),
      fallthrough = list(variation = 0),
      targets = list(list(values = list("alice"), variation = 1))
    ),
    "rule-flag" = list(
      key = "rule-flag", version = 1, on = TRUE, salt = "s",
      variations = list("default", "matched"),
      fallthrough = list(variation = 0),
      rules = list(list(
        id = "r1",
        variation = 1,
        clauses = list(list(
          attribute = "email",
          op = "endsWith",
          values = list("@example.com"),
          negate = FALSE
        ))
      ))
    ),
    "prereq-child" = list(
      key = "prereq-child", version = 1, on = TRUE,
      variations = list(FALSE, TRUE),
      fallthrough = list(variation = 1),
      prerequisites = list(list(key = "prereq-parent", variation = 1))
    ),
    "prereq-parent" = list(
      key = "prereq-parent", version = 1, on = TRUE,
      variations = list(FALSE, TRUE),
      fallthrough = list(variation = 1)
    ),
    "seg-flag" = list(
      key = "seg-flag", version = 1, on = TRUE,
      variations = list("out", "in"),
      fallthrough = list(variation = 0),
      rules = list(list(
        variation = 1,
        clauses = list(list(
          attribute = "",
          op = "segmentMatch",
          values = list("seg1")
        ))
      ))
    )
  )
  segments <- list(
    "seg1" = list(key = "seg1", version = 1, included = list("bob"), rules = list())
  )

  client <- make_client_with_flags(flags, segments)
  on.exit(client$close(), add = TRUE)

  expect_equal(client$variation("off-flag", ld_context("x"), "d"), "b")

  expect_equal(client$variation("target-flag", ld_context("alice"), "d"), "yes")
  expect_equal(client$variation("target-flag", ld_context("other"), "d"), "no")

  matched <- client$variation_detail(
    "rule-flag",
    ld_context("u", email = "a@example.com"),
    "d"
  )
  expect_equal(matched$value, "matched")
  expect_equal(matched$reason$kind, "RULE_MATCH")

  expect_equal(client$variation("prereq-child", ld_context("u"), FALSE), TRUE)

  expect_equal(client$variation("seg-flag", ld_context("bob"), "d"), "in")
  expect_equal(client$variation("seg-flag", ld_context("amy"), "d"), "out")
})

test_that("numeric and negate operators", {
  flags <- list(
    "num" = list(
      key = "num", version = 1, on = TRUE,
      variations = list("lo", "hi"),
      fallthrough = list(variation = 0),
      rules = list(list(
        variation = 1,
        clauses = list(list(
          attribute = "age", op = "greaterThanOrEqual", values = list(18), negate = FALSE
        ))
      ))
    ),
    "neg" = list(
      key = "neg", version = 1, on = TRUE,
      variations = list("no", "yes"),
      fallthrough = list(variation = 0),
      rules = list(list(
        variation = 1,
        clauses = list(list(
          attribute = "country", op = "in", values = list("us"), negate = TRUE
        ))
      ))
    )
  )
  client <- make_client_with_flags(flags)
  on.exit(client$close(), add = TRUE)
  expect_equal(client$variation("num", ld_context("u", age = 21), "d"), "hi")
  expect_equal(client$variation("num", ld_context("u", age = 10), "d"), "lo")
  expect_equal(client$variation("neg", ld_context("u", country = "ca"), "d"), "yes")
  expect_equal(client$variation("neg", ld_context("u", country = "us"), "d"), "no")
})
