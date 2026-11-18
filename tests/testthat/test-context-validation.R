test_that("context keys must be a single non-NA string (D8)", {
  bad <- list(null = NULL, na = NA, na_character = NA_character_, vector = c("a", "b"),
              empty_vec = character(), empty = "")
  for (nm in names(bad)) {
    c <- context_create(bad[[nm]])
    expect_false(context_valid(c), label = nm)
    expect_true(is.character(context_error(c)), label = nm)
  }
  expect_true(context_valid(context_create(123)))
  expect_equal(context_create(123)$key, "123")
})

test_that("context kinds must be a single valid string", {
  expect_false(context_valid(context_create("k", NA_character_)))
  expect_false(context_valid(context_create("k", c("a", "b"))))
  expect_false(context_valid(context_create("k", "k\u00e9")))
  expect_true(context_valid(context_create("k", "org.sub-kind_1")))
})

test_that("ld_context does not partially match attributes to its options", {
  # `key` precedes `...`, so attribute names that prefix "key" (e.g. `k`)
  # still bind to it; every other option is exact-match only.
  c <- ld_context("x", n = 33333, a = 1, ki = "v", p = "q")
  expect_null(c$name)
  expect_false(c$anonymous)
  expect_equal(c$kind, "user")
  expect_equal(context_get(c, "n"), 33333)
  expect_equal(context_get(c, "a"), 1)
  expect_equal(context_get(c, "ki"), "v")
  expect_equal(context_private_attributes(c), character())
  expect_equal(ld_context("x", kind = "org", name = "N")$name, "N")
})

test_that("from_json rejects malformed input without throwing", {
  inputs <- c("", "not json", "[]", "null", '"str"', '{"kind":"user"}',
              '{"kind":"user","key":5}', '{"kind":"multi","user":"x"}',
              '{"kind":"user","key":"k","_meta":{"privateAttributes":[1]}}')
  for (j in inputs) {
    c <- expect_no_error(context_from_json(j))
    expect_false(context_valid(c), label = j)
  }
})

test_that("factors are stored as character attributes", {
  c <- ld_context("u", tier = factor("gold"))
  expect_identical(context_get(c, "tier"), "gold")
})

test_that("serialization: length-1 vectors become scalars; lists stay arrays", {
  # jsonlite auto_unbox cannot tell c("x") from "x"; use list() for arrays.
  expect_equal(as.character(context_to_json(ld_context("u", g = c("beta")))),
               '{"key":"u","kind":"user","g":"beta"}')
  expect_equal(as.character(context_to_json(ld_context("u", g = list("beta")))),
               '{"key":"u","kind":"user","g":["beta"]}')
  expect_equal(as.character(context_to_json(ld_context("u", g = c("a", "b")))),
               '{"key":"u","kind":"user","g":["a","b"]}')
})

test_that("contexts round-trip through JSON with nested attributes", {
  c <- ld_context("u", name = "Ada", address = list(street = "Main", geo = list(lat = 1.5)),
                  tags = list("a", "b"), private = c("/address/street"))
  back <- context_from_json(context_to_json(c))
  expect_true(context_valid(back))
  expect_equal(context_get(back, "address"), list(street = "Main", geo = list(lat = 1.5)))
  expect_equal(context_get(back, "tags"), list("a", "b"))
  expect_equal(context_private_attributes(back), "/address/street")
  expect_equal(context_fully_qualified_key(back), "u")
})
