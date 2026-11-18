test_that("context_create defaults to user kind", {
  c <- context_create("user-key")
  expect_true(context_valid(c))
  expect_equal(c$kind, "user")
  expect_equal(c$key, "user-key")
  expect_equal(context_fully_qualified_key(c), "user-key")
  expect_false(context_multiple(c))
  expect_equal(context_individual_count(c), 1L)
})

test_that("context_create with custom kind builds FQK", {
  c <- context_create("org-1", "organization")
  expect_equal(context_fully_qualified_key(c), "organization:org-1")
})

test_that("empty key is invalid", {
  c <- context_create("")
  expect_false(context_valid(c))
  expect_match(context_error(c), "key")
})

test_that("invalid kinds are rejected", {
  expect_false(context_valid(context_create("k", "")))
  expect_false(context_valid(context_create("k", "kind")))
  expect_false(context_valid(context_create("k", "multi")))
  expect_false(context_valid(context_create("k", "bad kind")))
})

test_that("builder sets attributes and private", {
  c <- context_builder("u1")$
    kind("user")$
    name("Ada")$
    anonymous(TRUE)$
    set("email", "a@b.c")$
    set("tier", 3)$
    mark_private("email", "/tier")$
    build()
  expect_true(context_valid(c))
  expect_equal(c$name, "Ada")
  expect_true(c$anonymous)
  expect_equal(context_get(c, "email"), "a@b.c")
  expect_equal(context_get(c, "tier"), 3)
  expect_equal(context_custom_attributes(c), c("email", "tier"))
  expect_equal(context_private_attributes(c), c("email", "/tier"))
})

test_that("ld_context convenience works", {
  c <- ld_context("u", country = "us", private = c("country"))
  expect_equal(context_get(c, "country"), "us")
  expect_equal(context_private_attributes(c), "country")
})

test_that("multi-context sorts kinds and builds FQK", {
  c <- context_create_multi(
    context_create("o1", "org"),
    context_create("u1", "user")
  )
  expect_true(context_multiple(c))
  expect_equal(c$kind, "multi")
  expect_equal(context_individual_count(c), 2L)
  # sorted by kind: org then user
  expect_equal(context_get_individual(c, 0L)$kind, "org")
  expect_equal(context_get_individual(c, "user")$key, "u1")
  expect_equal(context_fully_qualified_key(c), "org:o1:user:u1")
})

test_that("multi with duplicate kinds is invalid", {
  c <- context_create_multi(context_create("a", "user"), context_create("b", "user"))
  expect_false(context_valid(c))
})

test_that("multi builder flattens nested multi", {
  inner <- context_create_multi(context_create("a", "a"), context_create("b", "b"))
  outer <- context_multi_builder()$add(inner)$add(context_create("c", "c"))$build()
  expect_equal(context_individual_count(outer), 3L)
})

test_that("single-context multi builder returns that context", {
  u <- context_create("only")
  c <- context_multi_builder()$add(u)$build()
  expect_false(context_multiple(c))
  expect_equal(c$key, "only")
})

test_that("FQK escapes percent and colon in keys", {
  c <- context_create("a%b:c", "org")
  expect_equal(context_fully_qualified_key(c), "org:a%25b%3Ac")
})

test_that("to_list / from_list round-trip", {
  c <- context_builder("u")$name("N")$set("x", list(a = 1))$mark_private("x")$build()
  props <- context_to_list(c)
  c2 <- context_from_list(props)
  expect_true(context_valid(c2))
  expect_equal(c2$key, "u")
  expect_equal(c2$name, "N")
  expect_equal(context_get(c2, "x"), list(a = 1))
  expect_equal(context_private_attributes(c2), "x")
})

test_that("from_list multi and json round-trip", {
  c <- context_create_multi(context_create("u1"), context_create("d1", "device"))
  json <- context_to_json(c)
  c2 <- context_from_json(json)
  expect_true(context_multiple(c2))
  expect_equal(context_fully_qualified_key(c2), context_fully_qualified_key(c))
})

test_that("without_anonymous filters", {
  multi <- context_create_multi(
    context_builder("u")$anonymous(TRUE)$build(),
    context_create("o", "org")
  )
  filtered <- context_without_anonymous(multi)
  expect_false(context_multiple(filtered))
  expect_equal(filtered$kind, "org")
})

test_that("try_set rejects bad types for reserved attrs", {
  b <- context_builder("k")
  expect_false(b$try_set("key", 1))
  expect_false(b$try_set("anonymous", "yes"))
  expect_false(b$try_set("name", 1))
  expect_false(b$try_set("_meta", "x"))
})
