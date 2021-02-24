context("User")

source("helper-credentials.R")

test_that("get_version", {
  expect_is(get_version(), "list", "get_version() doesn't retrieve a list")
})

test_that("get_users", {
  expect_is(get_users(), "list", "get_users() doesn't retrieve a list")
})

test_that("get_users_usage", {
  expect_is(get_users_usage(), "list", "get_users_usage() doesn't retrieve a list")
})
