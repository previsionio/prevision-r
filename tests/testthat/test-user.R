context("User")

source("helper-credentials.R")

test_that("getProfile", {
  expect_is(getProfile(), "list", "getProfile() doesn't retrieve a list")
})

test_that("getVersion", {
  expect_is(getVersion(), "list", "getVersion() doesn't retrieve a list")
})
