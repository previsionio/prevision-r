context("Scheduler")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_scheduler = length(getScheduler())

# TODO : SCHEDULER CREATION TEST U

test_that("getScheduler", {
  expect_is(getScheduler(), "list", "getScheduler() doesn't retrieve a list")
  expect(length(getScheduler()) > nb_scheduler, "The number of schedulder tasks has not increased after scheduler task creation")
})

test_that("getSchedulerIdFromName", {
  expect_is(getSchedulerIdFromName("SCHEDULER_TESTU"), "character", "getSchedulerIdFromName() doesn't retrieve a character for SCHEDULER_TESTU")
})

test_that("startScheduler", {
  expect_is(startScheduler(getSchedulerIdFromName("SCHEDULER_TESTU")), "character", "startScheduler() doesn't retrieve a 200 status code for SCHEDULER_TESTU")
})

test_that("deleteScheduler", {
  expect(deleteScheduler(getSchedulerIdFromName("SCHEDULER_TESTU")) == 200, "deleteScheduler() doesn't retrieve a 200 status code for SCHEDULER_TESTU")
})
