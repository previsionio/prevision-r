context("App")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_app = length(getApps())

test_that("createApp", {
  expect_is(createApp("model",
                      "MODEL_APP_TESTU",
                      1,
                      NULL,
                      getUsecaseModels(getUsecases()[[1]]$usecaseId)[[1]]$`_id`,
                      NULL), "list", "createApp() doesn't retrieve a list for model type app")
})

test_that("getApps", {
  expect_is(getApps(), "list", "getApps() doesn't retrieve a list")
  expect(length(getApps()) > nb_app, "The number of apps has not increased after apps creation")
})

test_that("getApp", {
  expect_is(getApp(getApps()[1]$`_id`), "list", "getApp() doesn't retrieve a list")
})

test_that("getAppIdFromName", {
  expect_is(getAppIdFromName("MODEL_APP_TESTU"), "character", "getAppIdFromName() doesn't retrieve a character for MODEL_APP_TESTU")
})

test_that("updateAppName", {
  expect(updateAppName(getAppIdFromName("MODEL_APP_TESTU"), "MODEL_APP_TESTU_NEW") == 200, "updateAppName() doesn't retrieve a 200 status code for MODEL_APP_TEST")
})

test_that("deleteApp", {
  expect(deleteApp(getAppIdFromName("MODEL_APP_TESTU_NEW")) == 200, "deleteApp() doesn't retrieve a 200 status code for MODEL_APP_TESTU")
})
