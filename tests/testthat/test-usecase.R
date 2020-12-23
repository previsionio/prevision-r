context("Usecases")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_usecases = length(getUsecases())

##################################
### CREATE SOME USE CASES HERE ###
##################################
test_that("startUsecase", {
  expect_is(startUsecase("USECASE_REGRESSION_TESTU", "tabular", "regression", createDatasetFromDataframe("DS_REGRESSION_TESTU", tabularDataset("regression"))$`_id`, "TARGET", normalModels = c("LR", "RF"), liteModels = c("LR"), simpleModels = c("LR", "DT"), withBlend = F), "list", "getUsecases() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(startUsecase("USECASE_CLASSIFICATION_TESTU", "tabular", "classification", createDatasetFromDataframe("DS_CLASSIFCICATION_TESTU", tabularDataset("classification"))$`_id`, "TARGET", normalModels = c("LGB", "XGB")), "list", "getUsecases() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(startUsecase("USECASE_MULTICLASSIFICATION_TESTU", "tabular", "multiclassification", createDatasetFromDataframe("USECASE_MULTICLASSIFICATION_TESTU", tabularDataset("multiclassification"))$`_id`, "TARGET"), "list", "getUsecases() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(startUsecase("USECASE_TIMESERIES_TESTU", "timeseries", "regression", createDatasetFromDataframe("USECASE_TIMESERIES_TESTU", timeseriesDataset())$`_id`, "TARGET", timeColumn = "TS", startDW = -2, endDW = -1, startFW = 1, endFW = 2), "list", "getUsecases() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
})

Sys.sleep(120)

test_that("getUsecases", {
  expect_is(getUsecases(), "list", "getUsecases() doesn't retrieve a list")
  expect(length(getUsecases()) >= 1, "getUsecases() doesn't have at least one element")
  expect(length(getUsecases()) >= nb_usecases+1, "The number of usecases has not increased after dataset creation")
})

test_that("getUsecaseIdFromName", {
  expect_is(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), "character", "getUsecaseIdFromName() doesn't retrieve a character")
})

test_that("getUsecaseInfos", {
  expect_is(getUsecaseInfos(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseInfos() doesn't retrieve a list")
})

test_that("getUsecaseTasks", {
  expect_is(getUsecaseTasks(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseTasks() doesn't retrieve a list")
})

test_that("getUsecaseSchema", {
  expect_is(getUsecaseSchema(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseSchema() doesn't retrieve a list")
})

test_that("getUsecaseFeatures", {
  expect_is(getUsecaseFeatures(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseFeatures() doesn't retrieve a list")
})

test_that("getFeaturesInfos", {
  expect_is(getFeaturesInfos(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseFeatures(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")[[1]])$featureList[[1]]$name), "list", "getFeaturesInfos() doesn't retrieve a list")
})

test_that("getUsecaseModels", {
  expect_is(getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseModels() doesn't retrieve a list")
})

test_that("getModelInfos", {
  expect_is(getModelInfos(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "getModelInfos() doesn't retrieve a list")
})

test_that("getModelHyperparameters", {
  expect_is(getModelHyperparameters(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "getModelHyperparameters() doesn't retrieve a list")
})

test_that("getModelFeatureImportance", {
  expect_is(getModelFeatureImportance(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`, "raw"), "data.frame", "getModelFeatureImportance() doesn't retrieve a data.frame")
  expect_is(getModelFeatureImportance(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`, "engineering"), "data.frame", "getModelFeatureImportance() doesn't retrieve a data.frame")
})

test_that("startPrediction", {
  expect_is(startPrediction(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getDatasets()[[1]]$`_id`), "list", "startPrediction() doesn't retrieve a list")
})

Sys.sleep(30)

test_that("getUsecasePredictions", {
  expect_is(getUsecasePredictions(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecasePredictions() doesn't retrieve a list")
})

test_that("getPredictionInfos", {
  expect_is(getPredictionInfos(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecasePredictions(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "getPredictionInfos() doesn't retrieve a list")
})

test_that("getPrediction", {
  expect_is(getPrediction(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecasePredictions(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "data.frame", "getPrediction() doesn't retrieve a data.frame")
})

# test_that("deletePrediction", {
#   expect_is(deletePrediction(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecasePredictions(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "deletePrediction() doesn't retrieve a list")
# })
#
# test_that("getPredictionEvents", {
#   expect_is(getPredictionEvents(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getPredictionEvents() doesn't retrieve a list")
# })

test_that("shareUsecase", {
  expect_is(shareUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), "pierre.nowak@prevision.io"), "list", "shareUsecase() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
})

test_that("getSharedUsecaseUsers", {
  expect_is(getSharedUsecaseUsers(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getSharedUsecaseUsers() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect(length(getSharedUsecaseUsers(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))) == 1, "getSharedUsecaseUsers() hasn't 1 element for USECASE_REGRESSION_TESTU after sharing")
})

test_that("unshareUsecase", {
  expect_is(unshareUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "unshareUsecase() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
})

test_that("getSharedUsecaseUsers", {
  expect(length(getSharedUsecaseUsers(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))) == 0, "getSharedUsecaseUsers() hasn't 0 element for USECASE_REGRESSION_TESTU after unsharing")
})

test_that("updateUsecaseDescription", {
  expect_is(updateUsecaseDescription(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), "DESCRIPTION"), "list", "updateUsecaseDescription() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
})

test_that("getUsecaseCV", {
  expect_is(getUsecaseCV(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "data.frame", "getUsecaseCV() doesn't retrieve a data.frame for USECASE_REGRESSION_TESTU")
  expect_is(getUsecaseCV(getUsecaseIdFromName("USECASE_CLASSIFICATION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "getUsecaseCV() doesn't retrieve a data.frame for USECASE_CLASSIFICATION_TESTU")
  expect_is(getUsecaseCV(getUsecaseIdFromName("USECASE_MULTICLASSIFICATION_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_MULTICLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "getUsecaseCV() doesn't retrieve a data.frame for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(getUsecaseCV(getUsecaseIdFromName("USECASE_TIMESERIES_TESTU"), getUsecaseModels(getUsecaseIdFromName("USECASE_TIMESERIES_TESTU"))[[1]]$`_id`), "data.frame", "getUsecaseCV() doesn't retrieve a data.frame for USECASE_TIMESERIES_TESTU")
})

test_that("getBestModelId", {
  expect_is(getBestModelId(getUsecaseIdFromName("USECASE_REGRESSION_TESTU"), 1, TRUE), "character", "getBestModelId() doesn't retrieve a character")
})

# test_that("getUsecaseEvents", {
#   expect_is(getUsecaseEvents(), "list", "getUsecaseEvents() doesn't retrieve a list for all use cases")
#   expect_is(getUsecaseEvents(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")), "list", "getUsecaseEvents() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
# })

test_that("pauseUsecase", {
  expect(pauseUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")) == 200, "pauseUsecase() doesn't retrieve a 200 status code")
})

test_that("resumeUsecase", {
  expect(resumeUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")) == 200, "resumeUsecase() doesn't retrieve a 200 status code")
})

test_that("stopUsecase", {
  expect(stopUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")) == 200, "stopUsecase() doesn't a 200 status code")
})
