context("Usecases delete")

source("helper-credentials.R")

test_that("deleteUsecase", {
  expect(deleteUsecase(getUsecaseIdFromName("USECASE_REGRESSION_TESTU")) == 200, "deleteUsecase() doesn't retrieve a 200 status code for USECASE_REGRESSION_TESTU")
  expect(deleteUsecase(getUsecaseIdFromName("USECASE_CLASSIFICATION_TESTU")) == 200, "deleteUsecase() doesn't retrieve a 200 status code for USECASE_CLASSIFICATION_TESTU")
  expect(deleteUsecase(getUsecaseIdFromName("USECASE_MULTICLASSIFICATION_TESTU")) == 200, "deleteUsecase() doesn't retrieve a 200 status code for USECASE_MULTICLASSIFICATION_TESTU")
  expect(deleteUsecase(getUsecaseIdFromName("USECASE_TIMESERIES_TESTU")) == 200, "deleteUsecase() doesn't retrieve a 200 status code for USECASE_TIMESERIES_TESTU")
})
