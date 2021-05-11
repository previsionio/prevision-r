context("Usecases delete")

source("helper-credentials.R")

test_that("delete_usecase", {
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_REGRESSION_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_CLASSIFICATION_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_CLASSIFICATION_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_MULTICLASSIFICATION_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_MULTICLASSIFICATION_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_TIMESERIES_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_TIMESERIES_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_TEXT_SIM_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_TEXT_SIM_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_OBJECT_DETECTOR_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_OBJECT_DETECTOR_TESTU")
  expect(delete_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_IMAGE_CLASSIFICATION_TESTU")) == 200, "delete_usecase() doesn't retrieve a 200 status code for USECASE_IMAGE_CLASSIFICATION_TESTU")
})
