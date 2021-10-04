context("experiments delete")

source("helper-credentials.R")

test_that("delete_experiment", {
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_REGRESSION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_TIMESERIES_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_TEXTSIM_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_CLASSIFICATION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_CLASSIFICATION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_MULTICLASSIFICATION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_MULTICLASSIFICATION_TESTU")
  expect(delete_experiment(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")) == 204, "delete_experiment() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
})
