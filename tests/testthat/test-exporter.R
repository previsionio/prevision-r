context("Exporter")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_exporters = length(get_exporters(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_exporter", {
  expect_is(create_exporter(project_id = get_project_id_from_name("PROJECT_TESTU"),
                            connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_CONNECTOR_TESTU"),
                            name = "S3_EXPORTER_TESTU",
                            bucket = S3_DATASOURCE_BUCKET,
                            filepath = "S3_EXPORTER_TESTU.csv"), "list", "create_exporter() doesn't retrieve a list for S3 connector type with bucket and path")

  expect_is(create_exporter(project_id = get_project_id_from_name("PROJECT_TESTU"),
                            connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_CONNECTOR_TESTU"),
                            name = "SFTP_EXPORTER_TESTU",
                            filepath = "SFTP_EXPORTER_TESTU.csv"), "list", "create_exporter() doesn't retrieve a list for SFTP connector type with path")

  expect_is(create_exporter(project_id = get_project_id_from_name("PROJECT_TESTU"),
                            connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_CONNECTOR_TESTU"),
                            name = "FTP_EXPORTER_TESTU",
                            filepath = "FTP_EXPORTER_TESTU.csv"), "list", "create_exporter() doesn't retrieve a list for FTP connector type with path")
})

test_that("get_exporters", {
  expect_is(get_exporters(get_project_id_from_name("PROJECT_TESTU")), "list", "get_exporters() doesn't retrieve a list")
  expect(length(get_exporters(get_project_id_from_name("PROJECT_TESTU"))) > nb_exporters, "The number of exporters has not increased after exporters creation")
})

test_that("get_exporter_id_from_name", {
  expect_is(get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                      exporter_name = get_exporters(get_project_id_from_name("PROJECT_TESTU"))[[1]]$name), "character", "get_exporter_id_from_name() doesn't retrieve a character")
})

test_that("get_exporter_info", {
  expect_is(get_exporter_info(get_exporters(get_project_id_from_name("PROJECT_TESTU"))[[1]]$`_id`), "list", "get_exporter_info() doesn't retrieve a list")
})

test_that("create_export", {
  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "S3_EXPORTER_TESTU"),
                          type = "dataset",
                          dataset_id = get_dataset_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                dataset_name = "DATASET_TESTU")), "list", "create_export() doesn't retrieve a list for S3 exporter and a dataset")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "FTP_EXPORTER_TESTU"),
                          type = "dataset",
                          dataset_id = get_dataset_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                dataset_name = "DATASET_TESTU")), "list", "create_export() doesn't retrieve a list for FTP exporter and a dataset")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "SFTP_EXPORTER_TESTU"),
                          type = "dataset",
                          dataset_id = get_dataset_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                dataset_name = "DATASET_TESTU")), "list", "create_export() doesn't retrieve a list for SFTP exporter and a dataset")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "S3_EXPORTER_TESTU"),
                          type = "validation-prediction",
                          prediction_id = get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                                   "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for S3 exporter and a validation-prediction")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "FTP_EXPORTER_TESTU"),
                          type = "validation-prediction",
                          prediction_id = get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                                   "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for FTP exporter and a validation-prediction")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "SFTP_EXPORTER_TESTU"),
                          type = "validation-prediction",
                          prediction_id = get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                                   "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for SFTP exporter and a validation-prediction")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "S3_EXPORTER_TESTU"),
                          type = "deployment-prediction",
                          prediction_id = get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                 "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED",
                                                                                                 "model"))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for S3 exporter and a deployment-prediction")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "FTP_EXPORTER_TESTU"),
                          type = "deployment-prediction",
                          prediction_id = get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                 "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED",
                                                                                                 "model"))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for FTP exporter and a deployment-prediction")

  expect_is(create_export(exporter_id = get_exporter_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                  exporter_name = "SFTP_EXPORTER_TESTU"),
                          type = "deployment-prediction",
                          prediction_id = get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                 "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED",
                                                                                                 "model"))[[1]]$`_id`), "list", "create_export() doesn't retrieve a list for SFTP exporter and a deployment-prediction")

})
