context("Datasource delete")

source("helper-credentials.R")

test_that("delete_datasource", {
  expect(delete_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_DATASOURCE_RAW_TESTU")) == 200, "delete_datasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_RAW_TESTU")
  expect(delete_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_DATASOURCE_REQUEST_TESTU")) == 200, "delete_datasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_REQUEST_TESTU")
  expect(delete_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_DATASOURCE_RAW_TESTU")) == 200, "delete_datasource() doesn't retrieve a 200 status code for S3_DATASOURCE_RAW_TESTU")
  expect(delete_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_DATASOURCE_RAW_TESTU")) == 200, "delete_datasource() doesn't retrieve a 200 status code for SFTP_DATASOURCE_RAW_TESTU")
  expect(delete_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_DATASOURCE_RAW_TESTU")) == 200, "delete_datasource() doesn't retrieve a 200 status code for FTP_DATASOURCE_RAW_TESTU")
})
