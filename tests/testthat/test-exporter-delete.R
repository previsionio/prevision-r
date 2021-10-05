context("Exporter delete")

source("helper-credentials.R")

test_that("delete_exporter", {
  expect(delete_exporter(get_exporter_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_EXPORTER_TESTU")) == 204, "delete_exporter() doesn't retrieve a 204 status code for S3_EXPORTER_TESTU")
  expect(delete_exporter(get_exporter_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_EXPORTER_TESTU")) == 204, "delete_exporter() doesn't retrieve a 204 status code for FTP_EXPORTER_TESTU")
  expect(delete_exporter(get_exporter_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_EXPORTER_TESTU")) == 204, "delete_exporter() doesn't retrieve a 204 status code for SFTP_EXPORTER_TESTU")
})
