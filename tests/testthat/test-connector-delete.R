context("Connector delete")

source("helper-credentials.R")

test_that("delete_connector", {
  expect(delete_connector(get_connector_id_from_name("SQL_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for SQL_CONNECTOR_TESTU")
  expect(delete_connector(get_connector_id_from_name("HIVE_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for HIVE_CONNECTOR_TESTU")
  # expect(delete_connector(get_connector_id_from_name("HBASE_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for HBASE_CONNECTOR_TESTU")
  # expect(delete_connector(get_connector_id_from_name("HDFS_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for HDFS_CONNECTOR_TESTU")
  expect(delete_connector(get_connector_id_from_name("S3_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for S3_CONNECTOR_TESTU")
  expect(delete_connector(get_connector_id_from_name("SFTP_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for SFTP_CONNECTOR_TESTU")
  expect(delete_connector(get_connector_id_from_name("FTP_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for FTP_CONNECTOR_TESTU")
  expect(delete_connector(get_connector_id_from_name("GCP_CONNECTOR_TESTU")) == 200, "delete_connector() doesn't retrieve a 200 status code for GCP_CONNECTOR_TESTU")
})
