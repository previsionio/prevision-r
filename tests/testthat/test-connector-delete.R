context("Connector delete")

source("helper-credentials.R")

test_that("deleteConnector", {
  expect(deleteConnector(getConnectorIdFromName("SQL_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for SQL_CONNECTOR_TESTU")
  expect(deleteConnector(getConnectorIdFromName("HIVE_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for HIVE_CONNECTOR_TESTU")
  # expect(deleteConnector(getConnectorIdFromName("HBASE_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for HBASE_CONNECTOR_TESTU")
  # expect(deleteConnector(getConnectorIdFromName("HDFS_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for HDFS_CONNECTOR_TESTU")
  expect(deleteConnector(getConnectorIdFromName("S3_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for S3_CONNECTOR_TESTU")
  expect(deleteConnector(getConnectorIdFromName("SFTP_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for SFTP_CONNECTOR_TESTU")
  expect(deleteConnector(getConnectorIdFromName("FTP_CONNECTOR_TESTU")) == 200, "deleteConnector() doesn't retrieve a 200 status code for FTP_CONNECTOR_TESTU")
})
