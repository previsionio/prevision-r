context("Connector")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_connectors = length(getConnectors())

test_that("createConnector", {
  expect_is(createConnector(type = "SQL",
                            name = "SQL_CONNECTOR_TESTU",
                            host = SQL_CONNECTOR_HOST,
                            port = SQL_CONNECTOR_PORT,
                            username = SQL_CONNECTOR_USER,
                            password = SQL_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for SQL type")
  expect_is(createConnector(type = "HIVE",
                            name = "HIVE_CONNECTOR_TESTU",
                            host = HIVE_CONNECTOR_HOST,
                            port = HIVE_CONNECTOR_PORT,
                            username = HIVE_CONNECTOR_USER,
                            password = HIVE_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for HIVE type")
  # expect_is(createConnector(type = "HBASE",
  #                           name = "HBASE_CONNECTOR_TESTU",
  #                           host = HBASE_CONNECTOR_HOST,
  #                           port = HBASE_CONNECTOR_PORT,
  #                           username = HBASE_CONNECTOR_USER,
  #                           password = HBASE_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for HBASE type")
  # expect_is(createConnector(type = "HDFS",
  #                           name = "HDFS_CONNECTOR_TESTU",
  #                           host = HDFS_CONNECTOR_HOST,
  #                           port = HDFS_CONNECTOR_PORT,
  #                           username = HDFS_CONNECTOR_USER,
  #                           password = HDFS_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for HDFS type")
  expect_is(createConnector(type = "S3",
                            name = "S3_CONNECTOR_TESTU",
                            host = "",
                            port = "",
                            username = S3_CONNECTOR_USER,
                            password = S3_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for S3 type")
  expect_is(createConnector(type = "SFTP",
                            name = "SFTP_CONNECTOR_TESTU",
                            host = SFTP_CONNECTOR_HOST,
                            port = SFTP_CONNECTOR_PORT,
                            username = SFTP_CONNECTOR_USER,
                            password = SFTP_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for SFTP type")
  expect_is(createConnector(type = "FTP",
                            name = "FTP_CONNECTOR_TESTU",
                            host = FTP_CONNECTOR_HOST,
                            port = FTP_CONNECTOR_PORT,
                            username = FTP_CONNECTOR_USER,
                            password = FTP_CONNECTOR_PASS), "list", "createConnector() doesn't retrieve a list for FTP type")
  expect_is(createConnector(type = "GCP",
                            name = "GCP_CONNECTOR_TESTU",
                            host = "",
                            port = "",
                            username = "",
                            password = "",
                            googleCredentials = GCP_CREDENTIALS), "list", "createConnector() doesn't retrieve a list for GCP type")
})

test_that("getConnectors", {
  expect_is(getConnectors(), "list", "getConnectors() doesn't retrieve a list")
  expect(length(getConnectors()) > nb_connectors, "The number of connectors has not increased after connectors creation")
})

test_that("getConnectorInfos", {
  expect_is(getConnectorInfos(getConnectors()[1]$`_id`), "list", "getConnectorInfos() doesn't retrieve a list")
})

test_that("getConnectorIdFromName", {
  expect_is(getConnectorIdFromName("SQL_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for SQL_CONNECTOR_TESTU")
  expect_is(getConnectorIdFromName("HIVE_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for HIVE_CONNECTOR_TESTU")
  # expect_is(getConnectorIdFromName("HBASE_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for HBASE_CONNECTOR_TESTU")
  # expect_is(getConnectorIdFromName("HDFS_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for HDFS_CONNECTOR_TESTU")
  expect_is(getConnectorIdFromName("S3_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for S3_CONNECTOR_TESTU")
  expect_is(getConnectorIdFromName("SFTP_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for SFTP_CONNECTOR_TESTU")
  expect_is(getConnectorIdFromName("FTP_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for FTP_CONNECTOR_TESTU")
  expect_is(getConnectorIdFromName("GCP_CONNECTOR_TESTU"), "character", "getConnectorIdFromName() doesn't retrieve a character for GCP_CONNECTOR_TESTU")
})

test_that("testConnector", {
  expect(testConnector(getConnectorIdFromName("SQL_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for SQL_CONNECTOR_TESTU")
  expect(testConnector(getConnectorIdFromName("HIVE_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for HIVE_CONNECTOR_TESTU")
  # expect(testConnector(getConnectorIdFromName("HBASE_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for HBASE_CONNECTOR_TESTU")
  # expect(testConnector(getConnectorIdFromName("HDFS_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for HDFS_CONNECTOR_TESTU")
  expect(testConnector(getConnectorIdFromName("S3_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for S3_CONNECTOR_TESTU")
  expect(testConnector(getConnectorIdFromName("SFTP_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for SFTP_CONNECTOR_TESTU")
  expect(testConnector(getConnectorIdFromName("FTP_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for FTP_CONNECTOR_TESTU")
  expect(testConnector(getConnectorIdFromName("GCP_CONNECTOR_TESTU")) == 200, "testConnector() doesn't retrieve a 200 status code for GCP_CONNECTOR_TESTU")
})
