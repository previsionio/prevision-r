context("Datasource")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_datasources = length(getDatasources())

test_that("createDatasource", {
  expect_is(createDatasource(connectorId = getConnectorIdFromName("SQL_CONNECTOR_TESTU"),
                             name = "SQL_DATASOURCE_RAW_TESTU",
                             database = SQL_DATASOURCE_BASE,
                             table = SQL_DATASOURCE_TABLE), "list", "createDatasource() doesn't retrieve a list for SQL connector type without request")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("SQL_CONNECTOR_TESTU"),
                             name = "SQL_DATASOURCE_REQUEST_TESTU",
                             database = SQL_DATASOURCE_BASE,
                             request = SQL_DATASOURCE_REQUEST), "list", "createDatasource() doesn't retrieve a list for SQL connector type with request")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("HIVE_CONNECTOR_TESTU"),
                             name = "HIVE_DATASOURCE_RAW_TESTU",
                             database = HIVE_DATASOURCE_BASE,
                             table = HIVE_DATASOURCE_TABLE), "list", "createDatasource() doesn't retrieve a list for HIVE connector type without request")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("HIVE_CONNECTOR_TESTU"),
                             name = "HIVE_DATASOURCE_REQUEST_TESTU",
                             database = HIVE_DATASOURCE_BASE,
                             request = HIVE_DATASOURCE_REQUEST), "list", "createDatasource() doesn't retrieve a list for HIVE connector type with request")
  # expect_is(createDatasource(connectorId = getConnectorIdFromName("HBASE_CONNECTOR_TESTU"),
  #                            name = "HBASE_DATASOURCE_RAW_TESTU",
  #                            table = HBASE_DATASOURCE_TABLE), "list", "createDatasource() doesn't retrieve a list for HBASE connector type without request")
  # expect_is(createDatasource(connectorId = getConnectorIdFromName("HDFS_CONNECTOR_TESTU"),
  #                            name = "HDFS_DATASOURCE_RAW_TESTU",
  #                            path = HDFS_DATASOURCE_PATH), "list", "createDatasource() doesn't retrieve a list for HBASE connector type with path")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("S3_CONNECTOR_TESTU"),
                             name = "S3_DATASOURCE_RAW_TESTU",
                             bucket = S3_DATASOURCE_BUCKET,
                             path = S3_DATASOURCE_PATH), "list", "createDatasource() doesn't retrieve a list for S3 connector type with bucket and path")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("SFTP_CONNECTOR_TESTU"),
                             name = "SFTP_DATASOURCE_RAW_TESTU",
                             path = SFTP_DATASOURCE_PATH), "list", "createDatasource() doesn't retrieve a list for SFTP connector type with path")
  expect_is(createDatasource(connectorId = getConnectorIdFromName("FTP_CONNECTOR_TESTU"),
                             name = "FTP_DATASOURCE_RAW_TESTU",
                             path = FTP_DATASOURCE_PATH), "list", "createDatasource() doesn't retrieve a list for FTP connector type with path")
})

test_that("getDatasources", {
  expect_is(getDatasources(), "list", "getDatasources() doesn't retrieve a list")
  expect(length(getDatasources()) > nb_datasources, "The number of datasources has not increased after datasources creation")
})

test_that("getDatasourceIdFromName", {
  expect_is(getDatasourceIdFromName(getDatasources()[[1]]$name), "character", "getDatasourceIdFromName() doesn't retrieve a character")
})

test_that("getDatasource", {
  expect_is(getDatasourceInfos(getDatasources()[1]$`_id`), "list", "getDatasourceInfos() doesn't retrieve a list")
})

test_that("testDatasource", {
  expect(testDatasource(getDatasourceIdFromName("SQL_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_RAW_TESTU")
  expect(testDatasource(getDatasourceIdFromName("SQL_DATASOURCE_REQUEST_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_REQUEST_TESTU")
  expect(testDatasource(getDatasourceIdFromName("HIVE_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_RAW_TESTU")
  expect(testDatasource(getDatasourceIdFromName("HIVE_DATASOURCE_REQUEST_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_REQUEST_TESTU")
  # expect(testDatasource(getDatasourceIdFromName("HBASE_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for HBASE_DATASOURCE_RAW_TESTU")
  # expect(testDatasource(getDatasourceIdFromName("HDFS_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for HDFS_DATASOURCE_RAW_TESTU")
  expect(testDatasource(getDatasourceIdFromName("S3_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for S3_DATASOURCE_RAW_TESTU")
  expect(testDatasource(getDatasourceIdFromName("SFTP_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for SFTP_DATASOURCE_RAW_TESTU")
  expect(testDatasource(getDatasourceIdFromName("FTP_DATASOURCE_RAW_TESTU")) == 200, "testDatasource() doesn't retrieve a 200 status code for FTP_DATASOURCE_RAW_TESTU")
})
