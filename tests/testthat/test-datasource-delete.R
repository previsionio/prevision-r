context("Datasource delete")

source("helper-credentials.R")

test_that("deleteDatasource", {
  expect(deleteDatasource(getDatasourceIdFromName("SQL_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_RAW_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("SQL_DATASOURCE_REQUEST_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_REQUEST_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("HIVE_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_RAW_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("HIVE_DATASOURCE_REQUEST_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_REQUEST_TESTU")
  # expect(deleteDatasource(getDatasourceIdFromName("HBASE_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for HBASE_DATASOURCE_RAW_TESTU")
  # expect(deleteDatasource(getDatasourceIdFromName("HDFS_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for HDFS_DATASOURCE_RAW_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("S3_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for S3_DATASOURCE_RAW_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("SFTP_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for SFTP_DATASOURCE_RAW_TESTU")
  expect(deleteDatasource(getDatasourceIdFromName("FTP_DATASOURCE_RAW_TESTU")) == 200, "deleteDatasource() doesn't retrieve a 200 status code for FTP_DATASOURCE_RAW_TESTU")
})
