context("Dataset")

source("helper-credentials.R")
source("helper-dataset.R")

# INIT GLOBAL VARIABLE
nb_datasets = length(getDatasets())
n_row       = 1000
n_features  = 10

test_that("createDatasetFromDataframe", {
  expect_is(createDatasetFromDataframe("DATASET_TESTU", tabularDataset(typeProblem = "regression", n_row = n_row, n_features = n_features), F), "list", "createDatasetFromDataframe() doesn't retrieve a list for DATASET_TESTU if not zipped")
  expect_is(createDatasetFromDataframe("DATASET_TESTU", tabularDataset(typeProblem = "regression", n_row = n_row, n_features = n_features), T), "list", "createDatasetFromDataframe() doesn't retrieve a list for DATASET_TESTU if zipped")
})

test_that("createDatasetFromDatasource", {
  expect_is(createDatasetFromDatasource("DATASET_SQL_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("SQL_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for SQL_DATASOURCE_RAW_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_SQL_DATASOURCE_REQUEST_TESTU", getDatasourceIdFromName("SQL_DATASOURCE_REQUEST_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for SQL_DATASOURCE_REQUEST_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_HIVE_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("HIVE_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for HIVE_DATASOURCE_RAW_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_HIVE_DATASOURCE_REQUEST_TESTU", getDatasourceIdFromName("HIVE_DATASOURCE_REQUEST_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for HIVE_DATASOURCE_REQUEST_TESTU")
  # expect_is(createDatasetFromDatasource("DATASET_HBASE_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("HBASE_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for HBASE_DATASOURCE_RAW_TESTU")
  # expect_is(createDatasetFromDatasource("DATASET_HDFS_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("HDFS_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for HDFS_DATASOURCE_RAW_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_S3_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("S3_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for S3_DATASOURCE_RAW_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_SFTP_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("SFTP_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for SFTP_DATASOURCE_RAW_TESTU")
  expect_is(createDatasetFromDatasource("DATASET_FTP_DATASOURCE_RAW_TESTU", getDatasourceIdFromName("FTP_DATASOURCE_RAW_TESTU")), "list", "createDatasetFromDatasource() doesn't retrieve a list for FTP_DATASOURCE_RAW_TESTU")
})

test_that("getDatasetIdFromName", {
  expect_is(getDatasetIdFromName("DATASET_TESTU"), "character", "getDatasetIdFromName() doesn't retrieve a character for DATASET_TESTU")
})

test_that("startDatasetEmbedding", {
  expect(startDatasetEmbedding(getDatasetIdFromName("DATASET_TESTU")) == 200, "startDatasetEmbedding() doesn't retrieve a 200 status code for DATASET_TESTU")
})

test_that("getDatasets", {
  expect_is(getDatasets(), "list", "getDatasets() doesn't retrieve a list")
  expect(length(getDatasets()) >= 1, "getDatasets() doesn't have at least one element")
  expect(length(getDatasets()) >= nb_datasets+1, "The number of datasets has not increased after dataset creation")
})

test_that("getDatasetHead", {
  expect_is(getDatasetHead(getDatasetIdFromName("DATASET_TESTU")), "data.frame", "getDatasetHead() doesn't retrieve a data.frame")
  expect(nrow(getDatasetHead(getDatasetIdFromName("DATASET_TESTU"))) == min(10, n_row), "getDatasetHead() has a bad number of rows")
  expect(ncol(getDatasetHead(getDatasetIdFromName("DATASET_TESTU"))) == n_features+1, "getDatasetHead() has a bad number of features")
})

test_that("createDataframeFromDataset", {
  expect_is(createDataframeFromDataset(getDatasetIdFromName("DATASET_TESTU")), "data.frame", "createDataframeFromDataset() doesn't retrieve a data.frame")
  expect(nrow(createDataframeFromDataset(getDatasetIdFromName("DATASET_TESTU"))) == n_row, "createDataframeFromDataset() has a bad number of rows")
  expect(ncol(createDataframeFromDataset(getDatasetIdFromName("DATASET_TESTU"))) == n_features+1, "createDataframeFromDataset() has a bad number of features")
})

test_that("getDatasetEmbedding", {
  expect_is(getDatasetEmbedding(getDatasetIdFromName("DATASET_TESTU")), "data.table", "getDatasetEmbedding() doesn't a data table for data set DATASET_TESTU")
  expect(nrow(getDatasetEmbedding(getDatasetIdFromName("DATASET_TESTU")) == n_row), "getDatasetEmbedding() has a bad number of row for data set DATASET_TESTU")
})
