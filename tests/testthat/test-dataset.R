context("Dataset")

source("helper-credentials.R")
source("helper-dataset.R")

# INIT GLOBAL VARIABLE
nb_datasets = length(get_datasets())
n_row       = 1000
n_features  = 10

test_that("create_dataset_from_dataframe", {
  expect_is(create_dataset_from_dataframe("DATASET_TESTU", tabular_dataset(type_problem = "regression", n_row = n_row, n_features = n_features), F), "list", "create_dataset_from_dataframe() doesn't retrieve a list for DATASET_TESTU if not zipped")
  expect_is(create_dataset_from_dataframe("DATASET_TESTU_ZIPPED", tabular_dataset(type_problem = "regression", n_row = n_row, n_features = n_features), T), "list", "create_dataset_from_dataframe() doesn't retrieve a list for DATASET_TESTU_ZIPPED if zipped")
})

test_that("create_dataset_from_datasource", {
  expect_is(create_dataset_from_datasource("DATASET_SQL_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("SQL_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for SQL_DATASOURCE_RAW_TESTU")
  expect_is(create_dataset_from_datasource("DATASET_SQL_DATASOURCE_REQUEST_TESTU", get_datasource_id_from_name("SQL_DATASOURCE_REQUEST_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for SQL_DATASOURCE_REQUEST_TESTU")
  expect_is(create_dataset_from_datasource("DATASET_HIVE_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("HIVE_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for HIVE_DATASOURCE_RAW_TESTU")
  expect_is(create_dataset_from_datasource("DATASET_HIVE_DATASOURCE_REQUEST_TESTU", get_datasource_id_from_name("HIVE_DATASOURCE_REQUEST_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for HIVE_DATASOURCE_REQUEST_TESTU")
  # expect_is(create_dataset_from_datasource("DATASET_HBASE_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("HBASE_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for HBASE_DATASOURCE_RAW_TESTU")
  # expect_is(create_dataset_from_datasource("DATASET_HDFS_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("HDFS_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for HDFS_DATASOURCE_RAW_TESTU")
  expect_is(create_dataset_from_datasource("DATASET_S3_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("S3_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for S3_DATASOURCE_RAW_TESTU")
  # expect_is(create_dataset_from_datasource("DATASET_SFTP_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("SFTP_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for SFTP_DATASOURCE_RAW_TESTU")
  # expect_is(create_dataset_from_datasource("DATASET_FTP_DATASOURCE_RAW_TESTU", get_datasource_id_from_name("FTP_DATASOURCE_RAW_TESTU")), "list", "create_dataset_from_datasource() doesn't retrieve a list for FTP_DATASOURCE_RAW_TESTU")
})

test_that("get_dataset_id_from_name", {
  expect_is(get_dataset_id_from_name("DATASET_TESTU"), "character", "get_dataset_id_from_name() doesn't retrieve a character for DATASET_TESTU")
})

test_that("create_dataset_embedding", {
  expect(create_dataset_embedding(get_dataset_id_from_name("DATASET_TESTU")) == 200, "create_dataset_embedding() doesn't retrieve a 200 status code for DATASET_TESTU")
})

test_that("get_datasets", {
  expect_is(get_datasets(), "list", "get_datasets() doesn't retrieve a list")
  expect(length(get_datasets()) >= 1, "get_datasets() doesn't have at least one element")
  expect(length(get_datasets()) >= nb_datasets+1, "The number of datasets has not increased after dataset creation")
})

test_that("get_dataset_head", {
  expect_is(get_dataset_head(get_dataset_id_from_name("DATASET_TESTU")), "data.frame", "get_dataset_head() doesn't retrieve a data.frame")
  expect(nrow(get_dataset_head(get_dataset_id_from_name("DATASET_TESTU"))) == min(10, n_row), "get_dataset_head() has a bad number of rows")
  expect(ncol(get_dataset_head(get_dataset_id_from_name("DATASET_TESTU"))) == n_features+1, "get_dataset_head() has a bad number of features")
})

test_that("create_dataframe_from_dataset", {
  expect_is(create_dataframe_from_dataset(get_dataset_id_from_name("DATASET_TESTU")), "data.frame", "create_dataframe_from_dataset() doesn't retrieve a data.frame")
  expect(nrow(create_dataframe_from_dataset(get_dataset_id_from_name("DATASET_TESTU"))) == n_row, "create_dataframe_from_dataset() has a bad number of rows")
  expect(ncol(create_dataframe_from_dataset(get_dataset_id_from_name("DATASET_TESTU"))) == n_features+1, "create_dataframe_from_dataset() has a bad number of features")
})

Sys.sleep(30)

test_that("get_dataset_embedding", {
  expect_is(get_dataset_embedding(get_dataset_id_from_name("DATASET_TESTU")), "data.table", "get_dataset_embedding() doesn't a data table for data set DATASET_TESTU")
  expect(nrow(get_dataset_embedding(get_dataset_id_from_name("DATASET_TESTU")) == n_row), "get_dataset_embedding() has a bad number of row for data set DATASET_TESTU")
})
