context("Dataset delete")

source("helper-credentials.R")

test_that("deleteDataset", {
  expect(deleteDataset(getDatasetIdFromName("DATASET_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_SQL_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_SQL_DATASOURCE_RAW_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_SQL_DATASOURCE_REQUEST_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_SQL_DATASOURCE_REQUEST_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_HIVE_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_HIVE_DATASOURCE_RAW_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_HIVE_DATASOURCE_REQUEST_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_HIVE_DATASOURCE_REQUEST_TESTU")
  # expect(deleteDataset(getDatasetIdFromName("DATASET_HBASE_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_HBASE_DATASOURCE_RAW_TESTU")
  # expect(deleteDataset(getDatasetIdFromName("DATASET_HDFS_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_HDFS_DATASOURCE_RAW_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_S3_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_S3_DATASOURCE_RAW_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_SFTP_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_SFTP_DATASOURCE_RAW_TESTU")
  expect(deleteDataset(getDatasetIdFromName("DATASET_FTP_DATASOURCE_RAW_TESTU")) == 200, "deleteDataset() doesn't retrieve a 200 status code for DATASET_FTP_DATASOURCE_RAW_TESTU")
})
