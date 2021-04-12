context("Connector")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_connectors = length(get_connectors())

test_that("create_connector", {
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "SQL",
                             name = "SQL_CONNECTOR_TESTU",
                             host = SQL_CONNECTOR_HOST,
                             port = SQL_CONNECTOR_PORT,
                             username = SQL_CONNECTOR_USER,
                             password = SQL_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for SQL type")
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "HIVE",
                             name = "HIVE_CONNECTOR_TESTU",
                             host = HIVE_CONNECTOR_HOST,
                             port = HIVE_CONNECTOR_PORT,
                             username = HIVE_CONNECTOR_USER,
                             password = HIVE_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for HIVE type")
  # expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
  #                           type = "HBASE",
  #                           name = "HBASE_CONNECTOR_TESTU",
  #                           host = HBASE_CONNECTOR_HOST,
  #                           port = HBASE_CONNECTOR_PORT,
  #                           username = HBASE_CONNECTOR_USER,
  #                           password = HBASE_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for HBASE type")
  # expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
  #                           type = "HDFS",
  #                           name = "HDFS_CONNECTOR_TESTU",
  #                           host = HDFS_CONNECTOR_HOST,
  #                           port = HDFS_CONNECTOR_PORT,
  #                           username = HDFS_CONNECTOR_USER,
  #                           password = HDFS_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for HDFS type")
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "S3",
                             name = "S3_CONNECTOR_TESTU",
                             host = "",
                             port = "",
                             username = S3_CONNECTOR_USER,
                             password = S3_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for S3 type")
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "SFTP",
                             name = "SFTP_CONNECTOR_TESTU",
                             host = SFTP_CONNECTOR_HOST,
                             port = SFTP_CONNECTOR_PORT,
                             username = SFTP_CONNECTOR_USER,
                             password = SFTP_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for SFTP type")
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "FTP",
                             name = "FTP_CONNECTOR_TESTU",
                             host = FTP_CONNECTOR_HOST,
                             port = FTP_CONNECTOR_PORT,
                             username = FTP_CONNECTOR_USER,
                             password = FTP_CONNECTOR_PASS), "list", "create_connector() doesn't retrieve a list for FTP type")
  expect_is(create_connector(get_project_id_from_name("PROJECT_TESTU"),
                             type = "GCP",
                             name = "GCP_CONNECTOR_TESTU",
                             host = "",
                             port = "",
                             username = "",
                             password = "",
                             google_credentials = GCP_CREDENTIALS), "list", "create_connector() doesn't retrieve a list for GCP type")
})

test_that("get_connectors", {
  expect_is(get_connectors(get_project_id_from_name("PROJECT_TESTU"),), "list", "get_connectors() doesn't retrieve a list")
  expect(length(get_connectors(get_project_id_from_name("PROJECT_TESTU"),)) > nb_connectors, "The number of connectors has not increased after connectors creation")
})

test_that("get_connector_info", {
  expect_is(get_connector_info(get_connectors()[1]$`_id`), "list", "get_connector_info() doesn't retrieve a list")
})

test_that("get_connector_id_from_name", {
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for SQL_CONNECTOR_TESTU")
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for HIVE_CONNECTOR_TESTU")
  # expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HBASE_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for HBASE_CONNECTOR_TESTU")
  # expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HDFS_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for HDFS_CONNECTOR_TESTU")
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for S3_CONNECTOR_TESTU")
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for SFTP_CONNECTOR_TESTU")
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for FTP_CONNECTOR_TESTU")
  expect_is(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "GCP_CONNECTOR_TESTU"), "character", "get_connector_id_from_name() doesn't retrieve a character for GCP_CONNECTOR_TESTU")
})

test_that("test_connector", {
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for SQL_CONNECTOR_TESTU")
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for HIVE_CONNECTOR_TESTU")
  # expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HBASE_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for HBASE_CONNECTOR_TESTU")
  # expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HDFS_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for HDFS_CONNECTOR_TESTU")
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for S3_CONNECTOR_TESTU")
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for SFTP_CONNECTOR_TESTU")
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for FTP_CONNECTOR_TESTU")
  expect(test_connector(get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "GCP_CONNECTOR_TESTU")) == 200, "test_connector() doesn't retrieve a 200 status code for GCP_CONNECTOR_TESTU")
})
