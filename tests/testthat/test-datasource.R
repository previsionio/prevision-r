context("Datasource")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_datasources = length(get_datasources(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_datasource", {
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_CONNECTOR_TESTU"),
                              name = "SQL_DATASOURCE_RAW_TESTU",
                              database = SQL_DATASOURCE_BASE,
                              table = SQL_DATASOURCE_TABLE), "list", "create_datasource() doesn't retrieve a list for SQL connector type without request")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_CONNECTOR_TESTU"),
                              name = "SQL_DATASOURCE_REQUEST_TESTU",
                              database = SQL_DATASOURCE_BASE,
                              request = SQL_DATASOURCE_REQUEST), "list", "create_datasource() doesn't retrieve a list for SQL connector type with request")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_CONNECTOR_TESTU"),
                              name = "HIVE_DATASOURCE_RAW_TESTU",
                              database = HIVE_DATASOURCE_BASE,
                              table = HIVE_DATASOURCE_TABLE), "list", "create_datasource() doesn't retrieve a list for HIVE connector type without request")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_CONNECTOR_TESTU"),
                              name = "HIVE_DATASOURCE_REQUEST_TESTU",
                              database = HIVE_DATASOURCE_BASE,
                              request = HIVE_DATASOURCE_REQUEST), "list", "create_datasource() doesn't retrieve a list for HIVE connector type with request")
  # expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
  #                            connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HBASE_CONNECTOR_TESTU"),
  #                            name = "HBASE_DATASOURCE_RAW_TESTU",
  #                            table = HBASE_DATASOURCE_TABLE), "list", "create_datasource() doesn't retrieve a list for HBASE connector type without request")
  # expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
  #                            connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HDFS_CONNECTOR_TESTU"),
  #                            name = "HDFS_DATASOURCE_RAW_TESTU",
  #                            path = HDFS_DATASOURCE_PATH), "list", "create_datasource() doesn't retrieve a list for HBASE connector type with path")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_CONNECTOR_TESTU"),
                              name = "S3_DATASOURCE_RAW_TESTU",
                              bucket = S3_DATASOURCE_BUCKET,
                              path = S3_DATASOURCE_PATH), "list", "create_datasource() doesn't retrieve a list for S3 connector type with bucket and path")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_CONNECTOR_TESTU"),
                              name = "SFTP_DATASOURCE_RAW_TESTU",
                              path = SFTP_DATASOURCE_PATH), "list", "create_datasource() doesn't retrieve a list for SFTP connector type with path")
  expect_is(create_datasource(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              connector_id = get_connector_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_CONNECTOR_TESTU"),
                              name = "FTP_DATASOURCE_RAW_TESTU",
                              path = FTP_DATASOURCE_PATH), "list", "create_datasource() doesn't retrieve a list for FTP connector type with path")
})

test_that("get_datasources", {
  expect_is(get_datasources(get_project_id_from_name("PROJECT_TESTU")), "list", "get_datasources() doesn't retrieve a list")
  expect(length(get_datasources(get_project_id_from_name("PROJECT_TESTU"))) > nb_datasources, "The number of datasources has not increased after datasources creation")
})

test_that("get_datasource_id_from_name", {
  expect_is(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), get_datasources(get_project_id_from_name("PROJECT_TESTU"))[[1]]$name), "character", "get_datasource_id_from_name() doesn't retrieve a character")
})

test_that("get_datasource_info", {
  expect_is(get_datasource_info(get_datasources(get_project_id_from_name("PROJECT_TESTU"))[[1]]$`_id`), "list", "get_datasource_info() doesn't retrieve a list")
})

test_that("test_datasource", {
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_RAW_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SQL_DATASOURCE_REQUEST_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for SQL_DATASOURCE_REQUEST_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_RAW_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HIVE_DATASOURCE_REQUEST_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for HIVE_DATASOURCE_REQUEST_TESTU")
  # expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HBASE_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for HBASE_DATASOURCE_RAW_TESTU")
  # expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "HDFS_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for HDFS_DATASOURCE_RAW_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "S3_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for S3_DATASOURCE_RAW_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "SFTP_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for SFTP_DATASOURCE_RAW_TESTU")
  expect(test_datasource(get_datasource_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FTP_DATASOURCE_RAW_TESTU")) == 200, "test_datasource() doesn't retrieve a 200 status code for FTP_DATASOURCE_RAW_TESTU")
})
