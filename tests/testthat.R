library(testthat)
library(data.table)
library(previsionio)

#test_check("previsionio")

wd = getwd()

test_file(paste0(wd, "/tests/testthat/test-project.R"))
test_file(paste0(wd, "/tests/testthat/test-connector.R"))
test_file(paste0(wd, "/tests/testthat/test-datasource.R"))
test_file(paste0(wd, "/tests/testthat/test-folder.R"))
test_file(paste0(wd, "/tests/testthat/test-dataset.R"))
test_file(paste0(wd, "/tests/testthat/test-usecase.R"))
test_file(paste0(wd, "/tests/testthat/test-pipeline.R"))

test_file(paste0(wd, "/tests/testthat/test-pipeline-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-usecase-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-dataset-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-folder-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-datasource-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-connector-delete.R"))
test_file(paste0(wd, "/tests/testthat/test-project-delete.R"))
