library(testthat)
library(data.table)
library(previsionio)

#test_check("previsionio")

test_file("tests/testthat/test-connector.R")
test_file("tests/testthat/test-datasource.R")
test_file("tests/testthat/test-folder.R")
test_file("tests/testthat/test-dataset.R")
test_file("tests/testthat/test-usecase.R")
test_file("tests/testthat/test-app.R")
test_file("tests/testthat/test-user.R")
test_file("tests/testthat/test-scheduler.R")

test_file("tests/testthat/test-usecase-delete.R")
test_file("tests/testthat/test-dataset-delete.R")
test_file("tests/testthat/test-datasource-delete.R")
test_file("tests/testthat/test-connector-delete.R")
