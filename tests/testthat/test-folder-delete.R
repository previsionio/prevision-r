context("Folder delete")

source("helper-credentials.R")

test_that("delete_folder", {
  expect(delete_folder(get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU")) == 200, "delete_folder() doesn't retrieve a 200 status code for DATASET_TESTU")
})
