context("Folder")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_folders = length(get_folders(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_folder", {
  expect_is(create_folder(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU", paste0(wd, "/tests/testthat/data/img.zip")), "list", "create_folder() doesn't retrieve a list")
})

test_that("get_folders", {
  expect_is(get_folders(get_project_id_from_name("PROJECT_TESTU")), "list", "get_folders() doesn't retrieve a list")
  expect(length(get_folders(get_project_id_from_name("PROJECT_TESTU"))) > nb_folders, "The number of folders has not increased after folders creation")
})

test_that("get_folder_id_from_name", {
  expect_is(get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU"), "character", "get_folder_id_from_name() doesn't retrieve a character for FOLDER_TESTU")
})

test_that("get_folder", {
  expect_is(get_folder(get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU")), "list", "get_folder() doesn't retrieve a list")
})
