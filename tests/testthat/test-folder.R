context("Folder")

source("helper-credentials.R")

test_that("create_folder", {
  expect_is(create_folder("FOLDER_TESTU", paste0(wd, "/data/img.zip")), "list", "create_folder() doesn't retrieve a list")
})

test_that("get_folders", {
  expect_is(get_folders(), "list", "get_folders() doesn't retrieve a list")
})

test_that("get_folder_id_from_name", {
  expect_is(get_folder_id_from_name("FOLDER_TESTU"), "character", "get_folder_id_from_name() doesn't retrieve a character for FOLDER_TESTU")
})

test_that("get_folder", {
  expect_is(get_folder(get_folder_id_from_name("FOLDER_TESTU")), "list", "get_folder() doesn't retrieve a list")
})
