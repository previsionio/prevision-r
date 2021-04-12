context("Project delete")

source("helper-credentials.R")

test_that("delete_project", {
  expect(delete_project(get_project_id_from_name("PROJECT_TESTU")) == 204, "delete_project() doesn't retrieve a 200 status code for PROJECT_TESTU")
})
