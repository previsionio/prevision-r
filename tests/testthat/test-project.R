context("Project")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_projects = length(get_projects())

test_that("create_project", {
  expect_error(create_project(name = "PROJECT_TESTU",
                              description = "DESCRIPTION_TESTU",
                              color = "#000000"), info = "create_project() doesn't retrieve an error for a incorrect color in PROJECT_TESTU")
  expect_is(create_project(name = "PROJECT_TESTU",
                           description = "DESCRIPTION_TESTU",
                           color = "#a748f5"), "list", "create_project() doesn't retrieve a list for PROJECT_TESTU")
})

test_that("get_projects", {
  expect_is(get_projects(), "list", "get_projects() doesn't retrieve a list")
  expect(length(get_projects()) >= 1, "get_projects() doesn't have at least one element")
  expect(length(get_projects()) >= nb_projects+1, "The number of usecases has not increased after dataset creation")
})

test_that("get_project_id_from_name", {
  expect_is(get_project_id_from_name("PROJECT_TESTU"), "character", "get_project_id_from_name() doesn't retrieve a character")
})

test_that("get_project_info", {
  expect_is(get_project_info(get_project_id_from_name("PROJECT_TESTU")), "list", "get_project_info() doesn't retrieve a list")
})

test_that("get_project_users", {
  expect_is(get_project_users(get_project_id_from_name("PROJECT_TESTU")), "list", "get_project_users() doesn't retrieve a list")
})

test_that("create_project_user", {
  expect_is(create_project_user(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                user_mail = "gerome.pistre@prevision.io",
                                user_role = "viewer"), "list", "create_project_user() doesn't retrieve a list")
})

test_that("update_project_user_role", {
  expect_is(update_project_user_role(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                     user_id = get_project_users(get_project_id_from_name("PROJECT_TESTU"))[[2]]$id,
                                     user_role = "admin"), "list", "update_project_user_role() doesn't retrieve a list")
})

test_that("delete_project_user", {
  expect(delete_project_user(project_id = get_project_id_from_name("PROJECT_TESTU"),
                             user_id = get_project_users(get_project_id_from_name("PROJECT_TESTU"))[[2]]$id) == 200, "delete_project_user() doesn't retrieve a 200 status code")
})
