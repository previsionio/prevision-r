context("Deployment delete")

source("helper-credentials.R")

test_that("delete_deployment", {
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_PRIVATE", "model")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_PUBLIC", "model")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_CHALLENGER_PUBLIC", "model")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app")) == 204, "delete_deployment() doesn't retrieve a 204 status code for APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_2_256_2_PUBLIC", "app")) == 204, "delete_deployment() doesn't retrieve a 204 status code for APP_DEPLOYMENT_TESTU_R_2_256_2_PUBLIC")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_ENVVAR_PRIVATE", "app")) == 204, "delete_deployment() doesn't retrieve a 204 status code for APP_DEPLOYMENT_TESTU_R_ENVVAR_PRIVATE")
})
