context("Deployment delete")

source("helper-credentials.R")

test_that("delete_deployment", {
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_PRIVATE")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_PUBLIC")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
  expect(delete_deployment(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_CHALLENGER_PUBLIC")) == 204, "delete_deployment() doesn't retrieve a 204 status code for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
})
