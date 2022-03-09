context("Deployment alert")

source("helper-credentials.R")

test_that("get_deployment_alerts", {
  expect_is(get_deployment_alerts(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                              "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED",
                                                              "model")), "list", "get_deployments() doesn't retrieve a list")
})
