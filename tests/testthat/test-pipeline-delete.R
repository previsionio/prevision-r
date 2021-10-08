context("Pipeline delete")

source("helper-credentials.R")

test_that("delete_pipeline", {
  # expect(delete_pipeline(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_COMPONENT_TESTU", "component"), "component") == 204, "delete_pipeline() doesn't retrieve a 204 status code for PIPELINE_COMPONENT_TESTU")
  expect(delete_pipeline(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_TEMPLATE_TESTU", "template"), "template") == 204, "delete_pipeline() doesn't retrieve a 204 status code for PIPELINE_COMPONENT_TESTU")
  expect(delete_pipeline(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_RUN_TESTU", "run"), "run") == 204, "delete_pipeline() doesn't retrieve a 204 status code for PIPELINE_COMPONENT_TESTU")
})
