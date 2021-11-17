context("Pipeline")

source("helper-credentials.R")

test_that("get_pipelines", {
  expect_error(get_pipelines(get_project_id_from_name("PROJECT_TESTU")), info = "get_pipelines() needs a type argument")
  expect_error(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "bonjour"), info = "get_pipelines() needs a valid type argument")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "component"), "list", "get_pipelines() doesn't retrieve a list for components")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "template"), "list", "get_pipelines() doesn't retrieve a list for templates")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "run"), "list", "get_pipelines() doesn't retrieve a list for runs")
})

# test_that("get_pipeline_id_from_name", {
#   expect_error(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_TEMPLATE_TESTU"), info = "get_pipeline_id_from_name() needs a type argument")
#   expect_error(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_TEMPLATE_TESTU", "bonjour"), info = "get_pipeline_id_from_name() needs a valid type argument")
#   expect_is(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_COMPONENT_TESTU", "component"), "character", "get_pipeline_id_from_name() doesn't retrieve a character for PIPELINE_COMPONENT_TESTU")
#   expect_is(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_TEMPLATE_TESTU", "template"), "character", "get_pipeline_id_from_name() doesn't retrieve a character for PIPELINE_TEMPLATE_TESTU")
#   expect_is(get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_RUN_TESTU", "run"), "character", "get_pipeline_id_from_name() doesn't retrieve a character for a PIPELINE_RUN_TESTU")
# })

# test_that("get_pipeline_info", {
#   expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
#                                                                       name = "PIPELINE_COMPONENT_TESTU",
#                                                                       type = "component"),
#                               type = "component"), "list", "get_pipeline_info() doesn't retrieve a list for a component")
#   expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
#                                                                       name = "PIPELINE_TEMPLATE_TESTU",
#                                                                       type = "template"),
#                               type = "template"), "list", "get_pipeline_info() doesn't retrieve a list for a template")
#   expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
#                                                                       name = "PIPELINE_RUN_TESTU",
#                                                                       type = "run"),
#                               type = "run"), "list", "get_pipeline_info() doesn't retrieve a list for a run")
# })

# test_that("create_pipeline_trigger", {
#   expect(create_pipeline_trigger(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
#                                                                          name = "PIPELINE_RUN_TESTU",
#                                                                          type = "run")) == 200, "create_pipeline_trigger() doesn't retrieve a list for a run")
# })
