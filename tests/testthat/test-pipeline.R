context("Pipeline")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
# nb_pipeline_components = length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "component"))
nb_pipeline_templates = length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "template"))
nb_pipeline_runs = length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "run"))

test_that("create_pipeline", {
  # expect_is(create_pipeline(project_id = get_project_id_from_name("PROJECT_TESTU"),
  #                           type = "component",
  #                           name = "PIPELINE_COMPONENT_TESTU"), "list", "create_pipeline() doesn't retrieve a list for component")
  expect_is(create_pipeline(project_id = get_project_id_from_name("PROJECT_TESTU"),
                            type = "template",
                            name = "PIPELINE_TEMPLATE_TESTU"), "list", "create_pipeline() doesn't retrieve a list for component")
  expect_is(create_pipeline(project_id = get_project_id_from_name("PROJECT_TESTU"),
                            type = "run",
                            name = "PIPELINE_RUN_TESTU",
                            pipeline_template_id = get_pipeline_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "PIPELINE_TEMPLATE_TESTU", "template")), "list", "create_pipeline() doesn't retrieve a list for component")

})

test_that("get_pipelines", {
  expect_error(get_pipelines(get_project_id_from_name("PROJECT_TESTU")), info = "get_pipelines() needs a type argument")
  expect_error(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "bonjour"), info = "get_pipelines() needs a valid type argument")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "component"), "list", "get_pipelines() doesn't retrieve a list for components")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "template"), "list", "get_pipelines() doesn't retrieve a list for templates")
  expect_is(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "run"), "list", "get_pipelines() doesn't retrieve a list for runs")
  # expect(length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "component")) > nb_pipeline_components, "The number of pipeline components has not increased after pipeline component creation")
  expect(length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "template")) > nb_pipeline_templates, "The number of pipeline templates has not increased after pipeline template creation")
  expect(length(get_pipelines(get_project_id_from_name("PROJECT_TESTU"), "run")) > nb_pipeline_runs, "The number of pipeline runs has not increased after pipeline run creation")
})

test_that("get_pipeline_info", {
  # expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
  #                                                                     pipeline_name = "PIPELINE_COMPONENT_TESTU",
  #                                                                     type = "component"),
  #                             type = "component"), "list", "get_pipeline_info() doesn't retrieve a list for a component")
  expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                      pipeline_name = "PIPELINE_TEMPLATE_TESTU",
                                                                      type = "template"),
                              type = "template"), "list", "get_pipeline_info() doesn't retrieve a list for a template")
  expect_is(get_pipeline_info(pipeline_id = get_pipeline_id_from_name(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                      pipeline_name = "PIPELINE_RUN_TESTU",
                                                                      type = "run"),
                              type = "run"), "list", "get_pipeline_info() doesn't retrieve a list for a run")
})
