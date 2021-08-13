context("Deployment")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_model_deployed = length(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "model"))
nb_app_deployed = length(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "app"))

test_that("create_deployment_model", {
  expect_is(create_deployment_model(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                    name = "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED",
                                    usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU"),
                                    main_model_usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")),
                                    access_type = "fine_grained",
                                    description = "DESCRIPTION",
                                    main_model_id = get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))), "list", "create_deployment_model() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")

  expect_is(create_deployment_model(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                    name = "MODEL_DEPLOYMENT_TESTU_MAIN_PRIVATE",
                                    usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU"),
                                    main_model_usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")),
                                    access_type = "private",
                                    description = "DESCRIPTION",
                                    main_model_id = get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))), "list", "create_deployment_model() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_PRIVATE")

  expect_is(create_deployment_model(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                    name = "MODEL_DEPLOYMENT_TESTU_MAIN_PUBLIC",
                                    usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU"),
                                    main_model_usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")),
                                    access_type = "public",
                                    description = "DESCRIPTION",
                                    main_model_id = get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))), "list", "create_deployment_model() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_PUBLIC")

  expect_is(create_deployment_model(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                    name = "MODEL_DEPLOYMENT_TESTU_MAIN_CHALLENGER_PUBLIC",
                                    usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU"),
                                    main_model_usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")),
                                    challenger_model_usecase_version_id = get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))[[1]]$`_id`,
                                    access_type = c("public"),
                                    main_model_id = get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))), "list", "create_deployment_model() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_CHALLENGER_PUBLIC")
})

test_that("create_deployment_app", {
  expect_is(create_deployment_app(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                  name = "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED",
                                  git_url = "gitlab.com/prevision-app/rte-electricity-forecast.git",
                                  git_branch = "master",
                                  type = "r",
                                  broker = "gitlabrepositories",
                                  access_type = "fine_grained"), "list", "create_deployment_app() doesn't retrieve a list for APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED")

  expect_is(create_deployment_app(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                  name = "APP_DEPLOYMENT_TESTU_R_2_256_2_PUBLIC",
                                  git_url = "gitlab.com/prevision-app/rte-electricity-forecast.git",
                                  git_branch = "master",
                                  type = "r",
                                  broker = "gitlabrepositories",
                                  app_cpu = 2,
                                  app_ram = "256Mi",
                                  app_replica_count = 2,
                                  access_type = "public"), "list", "create_deployment_app() doesn't retrieve a list for APP_DEPLOYMENT_TESTU_R_2_256_2_PUBLIC")

  expect_is(create_deployment_app(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                  name = "APP_DEPLOYMENT_TESTU_R_ENVVAR_PRIVATE",
                                  git_url = "gitlab.com/prevision-app/rte-electricity-forecast.git",
                                  git_branch = "master",
                                  type = "r",
                                  broker = "gitlabrepositories",
                                  access_type = "private",
                                  env_vars = jsonlite::toJSON(list(list("var1" = "val1"),
                                                                   list("var2" = "val2")))), "list", "create_deployment_app() doesn't retrieve a list for APP_DEPLOYMENT_TESTU_R_ENVVAR_PRIVATE")
})

Sys.sleep(300)

test_that("get_deployments", {
  expect_error(get_deployments(get_project_id_from_name("PROJECT_TESTU")), info = "get_deployments() needs a type argument")
  expect_error(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "bonjour"), info = "get_deployments() needs a valid type argument")
  expect_is(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "model"), "list", "get_deployments() doesn't retrieve a list of deployed models")
  expect_is(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "app"), "list", "get_deployments() doesn't retrieve a list of deployed apps")
  expect(length(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "model")) > nb_model_deployed, "The number of deployed models has not increased after model deployment creation")
  expect(length(get_deployments(get_project_id_from_name("PROJECT_TESTU"), "app")) > nb_app_deployed, "The number of deployed apps has not increased after app deployment creation")
})

test_that("get_deployment_id_from_name", {
  expect_error(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED"), info = "get_deployment_id_from_name() needs a type argument")
  expect_error(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "bonjour"), info = "get_deployment_id_from_name() needs a valid type argument")
  expect_is(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"), "character", "get_deployment_id_from_name() doesn't retrieve a character for a deployed model")
  expect_is(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app"), "character", "get_deployment_id_from_name() doesn't retrieve a character for a deployed app")
})

test_that("get_deployment_info", {
  expect_is(get_deployment_info(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")), "list", "get_deployment_info() doesn't retrieve a list for a deployed model")
  expect_is(get_deployment_info(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app")), "list", "get_deployment_info() doesn't retrieve a list for a deployed app")
})

# Logs can take lot of time to come...
test_that("get_deployment_app_logs", {
  expect_error(get_deployment_app_logs(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app"), "bonjour"), info = "get_deployment_app_logs() needs a valid type argument")
  # expect_is(get_deployment_app_logs(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app"), "build"), "character", "get_deployment_app_logs() doesn't retrieve a character for build logs of a deployed app")
  # expect_is(get_deployment_app_logs(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app"), "deploy"), "character", "get_deployment_app_logs() doesn't retrieve a character for deploy logs of a deployed app")
  # expect_is(get_deployment_app_logs(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "APP_DEPLOYMENT_TESTU_R_1_128_1_FINE_GRAINED", "app"), "run"), "character", "get_deployment_app_logs() doesn't retrieve a character for run logs of a deployed app")
})

test_that("get_deployment_api_keys", {
  expect_is(get_deployment_api_keys(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")), "data.frame", "get_deployment_api_keys() doesn't retrieve a data frame for a deployed model")
  nb_api_keys = length(get_deployment_api_keys(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")))
})

test_that("create_deployment_api_key", {
  expect_is(create_deployment_api_key(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")), "data.frame", "get_deployment_info() doesn't retrieve a data frame for a deployed model")
  expect(length(get_deployment_api_keys(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"))) > nb_api_keys, "The number of api keys has not increased after api key creation")
})

test_that("get_deployment_predictions", {
  expect_is(get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")), "list", "get_deployment_predictions() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
})

test_that("create_deployment_predictions", {
  expect_is(create_deployment_predictions(deployment_id = get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"),
                                          dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_REGRESSION")), "list", "create_deployment_predictions() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
})

test_that("get_deployment_prediction_info", {
  expect_is(get_deployment_prediction_info(get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"))[[1]]$`_id`), "list", "get_deployment_prediction_info() doesn't retrieve a list for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
})

test_that("get_prediction", {
  expect_is(get_prediction(prediction_id = get_deployment_predictions(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"))[[1]]$`_id`,
                           prediction_type = "deployment"), "data.frame", "get_prediction() doesn't retrieve a data.frame for MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED")
})

test_that("get_deployment_usage", {
  expect_error(get_deployment_usage(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model")), info = "get_deployment_usage() needs a type argument")
  expect_error(get_deployment_usage(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"), "bonjour"), info = "get_deployment_usage() needs a valid type argument")
  expect_is(get_deployment_usage(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"), "calls"), "list", "get_deployment_usage() doesn't retrieve a list for calls monitoring of a deployed model")
  expect_is(get_deployment_usage(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"), "errors"), "list", "get_deployment_usage() doesn't retrieve a list for errors monitoring of a deployed model")
  expect_is(get_deployment_usage(get_deployment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "MODEL_DEPLOYMENT_TESTU_MAIN_FINEGRAINED", "model"), "response_time"), "list", "get_deployment_usage() doesn't retrieve a list for response_time monitoring of a deployed model")
})
