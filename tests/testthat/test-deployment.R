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
