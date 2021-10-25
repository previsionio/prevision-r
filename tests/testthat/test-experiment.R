context("experiments")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_experiments = length(get_experiments(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_experiment", {
  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_REGRESSION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "tabular",
                              training_type = "regression"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_REGRESSION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "tabular",
                              training_type = "classification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_classification_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "tabular",
                              training_type = "multiclassification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_TEXTSIM_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "tabular",
                              training_type = "text-similarity"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_TEXTSIM_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_TIMESERIES_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "timeseries",
                              training_type = "regression"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_TIMESERIES_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "images",
                              training_type = "regression"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_IMG_CLASSIFICATION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "images",
                              training_type = "classification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_CLASSIFICATION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_IMG_MULTICLASSIFICATION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "images",
                              training_type = "multiclassification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_MULTICLASSIFICATION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU",
                              provider = "prevision-auto-ml",
                              data_type = "images",
                              training_type = "object-detection"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_EXTERNAL_REGRESSION_TESTU",
                              provider = "external",
                              data_type = "tabular",
                              training_type = "regression"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_EXTERNAL_REGRESSION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_EXTERNAL_CLASSIFICATION_TESTU",
                              provider = "external",
                              data_type = "tabular",
                              training_type = "classification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_EXTERNAL_CLASSIFICATION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU",
                              provider = "external",
                              data_type = "tabular",
                              training_type = "multiclassification"), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU")

  expect_is(create_experiment(project_id = get_project_id_from_name("PROJECT_TESTU"),
                              name = "EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU",
                              provider = "external",
                              data_type = "tabular",
                              training_type = "multiclassification",
                              check_if_exist = TRUE), "list", "create_experiment() doesn't retrieve a list for EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU with check enabled")
})

test_that("create_experiment_version", {
  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_REGRESSION_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_REGRESSION"),
                                      target_column = "TARGET",
                                      normal_models = list("LR", "RF"),
                                      lite_models = list("LR"),
                                      simple_models = list("LR", "DT"),
                                      with_blend = FALSE), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_REGRESSION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_CLASSIFICATION"),
                                      target_column = "TARGET",
                                      normal_models = list("LGB", "XGB")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_MULTICLASSIFICATION"),
                                      target_column = "TARGET",
                                      lite_models = list("CB")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_TIMESERIES_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_TS"),
                                      target_column = "TARGET",
                                      time_column = "TS",
                                      apriori_list = list("X2", "X3"),
                                      start_dw = -2,
                                      end_dw = -1,
                                      start_fw = 1,
                                      end_fw = 2,
                                      features_engineering_selected_list = list("Date"),
                                      lite_models = list("RF")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_TIMESERIES_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_TEXTSIM_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_TEXTSIM_ITEMS"),
                                      id_column = "item_id",
                                      content_column = "item_desc",
                                      metric = "accuracy_at_k",
                                      top_k = 10,
                                      lang = "auto",
                                      queries_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_TEXTSIM_QUERIES"),
                                      queries_dataset_content_column = "query",
                                      queries_dataset_matching_id_description_column = "true_item_id",
                                      models_params = list(
                                        list("model_embedding" = "tf_idf", "preprocessing" = list("word_stemming" = "yes", "ignore_stop_word" = "auto", "ignore_punctuation" = "no"), "models" = list("brute_force", "cluster_pruning")),
                                        list("model_embedding" = "transformer", "preprocessing" = NULL, "models" = list("brute_force", "lsh", "hkm")),
                                        list("model_embedding" = "transformer_fine_tuned", "preprocessing" = NULL, models = list("brute_force", "lsh", "hkm"))
                                      )), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_TEXTSIM_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_IMG"),
                                      folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU"),
                                      target_column = "x1",
                                      filename_column = "PATH",
                                      lite_models = list("XGB")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"),
                                      dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_IMG"),
                                      folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU"),
                                      target_column = "TARGET",
                                      filename_column = "PATH",
                                      xmin = "x1",
                                      xmax = "x2",
                                      ymin = "y2",
                                      ymax = "y1"), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_EXTERNAL_REGRESSION_TESTU"),
                                      holdout_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_EXTERNAL_REGRESSION_TESTU"),
                                      target_column = "TARGET",
                                      name = "EXPERIMENT_EXTERNAL_REGRESSION_TESTU",
                                      onnx_file = paste0(wd, "/tests/testthat/data/regression_model.onnx"),
                                      yaml_file = paste0(wd, "/tests/testthat/data/regression_model.yaml")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_EXTERNAL_REGRESSION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_EXTERNAL_CLASSIFICATION_TESTU"),
                                      holdout_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_EXTERNAL_CLASSIFICATION_TESTU"),
                                      target_column = "TARGET",
                                      name = "EXPERIMENT_EXTERNAL_CLASSIFICATION_TESTU",
                                      onnx_file = paste0(wd, "/tests/testthat/data/classification_model.onnx"),
                                      yaml_file = paste0(wd, "/tests/testthat/data/classification_model.yaml")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_EXTERNAL_CLASSIFICATION_TESTU")

  expect_is(create_experiment_version(experiment_id = get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU"),
                                      holdout_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_EXTERNAL_MULTICLASSIFICATION_TESTU"),
                                      target_column = "TARGET",
                                      name = "EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU",
                                      onnx_file = paste0(wd, "/tests/testthat/data/multiclassification_model.onnx"),
                                      yaml_file = paste0(wd, "/tests/testthat/data/multiclassification_model.yaml")), "list", "get_experiments() doesn't retrieve a list for EXPERIMENT_EXTERNAL_MULTICLASSIFICATION_TESTU")
})

Sys.sleep(300)

test_that("get_experiments", {
  expect_is(get_experiments(get_project_id_from_name("PROJECT_TESTU")), "list", "get_experiments() doesn't retrieve a list")
  expect(length(get_experiments(get_project_id_from_name("PROJECT_TESTU"))) >= 1, "get_experiments() doesn't have at least one element")
  expect(length(get_experiments(get_project_id_from_name("PROJECT_TESTU"))) >= nb_experiments+1, "The number of experiments has not increased after dataset creation")
})

test_that("get_experiment_id_from_name", {
  expect_is(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                        "EXPERIMENT_PREVISION_REGRESSION_TESTU"), "character", "get_experiment_id_from_name() doesn't retrieve a character")
})

test_that("get_experiment_version_id", {
  expect_is(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                  "EXPERIMENT_PREVISION_REGRESSION_TESTU"), 1), "character", "get_experiment_version_id() doesn't retrieve a character")
})

test_that("get_experiment_info", {
  expect_is(get_experiment_info(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                            "EXPERIMENT_PREVISION_REGRESSION_TESTU")), "list", "get_experiment_info() doesn't retrieve a list")
})

test_that("get_experiment_version_features", {
  expect_is(get_experiment_version_features(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                  "EXPERIMENT_PREVISION_REGRESSION_TESTU"))), "list", "get_experiment_version_features() doesn't retrieve a list")
})

test_that("get_features_infos", {
  expect_is(get_features_infos(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                     "EXPERIMENT_PREVISION_REGRESSION_TESTU"), 1),
                               get_experiment_version_features(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "EXPERIMENT_PREVISION_REGRESSION_TESTU")))$items[[1]]$name), "list", "get_features_infos() doesn't retrieve a list")
})

test_that("get_experiment_version_models", {
  expect_is(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                "EXPERIMENT_PREVISION_REGRESSION_TESTU"))), "list", "get_experiment_version_models() doesn't retrieve a list")
})

test_that("get_model_infos", {
  expect_is(get_model_infos(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_model_infos() doesn't retrieve a list")
})

test_that("get_model_hyperparameters", {
  expect_is(get_model_hyperparameters(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_model_hyperparameters() doesn't retrieve a list")
})

test_that("get_model_feature_importance", {
  expect_is(get_model_feature_importance(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                             "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`, "raw"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on raw level")
  expect_is(get_model_feature_importance(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                             "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`, "engineered"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on engineered level")
})

test_that("create_prediction", {
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_REGRESSION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_REGRESSION")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_CLASSIFICATION")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_MULTICLASSIFICATION")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_TIMESERIES_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_TS")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_TEXTSIM_TESTU")),
                              model_id = get_best_model_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                       "EXPERIMENT_PREVISION_TEXTSIM_TESTU")),
                              queries_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                            "DATASET_TEXT_SIM_QUERY_TESTU"),
                              queries_dataset_content_column = "query",
                              top_k = 10), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_IMG"),
                              folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                          "FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect_is(create_prediction(experiment_version_id = get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                            "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_IMG"),
                              folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                          "FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

Sys.sleep(60)

test_that("get_experiment_version_predictions", {
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_REGRESSION_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_TIMESERIES_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_TEXTSIM_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect_is(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU"))), "list", "get_experiment_version_predictions() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

test_that("get_prediction_infos", {
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_TIMESERIES_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_TEXTSIM_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect_is(get_prediction_infos(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                          "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

test_that("get_prediction", {
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_TIMESERIES_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_TEXTSIM_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect_is(get_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")))[[1]]$`_id`, "validation"), "data.frame", "get_prediction() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

test_that("delete_prediction", {
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for  EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_TIMESERIES_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_TEXTSIM_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect(delete_prediction(get_experiment_version_predictions(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

test_that("update_experiment_version_description", {
  expect(update_experiment_version_description(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                     "EXPERIMENT_PREVISION_REGRESSION_TESTU")), "DESCRIPTION") == 200, "update_experiment_description() doesn't retrieve a 200 status code for EXPERIMENT_PREVISION_REGRESSION_TESTU")
})

test_that("get_model_cv", {
  expect_is(get_model_cv(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                             "EXPERIMENT_PREVISION_REGRESSION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(get_model_cv(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                             "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(get_model_cv(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                             "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(get_model_cv(get_experiment_version_models(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                             "EXPERIMENT_PREVISION_TIMESERIES_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
})

test_that("get_best_model_id", {
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_REGRESSION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_REGRESSION_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_CLASSIFICATION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_CLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_MULTICLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_TIMESERIES_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_TIMESERIES_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_TEXTSIM_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_TEXTSIM_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU")
  expect_is(get_best_model_id(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for EXPERIMENT_PREVISION_IMG_REGRESSION_TESTU")
})

test_that("pause_experiment_version", {
  expect(pause_experiment_version(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                        "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"))) == 200, "pause_experiment_version() doesn't retrieve a 200 status code")
})

Sys.sleep(10)

test_that("resume_experiment_version", {
  expect(resume_experiment_version(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                         "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"))) == 200, "resume_experiment_version() doesn't retrieve a 200 status code")
})

Sys.sleep(10)

test_that("stop_experiment_version", {
  expect(stop_experiment_version(get_experiment_version_id(get_experiment_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                       "EXPERIMENT_PREVISION_IMG_OBJECT_DETECTION_TESTU"))) == 200, "stop_experiment_version() doesn't a 200 status code")
})
