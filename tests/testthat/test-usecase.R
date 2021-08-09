context("Usecases")

source("helper-credentials.R")

# INIT GLOBAL VARIABLE
nb_usecases = length(get_usecases(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_usecase", {
  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_REGRESSION_TESTU",
                           data_type = "tabular",
                           training_type = "regression",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_REGRESSION"),
                           target_column = "TARGET",
                           normal_models = list("LR", "RF"),
                           lite_models = list("LR"),
                           simple_models = list("LR", "DT"),
                           with_blend = F), "list", "get_usecases() doesn't retrieve a list for USECASE_REGRESSION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_CLASSIFICATION_TESTU",
                           data_type = "tabular",
                           training_type = "classification",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_CLASSIFICATION"),
                           target_column = "TARGET",
                           normal_models = list("LGB", "XGB")), "list", "get_usecases() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_MULTICLASSIFICATION_TESTU",
                           data_type = "tabular",
                           training_type = "multiclassification",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_MULTICLASSIFICATION"),
                           target_column = "TARGET",
                           lite_models = list("CB")), "list", "get_usecases() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_TIMESERIES_TESTU",
                           data_type = "timeseries",
                           training_type = "regression",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_TS"),
                           target_column = "TARGET",
                           time_column = "TS",
                           start_dw = -2,
                           end_dw = -1,
                           start_fw = 1,
                           end_fw = 2,
                           features_engineering_selected_list = list("Date"),
                           lite_models = list("RF")), "list", "get_usecases() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_TEXT_SIM_TESTU",
                           data_type = "tabular",
                           training_type = "text-similarity",
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
                             list("model_embedding" = "transformer", "preprocessing" = NA, "models" = list("brute_force", "lsh", "hkm")),
                             list("model_embedding" = "transformer_fine_tuned", "preprocessing" = NA, models = list("brute_force", "lsh", "hkm"))
                           )), "list", "get_usecases() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_IMAGE_REGRESSION_TESTU",
                           data_type = "images",
                           training_type = "regression",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_IMG"),
                           folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU"),
                           target_column = "x1",
                           filename_column = "PATH",
                           lite_models = list("XGB")), "list", "get_usecases() doesn't retrieve a list for USECASE_IMAGE_REGRESSION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_OBJECT_DETECTOR_TESTU",
                           data_type = "images",
                           training_type = "object-detection",
                           dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "DATASET_TESTU_IMG"),
                           folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "FOLDER_TESTU"),
                           target_column = "TARGET",
                           filename_column = "PATH",
                           xmin = "x1",
                           xmax = "x2",
                           ymin = "y2",
                           ymax = "y1"), "list", "get_usecases() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
})

Sys.sleep(180)

test_that("get_usecases", {
  expect_is(get_usecases(get_project_id_from_name("PROJECT_TESTU")), "list", "get_usecases() doesn't retrieve a list")
  expect(length(get_usecases(get_project_id_from_name("PROJECT_TESTU"))) >= 1, "get_usecases() doesn't have at least one element")
  expect(length(get_usecases(get_project_id_from_name("PROJECT_TESTU"))) >= nb_usecases+1, "The number of usecases has not increased after dataset creation")
})

test_that("get_usecase_id_from_name", {
  expect_is(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                     "USECASE_REGRESSION_TESTU"), "character", "get_usecase_id_from_name() doesn't retrieve a character")
})

test_that("get_usecase_version_id", {
  expect_is(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                            "USECASE_REGRESSION_TESTU"), 1), "character", "get_usecase_version_id() doesn't retrieve a character")
})

test_that("get_usecase_info", {
  expect_is(get_usecase_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                      "USECASE_REGRESSION_TESTU")), "list", "get_usecase_info() doesn't retrieve a list")
})

test_that("get_usecase_version_features", {
  expect_is(get_usecase_version_features(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                         "USECASE_REGRESSION_TESTU"))), "list", "get_usecase_version_features() doesn't retrieve a list")
})

# test_that("get_features_infos", {
#   expect_is(get_features_infos(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
#                                                        "USECASE_REGRESSION_TESTU"), get_usecase_version_features(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"), "USECASE_REGRESSION_TESTU")))$items[[1]]$name), "list", "get_features_infos() doesn't retrieve a list")
# })

test_that("get_usecase_version_models", {
  expect_is(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                       "USECASE_REGRESSION_TESTU"))), "list", "get_usecase_version_models() doesn't retrieve a list")
})

test_that("get_model_infos", {
  expect_is(get_model_infos(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                       "USECASE_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_model_infos() doesn't retrieve a list")
})

test_that("get_model_hyperparameters", {
  expect_is(get_model_hyperparameters(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_model_hyperparameters() doesn't retrieve a list")
})

test_that("get_model_feature_importance", {
  expect_is(get_model_feature_importance(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "USECASE_REGRESSION_TESTU")))[[1]]$`_id`, "raw"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on raw level")
  expect_is(get_model_feature_importance(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                    "USECASE_REGRESSION_TESTU")))[[1]]$`_id`, "engineered"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on engineered level")
})

test_that("create_prediction", {
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_REGRESSION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_REGRESSION")), "list", "create_prediction() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_CLASSIFICATION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_CLASSIFICATION")), "list", "create_prediction() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_MULTICLASSIFICATION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_MULTICLASSIFICATION")), "list", "create_prediction() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_TIMESERIES_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_TS")), "list", "create_prediction() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_TEXT_SIM_TESTU")),
                              model_id = get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                    "USECASE_TEXT_SIM_TESTU")),
                              queries_dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                            "DATASET_TEXT_SIM_QUERY_TESTU"),
                              queries_dataset_content_column = "query",
                              top_k = 10), "list", "create_prediction() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_OBJECT_DETECTOR_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_IMG"),
                              folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                          "FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(create_prediction(usecase_version_id = get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                   "USECASE_IMAGE_REGRESSION_TESTU")),
                              dataset_id = get_dataset_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "DATASET_TESTU_IMG"),
                              folder_dataset_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                          "FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_IMAGE_REGRESSION_TESTU")
})

Sys.sleep(30)

test_that("get_usecase_version_predictions", {
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_REGRESSION_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_CLASSIFICATION_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_MULTICLASSIFICATION_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_TIMESERIES_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_TEXT_SIM_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_OBJECT_DETECTOR_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_IMAGE_REGRESSION_TESTU"))), "list", "get_usecase_version_predictions() doesn't retrieve a list for USECASE_IMAGE_REGRESSION_TESTU")
})

test_that("get_prediction_infos", {
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_CLASSIFICATION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_TIMESERIES_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_TEXT_SIM_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_OBJECT_DETECTOR_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_prediction_infos(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                                 "USECASE_IMAGE_REGRESSION_TESTU")))[[1]]$`_id`), "list", "get_prediction_infos() doesn't retrieve a list for USECASE_IMAGE_REGRESSION_TESTU")
})

test_that("get_prediction", {
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_REGRESSION_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_REGRESSION_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_CLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_TIMESERIES_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_TIMESERIES_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_TEXT_SIM_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_TEXT_SIM_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_OBJECT_DETECTOR_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_IMAGE_REGRESSION_TESTU")))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_IMAGE_REGRESSION_TESTU")
})

test_that("delete_prediction", {
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_REGRESSION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_REGRESSION_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_CLASSIFICATION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_CLASSIFICATION_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for  USECASE_MULTICLASSIFICATION_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_TIMESERIES_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_TIMESERIES_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_TEXT_SIM_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_TEXT_SIM_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_OBJECT_DETECTOR_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_OBJECT_DETECTOR_TESTU")
  expect(delete_prediction(get_usecase_version_predictions(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                           "USECASE_IMAGE_REGRESSION_TESTU")))[[1]]$`_id`) == 204, "delete_prediction() doesn't retrieve a 204 status code for USECASE_IMAGE_REGRESSION_TESTU")
})

test_that("update_usecase_version_description", {
  expect(update_usecase_version_description(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                            "USECASE_REGRESSION_TESTU")), "DESCRIPTION") == 200, "update_usecase_description() doesn't retrieve a 200 status code for USECASE_REGRESSION_TESTU")
})

test_that("get_model_cv", {
  expect_is(get_model_cv(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                    "USECASE_REGRESSION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for USECASE_REGRESSION_TESTU")
  expect_is(get_model_cv(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                    "USECASE_CLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_model_cv(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                    "USECASE_MULTICLASSIFICATION_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_model_cv(get_usecase_version_models(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                                    "USECASE_TIMESERIES_TESTU")))[[1]]$`_id`), "data.frame", "get_model_cv() doesn't retrieve a data.frame for USECASE_TIMESERIES_TESTU")
})

test_that("get_best_model_id", {
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_REGRESSION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_REGRESSION_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_CLASSIFICATION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_MULTICLASSIFICATION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_TIMESERIES_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_TIMESERIES_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_TEXT_SIM_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_TEXT_SIM_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_OBJECT_DETECTOR_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_best_model_id(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_IMAGE_REGRESSION_TESTU"))), "character", "get_best_model_id() doesn't retrieve a character for USECASE_IMAGE_REGRESSION_TESTU")
})

test_that("pause_usecase_version", {
  expect(pause_usecase_version(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                               "USECASE_REGRESSION_TESTU"))) == 200, "pause_usecase_version() doesn't retrieve a 200 status code")
})

test_that("resume_usecase_version", {
  expect(resume_usecase_version(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                                "USECASE_REGRESSION_TESTU"))) == 200, "resume_usecase_version() doesn't retrieve a 200 status code")
})

test_that("stop_usecase_version", {
  expect(stop_usecase_version(get_usecase_version_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "USECASE_REGRESSION_TESTU"))) == 200, "stop_usecase_version() doesn't a 200 status code")
})
