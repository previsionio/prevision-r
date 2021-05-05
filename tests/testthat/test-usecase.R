context("Usecases")

source("helper-credentials.R")
source("helper-dataset.R")

# INIT GLOBAL VARIABLE
nb_usecases = length(get_usecases(get_project_id_from_name("PROJECT_TESTU")))

test_that("create_usecase", {
  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_REGRESSION_TESTU",
                           data_type = "tabular",
                           training_type = "regression",
                           dataset_id = create_dataset_from_dataframe(project_id = get_project_id_from_name("PROJECT_TESTU"),
                                                                      dataset_name = "DATASET_REGRESSION_TESTU",
                                                                      dataframe = tabular_dataset("regression",
                                                                                                  n_row = 1000,
                                                                                                  n_features = 10),
                                                                      zip = FALSE)$`_id`,
                           target_column = "TARGET",
                           normal_models = c("LR", "RF"),
                           lite_models = c("LR"),
                           simple_models = c("LR", "DT"),
                           with_blend = F), "list", "get_usecases() doesn't retrieve a list for USECASE_REGRESSION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_CLASSIFICATION_TESTU",
                           data_type = "tabular",
                           training_type = "classification",
                           dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                      "DATASET_CLASSIFCICATION_TESTU",
                                                                      tabular_dataset("classification"))$`_id`,
                           target_column = "TARGET",
                           normal_models = c("LGB", "XGB")), "list", "get_usecases() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_MULTICLASSIFICATION_TESTU",
                           data_type = "tabular",
                           training_type = "multiclassification",
                           dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                      "DATASET_MULTICLASSIFICATION_TESTU",
                                                                      tabular_dataset("multiclassification"))$`_id`,
                           target_column = "TARGET"), "list", "get_usecases() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_TIMESERIES_TESTU",
                           data_type = "timeseries",
                           training_type = "regression",
                           dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                      "DATASET_TIMESERIES_TESTU",
                                                                      timeseries_dataset())$`_id`,
                           target_column = "TARGET",
                           time_column = "TS",
                           start_dw = -2,
                           end_dw = -1,
                           start_fw = 1,
                           end_fw = 2), "list", "get_usecases() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_TEXT_SIM_TESTU",
                           data_type = "tabular",
                           training_type = "text-similarity",
                           dataset_id = create_dataset_from_dataframe("DATASET_TEXT_SIM_ITEM_TESTU", fread(paste0(wd, "/data/txt_sim_items.csv")), T),
                           id_column = "item_id",
                           content_olumn = "item_desc",
                           queries_dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                              "DATASET_TEXT_SIM_QUERY_TESTU",
                                                                              fread(paste0(wd, "/data/txt_sim_queries.csv")), T),
                           queries_dataset_content_column = "query",
                           queries_dataset_matching_id_description_column = "true_item_id",
                           models_parameters = list(
                             list("model_embedding" = "tf_idf", "preprocessing" = list("word_stemming" = "yes", "ignore_stop_word" = "auto", "ignore_punctuation" = "no"), "models" = c("brute_force", "cluster_pruning")),
                             list("model_embedding" = "transformer", "preprocessing" = NA, "models" = c("brute_force", "lsh", "hkm")),
                             list("model_embedding" = "transformer_fine_tuned", "preprocessing" = NA, models = c("brute_force", "lsh", "hkm"))
                           )), "list", "get_usecases() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_OBJECT_DETECTOR_TESTU",
                           data_type = "images",
                           training_type = "object-detection",
                           dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                      "DATASET_IMG_TESTU",
                                                                      fread(paste0(wd, "/data/img.csv")), T),
                           dataset_folder_id = get_folder_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                       "FOLDER_TESTU"),
                           target_column = "TARGET",
                           filename_column = "PATH",
                           x_min = "x1",
                           x_max = "x2",
                           y_min = "y2",
                           y_max = "y1"), "list", "get_usecases() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")

  expect_is(create_usecase(project_id = get_project_id_from_name("PROJECT_TESTU"),
                           name = "USECASE_IMAGE_CLASSIFICATION_TESTU",
                           data_type = "images",
                           training_type = "classification",
                           dataset_id = create_dataset_from_dataframe(get_project_id_from_name("PROJECT_TESTU"),
                                                                      "DATASET_IMG_TESTU",
                                                                      fread(paste0(wd, "/data/img.csv")), T),
                           dataset_folder_id = get_folder_id_from_name("FOLDER_TESTU"),
                           target_column = "TARGET",
                           filename_column = "PATH",
                           normal_models = c("XGB")), "list", "get_usecases() doesn't retrieve a list for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

Sys.sleep(120)

test_that("get_usecases", {
  expect_is(get_usecases(), "list", "get_usecases() doesn't retrieve a list")
  expect(length(get_usecases()) >= 1, "get_usecases() doesn't have at least one element")
  expect(length(get_usecases()) >= nb_usecases+1, "The number of usecases has not increased after dataset creation")
})

test_that("get_usecase_id_from_name", {
  expect_is(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                     "USECASE_REGRESSION_TESTU"), "character", "get_usecase_id_from_name() doesn't retrieve a character")
})

test_that("get_usecase_version_id", {
  expect_is(get_usecase_version_id(get_project_id_from_name("PROJECT_TESTU"),
                                   get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                            "USECASE_REGRESSION_TESTU"), 1), "character", "get_usecase_version_id() doesn't retrieve a character")
})

test_that("get_usecase_info", {
  expect_is(get_usecase_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                      "USECASE_REGRESSION_TESTU")), "list", "get_usecase_info() doesn't retrieve a list")
})

test_that("get_usecase_features", {
  expect_is(get_usecase_features(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                          "USECASE_REGRESSION_TESTU")), "list", "get_usecase_features() doesn't retrieve a list")
})

test_that("get_features_info", {
  expect_is(get_features_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_REGRESSION_TESTU"), get_usecase_features(get_usecase_id_from_name("USECASE_REGRESSION_TESTU")[[1]])$featureList[[1]]$name), "list", "get_features_info() doesn't retrieve a list")
})

test_that("get_usecase_models", {
  expect_is(get_usecase_models(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                        "USECASE_REGRESSION_TESTU")), "list", "get_usecase_models() doesn't retrieve a list")
})

test_that("get_model_info", {
  expect_is(get_model_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_REGRESSION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "get_model_info() doesn't retrieve a list")
})

test_that("get_model_hyperparameters", {
  expect_is(get_model_hyperparameters(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                               "USECASE_REGRESSION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "get_model_hyperparameters() doesn't retrieve a list")
})

test_that("get_model_feature_importance", {
  expect_is(get_model_feature_importance(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                  "USECASE_REGRESSION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`, "raw"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on raw level")
  expect_is(get_model_feature_importance(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                  "USECASE_REGRESSION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`, "engineering"), "data.frame", "get_model_feature_importance() doesn't retrieve a data.frame on engineering level")
})

test_that("create_prediction", {
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_REGRESSION_TESTU"), dataset_id = get_dataset_id_from_name("DATASET_REGRESSION_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_CLASSIFICATION_TESTU"), dataset_id = get_dataset_id_from_name("DATASET_CLASSIFICATION_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_MULTICLASSIFICATION_TESTU"), dataset_id = get_dataset_id_from_name("DATASET_MULTICLASSIFICATION_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_TIMESERIES_TESTU"), dataset_id = get_dataset_id_from_name("USECASE_TIMESERIES_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_TEXT_SIM_TESTU"), model_id = get_best_model_id(get_usecase_id_from_name("USECASE_TEXT_SIM_TESTU")), queries_dataset_id = get_dataset_id_from_name("DATASET_TEXT_SIM_QUERY_TESTU"), queries_dataset_content_column = "query", top_k = 10), "list", "create_prediction() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_OBJECT_DETECTOR_TESTU"), dataset_id = get_dataset_id_from_name("DATASET_IMG_TESTU"), dataset_folder_id = get_folder_id_from_name("FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(create_prediction(usecase_id = get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                    "USECASE_IMAGE_CLASSIFICATION_TESTU"), dataset_id = get_dataset_id_from_name("DATASET_IMG_TESTU"), dataset_folder_id = get_folder_id_from_name("FOLDER_TESTU")), "list", "create_prediction() doesn't retrieve a list for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

Sys.sleep(30)

test_that("get_usecase_predictions", {
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_REGRESSION_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_CLASSIFICATION_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_MULTICLASSIFICATION_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_TIMESERIES_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_TEXT_SIM_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_OBJECT_DETECTOR_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_usecase_predictions(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                             "USECASE_IMAGE_CLASSIFICATION_TESTU")), "list", "get_usecase_predictions() doesn't retrieve a list for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

test_that("get_prediction_info", {
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_REGRESSION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_MULTICLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_MULTICLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_TIMESERIES_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TIMESERIES_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_TEXT_SIM_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TEXT_SIM_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_OBJECT_DETECTOR_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_OBJECT_DETECTOR_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(get_prediction_info(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                         "USECASE_IMAGE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_IMAGE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "get_prediction_info() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
})

test_that("get_prediction", {
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_REGRESSION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_REGRESSION_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_MULTICLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_MULTICLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_TIMESERIES_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TIMESERIES_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_TIMESERIES_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_TEXT_SIM_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TEXT_SIM_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_TEXT_SIM_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_OBJECT_DETECTOR_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_OBJECT_DETECTOR_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_IMAGE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_IMAGE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "get_prediction() doesn't retrieve a data.frame for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

test_that("delete_prediction", {
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_REGRESSION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_CLASSIFICATION_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_MULTICLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_MULTICLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_TIMESERIES_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TIMESERIES_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_TIMESERIES_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_TEXT_SIM_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_TEXT_SIM_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_TEXT_SIM_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_OBJECT_DETECTOR_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_OBJECT_DETECTOR_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(delete_prediction(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_IMAGE_CLASSIFICATION_TESTU"), get_usecase_predictions(get_usecase_id_from_name("USECASE_IMAGE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "list", "delete_prediction() doesn't retrieve a list for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

test_that("update_usecase_description", {
  expect_is(update_usecase_description(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                                "USECASE_REGRESSION_TESTU"), "DESCRIPTION"), "list", "update_usecase_description() doesn't retrieve a list for USECASE_REGRESSION_TESTU")
})

test_that("get_usecase_cv", {
  expect_is(get_usecase_cv(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_REGRESSION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_REGRESSION_TESTU"))[[1]]$`_id`), "data.frame", "get_usecase_cv() doesn't retrieve a data.frame for USECASE_REGRESSION_TESTU")
  expect_is(get_usecase_cv(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_CLASSIFICATION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_CLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "get_usecase_cv() doesn't retrieve a data.frame for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_usecase_cv(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_MULTICLASSIFICATION_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_MULTICLASSIFICATION_TESTU"))[[1]]$`_id`), "data.frame", "get_usecase_cv() doesn't retrieve a data.frame for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_usecase_cv(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                    "USECASE_TIMESERIES_TESTU"), get_usecase_models(get_usecase_id_from_name("USECASE_TIMESERIES_TESTU"))[[1]]$`_id`), "data.frame", "get_usecase_cv() doesn't retrieve a data.frame for USECASE_TIMESERIES_TESTU")
})

test_that("get_best_model_id", {
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_REGRESSION_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_REGRESSION_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_CLASSIFICATION_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_CLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_MULTICLASSIFICATION_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_MULTICLASSIFICATION_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_TIMESERIES_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_TIMESERIES_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_TEXT_SIM_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_TEXT_SIM_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_OBJECT_DETECTOR_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_OBJECT_DETECTOR_TESTU")
  expect_is(get_best_model_id(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                       "USECASE_IMAGE_CLASSIFICATION_TESTU"), 1, TRUE), "character", "get_best_model_id() doesn't retrieve a character for USECASE_IMAGE_CLASSIFICATION_TESTU")
})

test_that("pause_usecase", {
  expect(pause_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                "USECASE_REGRESSION_TESTU")) == 200, "pause_usecase() doesn't retrieve a 200 status code")
})

test_that("resume_usecase", {
  expect(resume_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                                 "USECASE_REGRESSION_TESTU")) == 200, "resume_usecase() doesn't retrieve a 200 status code")
})

test_that("stop_usecase", {
  expect(stop_usecase(get_usecase_id_from_name(get_project_id_from_name("PROJECT_TESTU"),
                                               "USECASE_REGRESSION_TESTU")) == 200, "stop_usecase() doesn't a 200 status code")
})
