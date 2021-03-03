get_usecases <- function() {
  #' Retrieves all usecases.
  #'
  #' @return a usecase list.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  useCases = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/usecases?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      useCases = c(useCases, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve usecases list - ", resp$status_code, ":", resp_parsed)
    }
  }
  useCases
}

get_usecase_id_from_name <- function(usecase_name) {
  #' Get a usecase_id from a usecase_name If duplicated name, the first usecase_id that match it is retrieved.
  #'
  #' @param usecase_name name of the usecase we are searching its id from. Can be obtained with get_usecases().
  #'
  #' @return usecase_id of the usecase_name if found.
  #'
  #' @import httr
  #'
  #' @export

  usecaseList = get_usecases()
  for (uc in usecaseList) {
    if(uc$name == usecase_name) {
      return(uc$usecase_id)
    }
  }
  stop("There is no usecase_id matching the usecase_name ", usecase_name)
}

get_usecase_info <- function(usecase_id, version_number = 1) {
  #' Get a usecase from its usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve information from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_tasks <- function(usecase_id, version_number = 1) {
  #' Get all tasks related to a usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase tasks.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/tasks'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve tasks from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_schema <- function(usecase_id, version_number = 1) {
  #' Get schema related to a usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase schema.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/schema'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve schema from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_features <- function(usecase_id, version_number = 1) {
  #' Get features information related to a usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase features information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/features'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve features information from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_features_infos <- function(usecase_id, feature_name, version_number = 1) {
  #' Get information of a given feature related to a usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param feature_name name of the feature to retrive information.
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the specific feature.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/features/', feature_name), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve information from feature ", feature_name, " from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_models <- function(usecase_id, version_number = 1) {
  #' Get a model list related to a usecase_id and its version number
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of models attached to usecase_id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed[["items"]]
  }
  else {
    stop("Can't retrieve models from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_infos <- function(usecase_id, model_id, version_number = 1) {
  #' Get model information corresponding to usecase_id and model_id and its version number
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param model_id id of the model, can be obtained with usecaseModels(usecase_id).
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the model.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models/', model_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve model ", model_id, " from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_hyperparameters <- function(usecase_id, model_id, version_number = 1) {
  #' Get hyperparameters corresponding to usecase_id and model_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param model_id id of the model, can be obtained with usecaseModels(usecase_id).
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the model's hyperparameters.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models/', model_id, '/download/hyperparameters'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve hyperparameters of model ", model_id, " from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_feature_importance <- function(usecase_id, model_id, version_number = 1, mode = "raw") {
  #' Get feature importance corresponding to usecase_id and model_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param model_id id of the model, can be obtained with usecaseModels(usecase_id).
  #' @param version_number number of the version of the usecase. 1 by default.
  #' @param mode character indicating the type of feature importance among "raw" (default) or "engineered".
  #'
  #' @return dataset of the model's feature importance.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  if(!mode %in% c("raw", "engineered")) {
    stop(mode, "should be equal to \"raw\" or \"engineered\"")
  }

  temp <- tempfile()

  if(mode == "raw") {
    resp <- pio_download(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models/', model_id, '/download/features-importance'), temp)
  }

  if(mode == "engineered") {
    resp <- pio_download(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models/', model_id, '/download/features-engineering-importance'), temp)
  }

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    data
  }
  else {
    stop("Can't retrieve feature importance of model ", model_id, " from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code)
  }
}

get_usecase_predictions <- function(usecase_id, generating_type = "user", version_number = 1) {
  #' Get a usecase from its usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param generating_type can be "user" (= user predictions) or "auto" (= hold out predictions).
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return parsed prediction list items.
  #'
  #' @import httr
  #'
  #' @export

  if(!generating_type %in% c("user", "auto")) {
    stop("generating_type should be equal to \"user\" or \"auto\"")
  }

  page = 1
  predictions = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/predictions?page=', page, '?generating_type=', generating_type, '/'), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      predictions = c(predictions, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve predictions from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
    }
  }
  predictions
}

create_prediction <- function(usecase_id, dataset_id = NULL, dataset_folder_id = NULL, confidence = F, best_single = F, model_id = NULL, version_number = 1, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL) {
  #' Create a prediction on a existing usecase
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param dataset_id id of the dataset to start the prediction on, can be obtained with get_usecases().
  #' @param dataset_folder_id id of the folder dataset to start prediction on, can be obtained with get_usecases(). Only usefull for images use cases.
  #' @param confidence boolean. If enable, confidence intervalle will be added to predictions.
  #' @param best_single boolean. If enable, best single model (non blend) will be used for making predictions other wise, best model will be used unless if model_id is fed.
  #' @param model_id id of the model to start the prediction on. If provided, it will overwrite the "best single" params.
  #' @param version_number number of the version of the usecase. 1 by default.
  #' @param queries_dataset_id id of the datasat containing queries (text-similarity).
  #' @param queries_dataset_content_column name of the content column in the queries dataset (text-similarity).
  #' @param queries_dataset_id_column name of the id columln in the queries dataset (text-similarity).
  #' @param queries_dataset_matching_id_description_column name of the column matching the id (text-similarity).
  #' @param top_k number of class to retrieve (text-similarity).
  #'
  #' @return parsed prediction list.
  #'
  #' @import httr
  #'
  #' @export

  params = list(dataset_id = dataset_id,
                dataset_folder_id = dataset_folder_id,
                confidence = confidence,
                best_single = best_single,
                model_id = model_id,
                version_number = version_number,
                queries_dataset_id = queries_dataset_id,
                queries_dataset_content_column = queries_dataset_content_column,
                queries_dataset_id_column = queries_dataset_id_column,
                queries_dataset_matching_id_description_column = queries_dataset_matching_id_description_column,
                top_k = top_k)

  params <- params[!sapply(params, is.null)]

  if(is.null(dataset_id) & is.null(queries_dataset_id)) {
    stop("either dataset_id or queries_dataset_id should be set")
  }

  if(!is.null(model_id)){
    message("model_id is set, the best_single param won't be taken into account")
  }

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/predictions'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 202) {
    resp_parsed
  }
  else {
    stop("Can't start prediction for usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction_infos <- function(usecase_id, prediction_id, version_number = 1) {
  #' Get a information about a prediction from a specific usecase / version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with usecasePredictions().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return list of prediction information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/predictions/', prediction_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  } else {
    stop("Can't retrieve prediction infos of prediction ", prediction_id, " for usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction <- function(usecase_id, prediction_id, version_number = 1, time_out = 3600, wait_time = 10) {
  #' Get a specific prediction from a specific usecase / version number. Wait up until time_out is reached and wait wait_time between each retry.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with usecasePredictions().
  #' @param version_number number of the version of the usecase. 1 by default.
  #' @param time_out maximum number of seconds to wait for the prediction. 3 600 by default.
  #' @param wait_time number of seconds to wait between each retry. 10 by default.
  #'
  #' @return a data.frame with the predictions.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  attempt = 0
  while(attempt < time_out/wait_time) {
    temp <- tempfile()
    resp <- pio_download(paste0('/usecases/', usecase_id, '/versions/', version_number, '/predictions/', prediction_id, '/download'), temp)

    # IF STATUS 200 RETURN PREDICTION
    if(resp$status_code == 200) {
      data <- fread(unzip(temp))
      file.remove(unzip(temp))
      file.remove(temp)
      return(data)
    }

    # IF STATUS 404 SLEEP AND RETRY
    if(resp$status_code == 404) {
      message("Prediction is beeing computed...")
      Sys.sleep(wait_time)
      attempt = attempt + 1
    }
  }
  stop("Can't retrieve prediction prediction ", prediction_id, " for usecase ", usecase_id, " version ", version_number, " - ", resp$status_code)
}

delete_prediction <- function(usecase_id, prediction_id, version_number = 1) {
  #' Delete a prediction of a given model and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with usecasePredictions().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @return list of predictions of usecase_id.
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/predictions/', prediction_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
  get_usecase_predictions(usecase_id)
}

share_usecase <- function(usecase_id, email) {
  #' Share a usecase to a specific user
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param email email adress of the user you want to share the usecase with. Should be a platform user.
  #'
  #' @import httr
  #'
  #' @return list of shared users.
  #'
  #' @export

  params = list(email = email)

  resp <- pio_request(paste0('/usecases/', usecase_id, '/sharing'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Usecase ", usecase_id, " shared to the user ", email)
  } else {
    message("Sharing of the usecase ", usecase_id, " for user ", email, " failed - ", resp$status_code, ":", resp_parsed$message)
  }
  get_shared_usecase_users(usecase_id)
}

unshare_usecase <- function(usecase_id, email = NULL) {
  #' Unshare a use case for the specified email adress. If missing, the usecase will be unshared from all users.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param email email adress of the user you want to unshare the usecase with. Should be an instance user.
  #'
  #' @import httr
  #'
  #' @return list of shared users.
  #'
  #' @export

  if(is.null(email)) {
    resp <- pio_request(paste0('/usecases/', usecase_id, '/sharing'), DELETE)
  }
  else {
    resp <- pio_request(paste0('/usecases/', usecase_id, '/sharing/', email), DELETE)
  }

  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Usecase ", usecase_id, " unshared")
  } else {
    message("Unsharing of the usecase ", usecase_id, " failed - ", resp$status_code, ":", resp_parsed$message)
  }
  get_shared_usecase_users(usecase_id)
}

get_shared_usecase_users <- function(usecase_id) {
  #' Get the list of users that can access the usecase.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/1'), GET)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    resp_parsed[["shareList"]]
  } else {
    stop("Can't get the list of the users that can access the usecase ", usecase_id, " failed - ", resp$status_code, ":", resp_parsed$message)
  }
}

create_usecase <- function(name, data_type, training_type, dataset_id, target_column = NULL, holdout_dataset_id = NULL, id_column = NULL, drop_list = NULL, profile = NULL, usecase_description = NULL, metric = NULL, fold_column = NULL, normal_models = NULL, lite_models = c('XGB'), simple_models = NULL, with_blend = NULL, weight_column = NULL, features_engineering_selected_list = NULL, features_selection_count = NULL, features_selection_time = NULL, dataset_folder_id = NULL, filename_column = NULL, y_min = NULL, y_max = NULL, x_min = NULL, x_max = NULL, time_column = NULL, start_dw = NULL, end_dw = NULL, start_fw = NULL, end_fw = NULL, group_list = NULL, apriori_list = NULL, content_column = NULL, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL, lang = NULL, models_parameters = NULL) {
  #' Create a new usecase on the platform.
  #'
  #' @param name name of the usecase.
  #' @param data_type type of data ("tabular" or "images" or "timeseries").
  #' @param training_type type of the training you want to achieve ("regression", "classification", "multiclassification", "clustering", "object-detection", "text-similarity").
  #' @param dataset_id id of the dataset used for the training phase.
  #' @param target_column name of the TARGET column.
  #' @param holdout_dataset_id id of the holdout dataset.
  #' @param id_column name of the id column.
  #' @param drop_list list of names of features to drop.
  #' @param profile chosen profil among "quick", "normal", "advanced".
  #' @param usecase_description usecase description.
  #' @param metric name of the metric to optimise.
  #' @param fold_column name of the fold column.
  #' @param normal_models list of (normal) models to select with full FE & hyperparameters search.
  #' @param lite_models list of (lite) models to select with lite FE & default hyperparameters.
  #' @param simple_models list of simple models to select.
  #' @param with_blend do we allow to include blend in the modelisation.
  #' @param weight_column name of the weight columns.
  #' @param features_engineering_selected_list list of feature engineering to select among "Counter", "Date", "freq", "text_tfidf", "text_word2vec", "text_embedding", "tenc", "poly", "pca", "kmean".
  #' @param features_selection_count number of features to keep after the feature selection process.
  #' @param features_selection_time time budget in minutes of the feature selection process.
  #' @param dataset_folder_id id of the dataset fold (images).
  #' @param filename_column name of the file name path (images).
  #' @param y_min name of the column matching the lower y value of the image (object detection).
  #' @param y_max name of the column matching the higher y value of the image (object detection).
  #' @param x_min name of the column matching the lower x value of the image (object detection).
  #' @param x_max name of the column matching the higher x value of the image (object detection).
  #' @param time_column name of column containing the timestamp (time series).
  #' @param start_dw value of the start of derivative window (time series), should be a strict negative integer.
  #' @param end_dw value of the end of derivative window (time series), should be a negative integer greater than start_dw.
  #' @param start_fw value of the start of forecast window (time series), should be a strict positive integer.
  #' @param end_fw value of the end of forecast window (time series), should be a strict positive integer greater than start_fw.
  #' @param group_list list of name of feature that describes groups (time series).
  #' @param apriori_list list of name of feature that are a priori (time series).
  #' @param content_column content column name (text-similarity).
  #' @param queries_dataset_id id of the dataset containing queries (text-similarity).
  #' @param queries_dataset_content_column name of the column containing queries in the query dataset (text-similarity).
  #' @param queries_dataset_id_column name of the ID column in the query dataset (text-similarity).
  #' @param queries_dataset_matching_id_description_column name of the column matching id in the description dataset (text-similarity).
  #' @param top_k top k individual to find (text-similarity).
  #' @param lang lang of the text (text-similarity).
  #' @param models_parameters parameters of the model (text-similarity).
  #'
  #' @import httr
  #'
  #' @return usecase information.
  #'
  #' @export

  # CHECKING data_type
  if(!data_type %in% c("tabular", "images", "timeseries")) {
    stop("data_type must be either \"tabular\", \"images\" or \"timeseries\"")
  }

  # CHECKING training_type
  if(!training_type %in% c("regression", "classification", "multiclassification", "object-detection", "text-similarity")) {
    stop("training_type must be either \"regression\", \"classification\", \"multiclassification\" or \"object-detection\" or \"text-similarity\"")
  }

  # CHECKING dataset_id EXISTS
  if(!dataset_id %in% unlist(get_datasets())) {
    stop("dataset_id doesn't exist")
  }

  # CHECKING CONDITIONS FOR normal_models, lite_models and simple_models
  if (length(normal_models) + length(lite_models) + length(simple_models) < 1) {
    stop("must give at least one model")
  }
  if(!all(normal_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN"))) {
    stop("normal_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\" or \"NN\"")
  }
  if(!all(lite_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "NBC"))) {
    stop("lite_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"NN\" or \"NBC\"")
  }
  if(!all(simple_models %in% c("DT", "LR"))) {
    stop("simple models must be either \"DT\" or \"LR\"")
  }
  if(!training_type %in% c("classification", "multiclassification") & "NBC" %in% lite_models) {
    stop("NBC liteModel is only available for classification or multiclassification")
  }

  # GET PARAMS AND REMOVE NULL ONES
  ucParams = list(name = name,
                  dataset_id = dataset_id,
                  target_column = target_column,
                  holdout_dataset_id = holdout_dataset_id,
                  id_column = id_column,
                  drop_list = drop_list,
                  profile = profile,
                  metric = metric,
                  fold_column = fold_column,
                  normal_models = normal_models,
                  lite_models = lite_models,
                  simple_models = simple_models,
                  with_blend = with_blend,
                  weight_column = weight_column,
                  features_engineering_selected_list = features_engineering_selected_list,
                  features_selection_count = features_selection_count,
                  features_selection_time = features_selection_time,
                  dataset_folder_id = dataset_folder_id,
                  filename_column = filename_column,
                  y_min = y_min,
                  y_max = y_max,
                  x_min = x_min,
                  x_max = x_max,
                  time_column = time_column,
                  start_dw = start_dw,
                  end_dw = end_dw,
                  start_fw = start_fw,
                  end_fw = end_fw,
                  group_list = group_list,
                  apriori_list = apriori_list,
                  content_column = content_column,
                  queries_dataset_id = queries_dataset_id,
                  queries_dataset_content_column = queries_dataset_content_column,
                  queries_dataset_id_column = queries_dataset_id_column,
                  queries_dataset_matching_id_description_column = queries_dataset_matching_id_description_column,
                  top_k = top_k,
                  lang = lang,
                  models_parameters = models_parameters)

  ucParams <- ucParams[!sapply(ucParams, is.null)]

  resp <- pio_request(paste0('/usecases/', data_type, '/', training_type), POST, ucParams)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 202) {
    message("Usecase started - ", resp$status_code, ":", resp_parsed$message)
  } else {
    stop("Usecase starting failed - ", resp$status_code, ":", resp_parsed$message)
  }
  get_usecase_info(resp_parsed$`_id`)
}

updateusecase_description <- function(usecase_id, description = "", version_number = 1) {
  #' Update the description of a given usecase and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param description Description of the usecase.
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  params = list(description = description)

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number), PUT, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Description of the usecase ", usecase_id, " version ", version_number, " updated - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Update of the description of the usecase ", usecase_id, " version ", version_number, " failed - ", resp$status_code, ":", resp_parsed$message)
  }
  get_usecase_info(usecase_id)
}

delete_usecase <- function(usecase_id, version_number = 1) {
  #' Delete a version of a usecase on the platform.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

pause_usecase <- function(usecase_id, version_number = 1) {
  #' Pause a version of a usecase on the platform.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/pause'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Pausing OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Pausing KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

resume_usecase <- function(usecase_id, version_number = 1) {
  #' Resume a version of usecase on the platform.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/resume'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Resuming OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Resuming KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

stop_usecase <- function(usecase_id, version_number = 1) {
  #' Stop a version of a usecase on the platform.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/', version_number, '/stop'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Stop OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Stop KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

get_usecase_cv <- function(usecase_id, model_id, version_number = 1) {
  #' Get the cross validation file from a specific model of a given usecase / version.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param model_id id of the model to get the CV, can be obtained with usecaseModels(usecase_id).
  #' @param version_number number of the version of the usecase. 1 by default.
  #'
  #' @return a dataframe containing cross validation data.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  temp <- tempfile()
  resp <- pio_download(paste0('/usecases/', usecase_id, '/versions/', version_number, '/models/', model_id, '/download/cv/'), temp)

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    file.remove(temp)
    data
  } else {
    stop("Can't retrieve CV file for model ", model_id, " of use case ", usecase_id, " version ", version_number, " - ", resp$status_code)
  }
}

get_best_model_id <- function(usecase_id, version_number = 1, include_blend = TRUE) {
  #' Get the model_id that provide the best predictive performance given usecase_id and version_number. If include_blend is false, it will return the model_id from the best "non blended" model.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default.
  #' @param include_blend boolean, indicating if you want to retrieve the best model among blended models too.
  #'
  #' @return model_id.
  #'
  #' @import httr
  #'
  #' @export

  # GET MODELS FROM A USE CASE
  models = get_usecase_models(usecase_id, version_number = 1)

  # LOOP OVER THEM AND RETRIEVE THE ONE MATCHING SELECTED CRITERIA
  for(model in models) {
    if(include_blend && !is.null(model$tags$best)) {
      return(model$`_id`)
    }
    if(!include_blend && !is.null(model$tags$recommended)) {
      return(model$`_id`)
    }
  }

  stop("No model found for usecase ", usecase_id, " version ", version_number)
}
