get_usecases <- function(project_id) {
  #' Get information of all usecases available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return parsed content of all usecases for the suppled project_id.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  usecases = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/usecases?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      usecases = c(usecases, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("Can't retrieve usecases list - ", resp$status_code, ":", resp_parsed)
    }
  }
  usecases
}

get_usecase_id_from_name <- function(project_id, usecase_name) {
  #' Get a usecase_id from a usecase_name If duplicated name, the first usecase_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param usecase_name name of the usecase we are searching its id from. Can be obtained with get_usecases().
  #'
  #' @return usecase_id of the usecase_name if found.
  #'
  #' @import httr
  #'
  #' @export

  usecase_list = get_usecases(project_id)
  for (usecase in usecase_list) {
    if(usecase$name == usecase_name) {
      return(usecase$`_id`)
    }
  }
  stop("There is no usecase_id matching the usecase_name ", usecase_name)
}

get_usecase_info <- function(usecase_id, version_number = NULL) {
  #' Get a usecase from its usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. If not supplied, retrieve all usecase information
  #'
  #' @return parsed content of the usecase.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    # If version number isn't supplied, retrieve the complete information
    if(is.null(version_number)) {
      resp_parsed$items
    }

    # Otherwise, retrieve only the selected version information
    else {
      for(item in resp_parsed$items) {
        if(item$version == version_number) {
          item
        }
      }
    }
  }
  else {
    stop("Can't retrieve information from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_version_id <- function(usecase_id, version_number = 1) {
  #' Get a usecase version id from a usecase_id and its version number.
  #'
  #' @param usecase_id id of the usecase, can be obtained with get_usecases().
  #' @param version_number number of the version of the usecase. 1 by default
  #'
  #' @return usecase version id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecases/', usecase_id, '/versions/'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    for(item in resp_parsed$items) {
      if(item$version == version_number) {
        return(item$`_id`)
      }
    }
  }
  else {
    stop("Can't retrieve usecase version id from usecase ", usecase_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_version_features <- function(usecase_version_id) {
  #' Get features information related to a usecase_version_id.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @return parsed content of the usecase_version features information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/features'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve features information from usecase_version ", usecase_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_features_infos <- function(usecase_version_id, feature_name) {
  #' Get information of a given feature related to a usecase_version_id.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #' @param feature_name name of the feature to retrive information.
  #'
  #' @return parsed content of the specific feature.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/features/', feature_name), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve information from feature ", feature_name, " from usecase_version ", usecase_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_usecase_version_models <- function(usecase_version_id) {
  #' Get a model list related to a usecase_version_id.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @return parsed content of models attached to usecase_version_id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/models'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed$items
  }
  else {
    stop("Can't retrieve models from usecase_version ", usecase_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_infos <- function(model_id) {
  #' Get model information corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with get_usecase_models().
  #'
  #' @return parsed content of the model.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/models/', model_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve model ", model_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_hyperparameters <- function(model_id) {
  #' Get hyperparameters corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with usecaseModels(usecase_id).
  #'
  #' @return parsed content of the model's hyperparameters.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/models/', model_id, '/hyperparameters/download'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve hyperparameters of model ", model_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_feature_importance <- function(model_id, mode = "raw") {
  #' Get feature importance corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with get_usecase_models().
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
    resp <- pio_download(paste0('/models/', model_id, '/features-importances/download'), temp)
  }

  if(mode == "engineered") {
    resp <- pio_download(paste0('/models/', model_id, '/features-engineering-importances/download'), temp)
  }

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    data
  }
  else {
    stop("Can't retrieve feature importance of model ", model_id, " - ", resp$status_code)
  }
}

get_usecase_version_predictions <- function(usecase_version_id, generating_type = "user") {
  #' Get a list of prediction from a usecase_version_id.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #' @param generating_type can be "user" (= user predictions) or "auto" (= hold out predictions).
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
    if(generating_type == "user") {
      resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/validation-predictions?page=', page), GET)
    }
    else {
      resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/holdout-predictions?page=', page), GET)
    }
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store items and continue
      predictions = c(predictions, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("Can't retrieve predictions from usecase_version ", usecase_version_id, " - ", resp$status_code, ":", resp_parsed)
    }
  }
  predictions
}

create_prediction <- function(usecase_version_id, dataset_id = NULL, folder_dataset_id = NULL, confidence = F, best_single = F, model_id = NULL, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL) {
  #' Create a prediction on a specified usecase_version
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #' @param dataset_id id of the dataset to start the prediction on, can be obtained with get_datasets().
  #' @param folder_dataset_id id of the folder dataset to start prediction on, can be obtained with get_folders(). Only usefull for images use cases.
  #' @param confidence boolean. If enable, confidence interval will be added to predictions.
  #' @param best_single boolean. If enable, best single model (non blend) will be used for making predictions other wise, best model will be used unless if model_id is fed.
  #' @param model_id id of the model to start the prediction on. If provided, it will overwrite the "best single" params.
  #' @param queries_dataset_id id of the dataset containing queries (text-similarity).
  #' @param queries_dataset_content_column name of the content column in the queries dataset (text-similarity).
  #' @param queries_dataset_id_column name of the id column in the queries dataset (text-similarity).
  #' @param queries_dataset_matching_id_description_column name of the column matching the id (text-similarity).
  #' @param top_k number of class to retrieve (text-similarity).
  #'
  #' @return parsed prediction list.
  #'
  #' @import httr
  #'
  #' @export

  params = list(dataset_id = dataset_id,
                folder_dataset_id = folder_dataset_id,
                confidence = confidence,
                best_single = best_single,
                model_id = model_id,
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

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/validation-predictions'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't start prediction for usecase_version ", usecase_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction_infos <- function(prediction_id) {
  #' Get a information about a prediction_id.
  #'
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with get_usecase_version_predictions().
  #'
  #' @return list of prediction information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/validation-predictions/', prediction_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  } else {
    stop("Can't retrieve prediction infos of prediction ", prediction_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction <- function(prediction_id, time_out = 3600, wait_time = 10) {
  #' Get a specific prediction from a prediction_id. Wait up until time_out is reached and wait wait_time between each retry.
  #'
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with get_usecase_version_predictions().
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
    resp <- pio_download(paste0('/validation-predictions/', prediction_id, '/download'), temp)

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
  stop("Can't retrieve prediction prediction ", prediction_id, " - ", resp$status_code)
}

delete_prediction <- function(prediction_id) {
  #' Delete a prediction.
  #'
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with get_usecase_version_predictions()
  #'
  #' @import httr
  #'
  #' @return list of predictions of usecase_id.
  #'
  #' @export

  resp <- pio_request(paste0('/validation-predictions/', prediction_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 204) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    stop("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
  return(resp$status_code)
}

create_usecase <- function(project_id, name, data_type, training_type, dataset_id, target_column = NULL, holdout_dataset_id = NULL, id_column = NULL, drop_list = NULL, profile = NULL, usecase_description = NULL, metric = NULL, fold_column = NULL, normal_models = NULL, lite_models = NULL, simple_models = NULL, with_blend = NULL, weight_column = NULL, features_engineering_selected_list = NULL, features_selection_count = NULL, features_selection_time = NULL, folder_dataset_id = NULL, filename_column = NULL, ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL, time_column = NULL, start_dw = NULL, end_dw = NULL, start_fw = NULL, end_fw = NULL, group_list = NULL, apriori_list = NULL, content_column = NULL, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL, lang = NULL, models_params = NULL) {
  #' Create a new usecase on the platform.
  #'
  #' @param project_id id of the project in which we create the usecase.
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
  #' @param normal_models list of (normal) models to select with full FE & hyperparameters search (among "LR", "RF", "ET", "XGB", "LGB", "NN", "CB").
  #' @param lite_models list of (lite) models to select with lite FE & default hyperparameters (among "LR", "RF", "ET", "XGB", "LGB", "NN", "CB", "NBC").
  #' @param simple_models list of simple models to select (among "LR", "DT").
  #' @param with_blend boolean, do we allow to include blend in the modelisation.
  #' @param weight_column name of the weight columns.
  #' @param features_engineering_selected_list list of feature engineering to select (among "Counter", "Date", "freq", "text_tfidf", "text_word2vec", "text_embedding", "tenc", "poly", "pca", "kmean").
  #' @param features_selection_count number of features to keep after the feature selection process.
  #' @param features_selection_time time budget in minutes of the feature selection process.
  #' @param folder_dataset_id id of the dataset folder (images).
  #' @param filename_column name of the file name path (images).
  #' @param ymin name of the column matching the lower y value of the image (object detection).
  #' @param ymax name of the column matching the higher y value of the image (object detection).
  #' @param xmin name of the column matching the lower x value of the image (object detection).
  #' @param xmax name of the column matching the higher x value of the image (object detection).
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
  #' @param models_params parameters of the model (text-similarity).
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
  if(!dataset_id %in% unlist(get_datasets(project_id))) {
    stop("dataset_id doesn't exist")
  }

  # CHECKING CONDITIONS FOR normal_models, lite_models and simple_models
  # if (length(normal_models) + length(lite_models) + length(simple_models) < 1) {
  #   stop("must give at least one model")
  # }
  if(!all(normal_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "CB"))) {
    stop("normal_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"CB\" or \"NN\"")
  }
  if(!all(lite_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "NBC", "CB"))) {
    stop("lite_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"NN\", \"CB\" or \"NBC\"")
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
                  folder_dataset_id = folder_dataset_id,
                  filename_column = filename_column,
                  ymin = ymin,
                  ymax = ymax,
                  xmin = xmin,
                  xmax = xmax,
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
                  models_params = models_params)

  ucParams <- ucParams[!sapply(ucParams, is.null)]

  resp <- pio_request(paste0('/projects/', project_id, '/usecases/', data_type, '/', training_type), POST, ucParams)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Usecase started - ", resp$status_code, ":", resp_parsed[[1]])
  } else {
    stop("Usecase starting failed - ", resp$status_code, ":", resp_parsed[[1]])
  }
  get_usecase_info(resp_parsed$usecase_id)
}

update_usecase_version_description <- function(usecase_version_id, description = "") {
  #' Update the description of a given usecase_version_id.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #' @param description Description of the usecase.
  #'
  #' @import httr
  #'
  #' @export

  params = list(description = description)

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id), PUT, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Description of the usecase_version ", usecase_version_id, " updated - ", resp$status_code, ":", resp_parsed$message)
  } else {
    stop("Update of the description of the usecase_version ", usecase_version_id, " failed - ", resp$status_code, ":", resp_parsed$message)
  }
  return(resp$status_code)
}

delete_usecase_version <- function(usecase_version_id) {
  #' Delete a usecase_version on the platform.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

pause_usecase_version <- function(usecase_version_id) {
  #' Pause a running usecase_version on the platform.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/pause'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Pause OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Pause KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

resume_usecase_version <- function(usecase_version_id) {
  #' Resume a paused usecase_version on the platform.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/resume'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Resume OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Resume KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

stop_usecase_version <- function(usecase_version_id) {
  #' Stop a running or paused usecase_version on the platform.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/usecase-versions/', usecase_version_id, '/stop'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Stop OK - ", resp$status_code, ":", resp_parsed$message)
  } else {
    message("Stop KO - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

get_model_cv <- function(model_id) {
  #' Get the cross validation file from a specific model.
  #'
  #' @param model_id id of the model to get the CV, can be obtained with get_usecase_version_models().
  #'
  #' @return a dataframe containing cross validation data.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  temp <- tempfile()
  resp <- pio_download(paste0('/models/', model_id, '/cross-validation/download'), temp)

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    file.remove(temp)
    data
  } else {
    stop("Can't retrieve CV file for model ", model_id, " - ", resp$status_code)
  }
}

get_best_model_id <- function(usecase_version_id, include_blend = TRUE) {
  #' Get the model_id that provide the best predictive performance given usecase_version_id. If include_blend is false, it will return the model_id from the best "non blended" model.
  #'
  #' @param usecase_version_id id of the usecase_version, can be obtained with get_usecase_version_id().
  #' @param include_blend boolean, indicating if you want to retrieve the best model among blended models too.
  #'
  #' @return model_id.
  #'
  #' @import httr
  #'
  #' @export

  # GET MODELS FROM A USE CASE
  models = get_usecase_version_models(usecase_version_id)

  # LOOP OVER THEM AND RETRIEVE THE ONE MATCHING SELECTED CRITERIA
  for(model in models) {
    if(include_blend && !is.null(model$tags$best)) {
      return(model$`_id`)
    }
    if(!include_blend && !is.null(model$tags$recommended)) {
      return(model$`_id`)
    }
  }

  stop("No model found for usecase_version ", usecase_version_id)
}
