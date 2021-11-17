get_experiments <- function(project_id) {
  #' Get information of all experiments available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return list - parsed content of all experiments for the supplied project_id.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  experiments = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/experiments?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      experiments = c(experiments, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve experiments list - ", resp$status_code, ":", resp_parsed)
    }
  }
  experiments
}

get_experiment_id_from_name <- function(project_id, experiment_name) {
  #' Get a experiment_id from a experiment_name If duplicated name, the first experiment_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param experiment_name name of the experiment we are searching its id from. Can be obtained with get_experiments().
  #'
  #' @return character - id matching the experiment_name if found.
  #'
  #' @import httr
  #'
  #' @export

  experiment_list = get_experiments(project_id)
  for (experiment in experiment_list) {
    if(experiment$name == experiment_name) {
      return(experiment$`_id`)
    }
  }
  stop("there is no experiment_id matching the experiment_name ", experiment_name)
}

get_experiment_info <- function(experiment_id) {
  #' Get a experiment from its experiment_id.
  #'
  #' @param experiment_id id of the experiment, can be obtained with get_experiments().
  #'
  #' @return list - parsed content of the experiment.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiments/', experiment_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve information from experiment ", experiment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_experiment_version_info <- function(experiment_version_id) {
  #' Get a experiment_version info from its experiment_version_id
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return list - parsed content of the experiment_version.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve information from experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_experiment_version_id <- function(experiment_id, version_number = 1) {
  #' Get a experiment version id from a experiment_id and its version number.
  #'
  #' @param experiment_id id of the experiment, can be obtained with get_experiments().
  #' @param version_number number of the version of the experiment. 1 by default
  #'
  #' @return character - experiment version id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiments/', experiment_id, '/versions/'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    for(item in resp_parsed$items) {
      if(item$version == version_number) {
        return(item$`_id`)
      }
    }
    stop("can't retrieve experiment version id from experiment ", experiment_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
  else {
    stop("can't retrieve experiment version id from experiment ", experiment_id, " version ", version_number, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_experiment_version_features <- function(experiment_version_id) {
  #' Get features information related to a experiment_version_id.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return list - parsed content of the experiment_version features information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/features'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve features information from experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_features_infos <- function(experiment_version_id, feature_name) {
  #' Get information of a given feature related to a experiment_version_id.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #' @param feature_name name of the feature to retrive information.
  #'
  #' @return list - parsed content of the specific feature.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/features/', feature_name), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve information from feature ", feature_name, " from experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_experiment_version_models <- function(experiment_version_id) {
  #' Get a model list related to a experiment_version_id.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return list - parsed content of models attached to experiment_version_id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/models'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed$items
  }
  else {
    stop("can't retrieve models from experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_infos <- function(model_id) {
  #' Get model information corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with get_experiment_models().
  #'
  #' @return list - parsed content of the model.
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
    stop("can't retrieve model ", model_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_hyperparameters <- function(model_id) {
  #' Get hyperparameters corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with experimentModels(experiment_id).
  #'
  #' @return list - parsed content of the model's hyperparameters.
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
    stop("can't retrieve hyperparameters of model ", model_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_model_feature_importance <- function(model_id, mode = "raw") {
  #' Get feature importance corresponding to a model_id.
  #'
  #' @param model_id id of the model, can be obtained with get_experiment_models().
  #' @param mode character indicating the type of feature importance among "raw" (default) or "engineered".
  #'
  #' @return data.frame - dataset of the model's feature importance.
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
    stop("can't retrieve feature importance of mode ", mode, " for model ", model_id, " - ", resp$status_code)
  }
}

get_experiment_version_predictions <- function(experiment_version_id, generating_type = "user") {
  #' Get a list of prediction from a experiment_version_id.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #' @param generating_type can be "user" (= user predictions) or "auto" (= hold out predictions).
  #'
  #' @return list - parsed prediction list items.
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
      resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/validation-predictions?page=', page), GET)
    }
    else {
      resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/holdout-predictions?page=', page), GET)
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
      stop("can't retrieve predictions from experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
    }
  }
  predictions
}

create_prediction <- function(experiment_version_id, dataset_id = NULL, folder_dataset_id = NULL, confidence = FALSE, best_single = FALSE, model_id = NULL, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL) {
  #' Create a prediction on a specified experiment_version
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
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
  #' @return list - parsed prediction information.
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

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/validation-predictions'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("predictions started on experiment_version ", experiment_version_id)
    resp_parsed
  }
  else {
    stop("can't start prediction on experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction_infos <- function(prediction_id) {
  #' Get a information about a prediction_id.
  #'
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with get_experiment_version_predictions().
  #'
  #' @return list - parsed prediction information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/validation-predictions/', prediction_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  } else {
    stop("can't retrieve prediction infos of prediction ", prediction_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_prediction <- function(prediction_id, prediction_type, time_out = 3600, wait_time = 10) {
  #' Get a specific prediction from a prediction_id. Wait up until time_out is reached and wait wait_time between each retry.
  #'
  #' @param prediction_id id of the prediction to be retrieved, can be obtained with get_experiment_version_predictions().
  #' @param prediction_type type of prediction among "validation" (not deployed model) and "deployment" (deployed model).
  #' @param time_out maximum number of seconds to wait for the prediction. 3 600 by default.
  #' @param wait_time number of seconds to wait between each retry. 10 by default.
  #'
  #' @return data.frame - predictions coming from prediction_id.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  if(!prediction_type %in% c("validation", "deployment")) {
    stop("prediction_type should be either \"validation\" or \"deployment\"")
  }

  attempt = 0
  while(attempt < time_out/wait_time) {
    temp <- tempfile()
    resp <- pio_download(paste0('/', prediction_type, '-predictions/', prediction_id, '/download'), temp)

    # IF STATUS 200 RETURN PREDICTION
    if(resp$status_code == 200) {
      data <- fread(unzip(temp))
      file.remove(unzip(temp))
      file.remove(temp)
      return(data)
    }

    # IF STATUS 404 SLEEP AND RETRY
    else if(resp$status_code == 404 | resp$status_code == 400) {
      message("Prediction is being computed...")
      Sys.sleep(wait_time)
      attempt = attempt + 1
    }

    # OTHERWISE BREAK
    else {
      break
    }
  }
  stop("can't retrieve prediction prediction ", prediction_id, " for prediction_type ", prediction_type," - ", resp$status_code)
}

delete_prediction <- function(prediction_id) {
  #' Delete a prediction.
  #'
  #' @param prediction_id id of the prediction to be deleted, can be obtained with get_experiment_version_predictions().
  #'
  #' @return integer - 204 on success.
  #'
  #' @import httr
  #'
  #' @return list of predictions of experiment_id.
  #'
  #' @export

  resp <- pio_request(paste0('/validation-predictions/', prediction_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 204) {
    message("prediction ", prediction_id, " deleted")
  } else {
    stop("failed to delete prediction ", prediction_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  return(resp$status_code)
}

create_experiment <- function(project_id, name, provider, data_type, training_type, check_if_exist = FALSE) {
  #' Create a new experiment.
  #' If check_if_exist is enabled, the function will check if an experiment with the same name already exists. If yes, it will return a message and the information of the existing experiment instead of creating a new one.
  #'
  #' @param project_id id of the project in which we create the experiment.
  #' @param name name of the experiment.
  #' @param provider provider of the experiment ("prevision-auto-ml" or "external")
  #' @param data_type type of data ("tabular", "images" or "timeseries").
  #' @param training_type type of the training you want to achieve ("regression", "classification", "multiclassification", "clustering", "object-detection" or "text-similarity").
  #' @param check_if_exist boolean (FALSE by default). If TRUE, makes extra checks to see if an experiment with the same name is already existing.
  #'
  #' @return list - experiment information.
  #'
  #' @import httr
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

  # GET PARAMS AND REMOVE NULL ONES
  params = list(name = name,
                provider = provider,
                data_type = data_type,
                training_type = training_type)

  params <- params[!sapply(params, is.null)]

  # DOUBLE CHECK ALREADY EXISTING EXPERIMENTS
  if(check_if_exist) {
    experiments = get_experiments(project_id)
    for(experiment in experiments) {
      if(experiment$name == name) {
        message("a experiment named ", name, " already exists - aborting experiment creation")
        return (get_experiment_info(experiment$`_id`))
      }
    }
    message("there is no experiment named ", name, " - continuing")
  }

  resp <- pio_request(paste0('/projects/', project_id, '/experiments/'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("experiment created")
  } else {
    stop("failed to create experiment - ", resp$status_code, ":", resp_parsed[[1]])
  }
  get_experiment_info(resp_parsed$`_id`)
}

create_experiment_version <- function(experiment_id, dataset_id = NULL, target_column = NULL, holdout_dataset_id = NULL, id_column = NULL, drop_list = NULL, profile = NULL, experiment_description = NULL, metric = NULL, fold_column = NULL, normal_models = NULL, lite_models = NULL, simple_models = NULL, with_blend = NULL, weight_column = NULL, features_engineering_selected_list = NULL, features_selection_count = NULL, features_selection_time = NULL, folder_dataset_id = NULL, filename_column = NULL, ymin = NULL, ymax = NULL, xmin = NULL, xmax = NULL, time_column = NULL, start_dw = NULL, end_dw = NULL, start_fw = NULL, end_fw = NULL, group_list = NULL, apriori_list = NULL, content_column = NULL, queries_dataset_id = NULL, queries_dataset_content_column = NULL, queries_dataset_id_column = NULL, queries_dataset_matching_id_description_column = NULL, top_k = NULL, lang = NULL, models_params = NULL, name = NULL, onnx_file = NULL, yaml_file = NULL) {
  #' Create a new version of an existing experiment.
  #'
  #' @param experiment_id id of the experiment that will host the new version.
  #' @param dataset_id id of the dataset used for the training phase.
  #' @param target_column name of the TARGET column.
  #' @param holdout_dataset_id id of the holdout dataset.
  #' @param id_column name of the id column.
  #' @param drop_list list of names of features to drop.
  #' @param profile chosen profil among "quick", "normal", "advanced".
  #' @param experiment_description experiment description.
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
  #' @param name name of the external model (external model).
  #' @param onnx_file path to the onnx file (external model).
  #' @param yaml_file path to the yaml file (external model).
  #'
  #' @return list - experiment information.
  #'
  #' @import httr
  #'
  #' @export

  # CHECKING CONDITIONS FOR normal_models, lite_models and simple_models
  if(!all(normal_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "CB"))) {
    stop("normal_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"CB\" or \"NN\"")
  }
  if(!all(lite_models %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "NBC", "CB"))) {
    stop("lite_models must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"NN\", \"CB\" or \"NBC\"")
  }
  if(!all(simple_models %in% c("DT", "LR"))) {
    stop("simple models must be either \"DT\" or \"LR\"")
  }

  # GET PARAMS AND REMOVE NULL ONES
  params = list(dataset_id = dataset_id,
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

  params <- params[!sapply(params, is.null)]

  # CREATE EXPERIMENT VERSION
  resp <- pio_request(paste0('/experiments/', experiment_id, '/versions/'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    # IF EXPERIMENT IS ABOUT EXTERNAL MODEL ADD ONE STEP
    if(get_experiment_info(experiment_id)$provider == "external") {
      params = list(name = name,
                    onnx_file = upload_file(onnx_file),
                    yaml_file = upload_file(yaml_file, type = "text/x-yaml"))

      params <- params[!sapply(params, is.null)]
      resp <- pio_request(paste0('/experiment-versions/', get_experiment_info(experiment_id)$latest_experiment_version, '/external-models/'), POST, params, upload = TRUE)
    }

    # LAUNCH EXPERIMENT VERSION
    resp <- pio_request(paste0('/experiment-versions/', resp_parsed$`_id`, '/confirm/'), PUT)
    if(resp$status_code == 200) {
      message("version ", resp_parsed$version, " of experiment ", resp_parsed$`_id`, " created")
      return(get_experiment_version_info(resp_parsed$`_id`))
    }
  } else {
    stop("failed to create new experiment version - ", resp$status_code, ":", resp_parsed[[1]])
  }
}

update_experiment_version_description <- function(experiment_version_id, description = "") {
  #' Update the description of a given experiment_version_id.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #' @param description Description of the experiment.
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  params = list(description = description)

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id), PUT, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("description of the experiment_version ", experiment_version_id, " updated")
  } else {
    stop("failed to update the description of the experiment_version ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  return(resp$status_code)
}

delete_experiment <- function(experiment_id) {
  #' Delete a experiment on the platform.
  #'
  #' @param experiment_id id of the experiment, can be obtained with get_experiments().
  #'
  #' @return integer - 204 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiments/', experiment_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 204) {
    message("experiment ", experiment_id, " deleted")
  } else {
    stop("failed to delete experiment ", experiment_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

pause_experiment_version <- function(experiment_version_id) {
  #' Pause a running experiment_version on the platform.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/pause'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("experiment_version_id ", experiment_version_id, " paused")
  } else {
    stop("failed to pause experiment_version_id ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

resume_experiment_version <- function(experiment_version_id) {
  #' Resume a paused experiment_version on the platform.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/resume'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("experiment_version_id ", experiment_version_id, " resumed")
  } else {
    stop("failed to resume experiment_version_id ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

stop_experiment_version <- function(experiment_version_id) {
  #' Stop a running or paused experiment_version on the platform.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/experiment-versions/', experiment_version_id, '/stop'), PUT)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("experiment_version_id ", experiment_version_id, " stopped")
  } else {
    stop("failed to stop experiment_version_id ", experiment_version_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
  resp$status_code
}

get_model_cv <- function(model_id) {
  #' Get the cross validation file from a specific model.
  #'
  #' @param model_id id of the model to get the CV, can be obtained with get_experiment_version_models().
  #'
  #' @return data.frame - cross validation data coming from model_id.
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
    stop("can't retrieve CV file for model ", model_id)
  }
}

get_best_model_id <- function(experiment_version_id, include_blend = TRUE) {
  #' Get the model_id that provide the best predictive performance given experiment_version_id. If include_blend is false, it will return the model_id from the best "non blended" model.
  #'
  #' @param experiment_version_id id of the experiment_version, can be obtained with get_experiment_version_id().
  #' @param include_blend boolean, indicating if you want to retrieve the best model among blended models too.
  #'
  #' @return character - model_id.
  #'
  #' @import httr
  #'
  #' @export

  # GET MODELS FROM A USE CASE
  models = get_experiment_version_models(experiment_version_id)

  # LOOP OVER THEM AND RETRIEVE THE ONE MATCHING SELECTED CRITERIA
  for(model in models) {
    if(include_blend && !is.null(model$tags$best)) {
      return(model$`_id`)
    }
    if(!include_blend && !is.null(model$tags$recommended)) {
      return(model$`_id`)
    }
  }

  stop("no model found for experiment_version ", experiment_version_id)
}
