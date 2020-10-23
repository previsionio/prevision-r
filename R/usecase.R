getUsecases <- function() {
  #' Retrieves all usecases
  #'
  #' @return a usecase list.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  useCases = c()

  # Looping over page to get all informations
  while(T) {
    resp <- previsionioRequest(paste0('/usecases?page=', page), GET)
    respParsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(respParsed[["items"]])==0) {
        break
      }

      # Store items and continue
      useCases = c(useCases, respParsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve usecases list - ", resp$status_code, ":", respParsed)
    }
  }
  useCases
}

getUsecaseIdFromName <- function(usecaseName) {
  #' Get a usecaseId from a usecaseName If duplicated name, the first usecaseId that match it is retrieved
  #'
  #' @param usecaseName name of the usecase we are searching its id from. Can be obtained with getUsecases().
  #'
  #' @return usecaseId of the usecaseName if found.
  #'
  #' @import httr
  #'
  #' @export

  usecaseList = getUsecases()
  for (uc in usecaseList) {
    if(uc$name == usecaseName) {
      return(uc$usecaseId)
    }
  }
  stop("There is no usecaseId matching the usecaseName ", usecaseName)
}

getUsecaseInfos <- function(usecaseId, versionNumber = 1) {
  #' Get a usecase from its usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve informations from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getUsecaseTasks <- function(usecaseId, versionNumber = 1) {
  #' Get all tasks related to a usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase tasks
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/tasks'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve tasks from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getUsecaseSchema <- function(usecaseId, versionNumber = 1) {
  #' Get schema related to a usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase schema
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/schema'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve schema from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getUsecaseFeatures <- function(usecaseId, versionNumber = 1) {
  #' Get features informations related to a usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the usecase features informations
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/features'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve features information from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getFeaturesInfos <- function(usecaseId, featureName, versionNumber = 1) {
  #' Get information of a given feature related to a usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param featureName name of the feature to retrive information
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the specific feature
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/features/', featureName), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve information from feature ", featureName, " from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getUsecaseModels <- function(usecaseId, versionNumber = 1) {
  #' Get a model list related to a usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of models attached to usecaseId
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/models'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed[["items"]]
  }
  else {
    stop("Can't retrieve models from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getModelInfos <- function(usecaseId, modelId, versionNumber = 1) {
  #' Get model informations corresponding to usecaseId and modelId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param modelId id of the model, can be obtained with usecaseModels(usecaseId).
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the model
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/models/', modelId), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve model ", modelId, " from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getModelHyperparameters <- function(usecaseId, modelId, versionNumber = 1) {
  #' Get hyperparameters corresponding to usecaseId and modelId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param modelId id of the model, can be obtained with usecaseModels(usecaseId).
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed content of the model's hyperparameters
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/models/', modelId, '/download/hyperparameters'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve hyperparameters of model ", modelId, " from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getModelFeatureImportance <- function(usecaseId, modelId, versionNumber = 1) {
  #' Get feature importance corresponding to usecaseId and modelId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param modelId id of the model, can be obtained with usecaseModels(usecaseId).
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return dataset of the model's feature importance
  #'
  #' @import httr
  #' @importFrom data.table fread
  #'
  #' @export

  temp <- tempfile()
  resp <- previsionDownload(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/models/', modelId, '/download/features-importance'), temp)

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    data
  }
  else {
    stop("Can't retrieve feature importance of model ", modelId, " from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getUsecasePredictions <- function(usecaseId, generatingType = "user", versionNumber = 1) {
  #' Get a usecase from its usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param generatingType can be "user" (= user predictions) or "auto" (= hold out predictions)
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed prediction list items
  #'
  #' @import httr
  #'
  #' @export

  if(!generatingType %in% c("user", "auto")) {
    stop("generatingType should be equal to \"user\" or \"auto\"")
  }

  page = 1
  predictions = c()

  # Looping over page to get all informations
  while(T) {
    resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions?page=', page, '?generatingType=', generatingType, '/'), GET)
    respParsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(respParsed[["items"]])==0) {
        break
      }

      # Store items and continue
      predictions = c(predictions, respParsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve predictions from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
    }
  }
  predictions
}

startPrediction <- function(usecaseId, datasetId, datasetFolderId = NULL, confidence = F, bestSingle = F, modelId = NULL, versionNumber = 1) {
  #' Start a prediction on a existing usecase with a given datasetId and a given version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param datasetId id of the dataset to start the prediction on, can be obtained with getUsecases()
  #' @param datasetFolderId id of the folder dataset to start prediction on, can be obtained with getUsecases(). Only usefull for images use cases
  #' @param confidence boolean. If enable, confidence intervalle will be added to predictions
  #' @param bestSingle boolean. If enable, best single model (non blend) will be used for making predictions other wise, best model will be used unless if modelId is fed
  #' @param modelId id of the model to start the prediction on. If provided, it will overwrite the "best single" params
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed prediction list
  #'
  #' @import httr
  #'
  #' @export

  params = list(datasetId = datasetId,
                datasetFolderId = datasetFolderId,
                confidence = confidence,
                bestSingle = bestSingle,
                modelId = modelId)

  params <- params[!sapply(params, is.null)]

  if(!is.null(modelId)){
    message("modelId is set, the bestSingle param won't be taken into account")
  }

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions'), POST, params)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 202) {
    respParsed
  }
  else {
    stop("Can't start prediction for usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getPredictionInfos <- function(usecaseId, predictionId, versionNumber = 1) {
  #' Get a informations about a prediction from a specific usecase / version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param predictionId id of the prediction to be retrieved, can be obtained with usecasePredictions()
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return list of prediction informations
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions/', predictionId), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  } else {
    stop("Can't retrieve prediction infos of prediction ", predictionId, " for usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

getPrediction <- function(usecaseId, predictionId, versionNumber = 1) {
  #' Get a specific prediction from a specific usecase / version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param predictionId id of the prediction to be retrieved, can be obtained with usecasePredictions()
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return a data.frame with the predictions
  #'
  #' @import httr
  #' @importFrom data.table fread
  #'
  #' @export

  temp <- tempfile()
  resp <- previsionDownload(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions/', predictionId, '/download'), temp)

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    file.remove(temp)
    data
  } else {
    stop("Can't retrieve prediction prediction ", predictionId, " for usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

deletePrediction <- function(usecaseId, predictionId, versionNumber = 1) {
  #' Delete a prediction of a given model and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param predictionId id of the prediction to be retrieved, can be obtained with usecasePredictions()
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @return list of predictions of usecaseId
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions/', predictionId), DELETE)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Delete KO - ", resp$status_code, ":", respParsed$message)
  }
  getUsecasePredictions(usecaseId)
}

getPredictionEvents <- function(usacaseId, versionNumber = 1) {
  #' Get events for prediction of usecaseId and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed event list
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/predictions/events'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve prediction events from usecase ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed)
  }
}

shareUsecase <- function(usecaseId, email) {
  #' Share a usecase to a specific user
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param email email adress of the user you want to share the usecase with. Should be a platform user.
  #'
  #' @import httr
  #'
  #' @return list of shared users
  #'
  #' @export

  params = list(email = email)

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/sharing'), POST, params)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Usecase ", usecaseId, " shared to the user ", email)
  } else {
    message("Sharing of the usecase ", usecaseId, " for user ", email, " failed - ", resp$status_code, ":", respParsed$message)
  }
  getSharedUsecaseUsers(usecaseId)
}

unshareUsecase <- function(usecaseId, email = NULL) {
  #' Unshare a use case for the specified email adress. If missing, the usecase will be unshared from all users
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param email email adress of the user you want to unshare the usecase with. Should be an instance user.
  #'
  #' @import httr
  #'
  #' @return list of shared users
  #'
  #' @export

  if(is.null(email)) {
    resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/sharing'), DELETE)
  }
  else {
    resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/sharing/', email), DELETE)
  }

  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Usecase ", usecaseId, " unshared")
  } else {
    message("Unsharing of the usecase ", usecaseId, " failed - ", resp$status_code, ":", respParsed$message)
  }
  getSharedUsecaseUsers(usecaseId)
}

getSharedUsecaseUsers <- function(usecaseId) {
  #' Get the list of users that can access the usecase
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/1'), GET)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    respParsed[["shareList"]]
  } else {
    stop("Can't get the list of the users that can access the usecase ", usecaseId, " failed - ", resp$status_code, ":", respParsed$message)
  }
}

startUsecase <- function(name, dataType, trainingType, datasetId, targetColumn = NULL, holdoutDatasetId = NULL, idColumn = NULL, dropList = NULL, profile = NULL, metric = NULL, foldColumn = NULL, normalModels = NULL, liteModels = c('XGB'), simpleModels = NULL, withBlend = NULL, weightColumn = NULL, featuresEngineeringSelectedList = NULL, featuresSelectionCount = NULL, featuresSelectionTime = NULL, datasetFolderId = NULL, filenameColumn = NULL, topColumn = NULL, bottomColumn = NULL, leftColumn = NULL, rightColumn = NULL, timeColumn = NULL, startDW = NULL, endDW = NULL, startFW = NULL, endFW = NULL, groupList = NULL, aprioriList = NULL, experimentalTimeseries = FALSE) {
  #' Start a new usecase on the platform.
  #'
  #' @param name name of the usecase.
  #' @param dataType type of data ("tabular" or "images" or "timeseries").
  #' @param trainingType type of the training you want to achieve ("regression", "classification", "multiclassification", "clustering", "object-detection").
  #' @param datasetId id of the dataset used for the training phase.
  #' @param targetColumn name of the TARGET column
  #' @param holdoutDatasetId id of the houldout dataset
  #' @param idColumn name of the id column
  #' @param dropList list of names of features to drop
  #' @param profile chosen profil among "quick", "normal", "advanced"
  #' @param metric name of the metric to optimise
  #' @param foldColumn name of the fold column
  #' @param normalModels list of (normal) models to select with full FE & hyperparameters search
  #' @param liteModels list of (lite) models to select with lite FE & default hyperparameters
  #' @param simpleModels list of simple models to select
  #' @param withBlend do we allow to include blend in the modelisation
  #' @param weightColumn name of the weight columns
  #' @param featuresEngineeringSelectedList list of feature engineering to select among "Counter", "Date", "freq", "text_tfidf", "text_word2vec", "text_embedding", "tenc", "poly", "pca", "kmean"
  #' @param featuresSelectionCount number of features to keep after the feature selection process
  #' @param featuresSelectionTime time budget in minutes of the feature selection process
  #' @param datasetFolderId id of the dataset fold (images)
  #' @param filenameColumn name of the file name path (images)
  #' @param topColumn name of the top column (object detection)
  #' @param bottomColumn name of the bottom column (object detection)
  #' @param leftColumn name of the left column (object detection)
  #' @param rightColumn name of the right column (object detection)
  #' @param timeColumn name of the time column (time series)
  #' @param startDW value of the start of derivative window (time series), should be a strict negative integer
  #' @param endDW value of the end of derivative window (time series), should be a negative integer greater than startDW
  #' @param startFW value of the start of forecast window (time series), should be a strict positive integer
  #' @param endFW value of the end of forecast window (time series), should be a strict positive integer greater than startFW
  #' @param groupList list of name of feature that describes groups (time series)
  #' @param aprioriList list of name of feature that are a priori (time series)
  #' @param experimentalTimeseries boolean that indicated if we use experimental time series (not recommanded)
  #'
  #' @import httr
  #'
  #' @return usecase infos
  #'
  #' @export

  # CHECKING dataType
  if(!dataType %in% c("tabular", "images", "timeseries")) {
    stop("dataType must be either \"tabular\", \"images\" or \"timeseries\"")
  }

  # CHECKING trainingType
  if(!trainingType %in% c("regression", "classification", "multiclassification", "object-detection")) {
    stop("trainingType must be either \"regression\", \"classification\", \"multiclassification\" or \"object-detection\"")
  }

  # CHECKING datasetId EXISTS
  if(!datasetId %in% unlist(getDatasets())) {
    stop("datasetId doesn't exist")
  }

  # CHECKING CONDITIONS FOR normalModels, liteModels and simpleModels
  if (length(normalModels) + length(liteModels) + length(simpleModels) < 1) {
    stop("must give at least one model")
  }
  if(!all(normalModels %in% c("LR", "RF", "ET", "XGB", "LGB", "NN"))) {
    stop("normalModels must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\" or \"NN\"")
  }
  if(!all(liteModels %in% c("LR", "RF", "ET", "XGB", "LGB", "NN", "NBC"))) {
    stop("liteModels must be either \"LR\", \"RF\", \"ET\", \"XGB\", \"LGB\", \"NN\" or \"NBC\"")
  }
  if(!all(simpleModels %in% c("DT", "LR"))) {
    stop("simple models must be either \"DT\" or \"LR\"")
  }
  if(!trainingType %in% c("classification", "multiclassification") & "NBC" %in% liteModels) {
    stop("NBC liteModel is only available for classification or multiclassification")
  }


# GET PARAMS AND REMOVE NULL ONES
ucParams = list(name = name,
                datasetId = datasetId,
                targetColumn = targetColumn,
                holdoutDatasetId = holdoutDatasetId,
                idColumn = idColumn,
                dropList = dropList,
                profile = profile,
                metric = metric,
                foldColumn = foldColumn,
                normalModels = normalModels,
                liteModels = liteModels,
                simpleModels = simpleModels,
                withBlend = withBlend,
                weightColumn = weightColumn,
                featuresEngineeringSelectedList = featuresEngineeringSelectedList,
                featuresSelectionCount = featuresSelectionCount,
                featuresSelectionTime = featuresSelectionTime,
                datasetFolderId = datasetFolderId,
                filenameColumn = filenameColumn,
                topColumn = topColumn,
                bottomColumn = bottomColumn,
                leftColumn = leftColumn,
                rightColumn = rightColumn,
                timeColumn = timeColumn,
                startDW = startDW,
                endDW = endDW,
                startFW = startFW,
                endFW = endFW,
                groupList = groupList,
                aprioriList = aprioriList,
                experimentalTimeseries = experimentalTimeseries)

ucParams <- ucParams[!sapply(ucParams, is.null)]

resp <- previsionioRequest(paste0('/usecases/', dataType, '/', trainingType), POST, ucParams)
respParsed <- content(resp, 'parsed')

if(resp$status_code == 202) {
  message("Usecase started - ", resp$status_code, ":", respParsed$message)
} else {
  stop("Usecase starting failed - ", resp$status_code, ":", respParsed$message)
}
getUsecaseInfos(respParsed$`_id`)
}

updateUsecaseDescription <- function(usecaseId, description = "", versionNumber = 1) {
  #' Update the description of a given usecase and its version number
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param description Description of the usecase.
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  params = list(description = description)

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber), PUT, params)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Description of the usecase ", usecaseId, " version ", versionNumber, " updated - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Update of the description of the usecase ", usecaseId, " version ", versionNumber, " failed - ", resp$status_code, ":", respParsed$message)
  }
  getUsecaseInfos(usecaseId)
}

deleteUsecase <- function(usecaseId, versionNumber = 1) {
  #' Delete a version of a usecase on the platform
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber), DELETE)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Delete KO - ", resp$status_code, ":", respParsed$message)
  }
  resp$status_code
}

pauseUsecase <- function(usecaseId, versionNumber = 1) {
  #' Pause a version of a usecase on the platform
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/pause'), PUT)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Pausing OK - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Pausing KO - ", resp$status_code, ":", respParsed$message)
  }
  resp$status_code
}

resumeUsecase <- function(usecaseId, versionNumber = 1) {
  #' Resume a version of usecase on the platform
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/resume'), PUT)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Resuming OK - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Resuming KO - ", resp$status_code, ":", respParsed$message)
  }
  resp$status_code
}

stopUsecase <- function(usecaseId, versionNumber = 1) {
  #' Stop a version of a usecase on the platform
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/stop'), PUT)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Stop OK - ", resp$status_code, ":", respParsed$message)
  } else {
    message("Stop KO - ", resp$status_code, ":", respParsed$message)
  }
  resp$status_code
}

getUsecaseCV <- function(usecaseId, modelId, versionNumber = 1) {
  #' Get the cross validation file from a specific model of a given usecase / version
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param modelId id of the model to get the CV, can be obtained with usecaseModels(usecaseId).
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return a dataframe
  #'
  #' @import httr
  #' @importFrom data.table fread
  #'
  #' @export

  temp <- tempfile()
  resp <- previsionDownload(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/models/', modelId, '/download/cv/'), temp)

  if(resp$status_code == 200) {
    data <- fread(unzip(temp))
    file.remove(unzip(temp))
    file.remove(temp)
    data
  } else {
    stop("Can't retrieve CV file for model ", modelId, " of use case ", usecaseId, " version ", versionNumber, " - ", resp$status_code, ":", respParsed$message)
  }
}

getUsecaseEvents <- function(usecaseId = NULL, versionNumber = 1) {
  #' Get events of a version of ausecase. If no usecaseId provided, get events from all usecases
  #'
  #' @param usecaseId id of the usecase, can be obtained with getUsecases().
  #' @param versionNumber number of the version of the usecase. 1 by default.
  #'
  #' @return parsed event list
  #'
  #' @import httr
  #'
  #' @export

  if(is.null(usecaseId)) {
    resp <- previsionioRequest('/usecases/events', GET)
  }
  else {
    resp <- previsionioRequest(paste0('/usecases/', usecaseId, '/versions/', versionNumber, '/events'), GET)
  }

  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve events - ", resp$status_code, ":", respParsed)
  }
}

### HELPER
# ADD A GET BEST MODEL ID
# ADD A GET BEST SINGLE MODEL ID
