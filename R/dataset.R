getDatasets <- function() {
  #' Get informations of all tabular datasets availables.
  #'
  #' @return parsed content of all datasets.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  datasets = c()

  # Looping over page to get all information
  while(T) {
    resp <- previsionioRequest(paste0('/datasets/files?page=', page), GET)
    respParsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(respParsed[["items"]])==0) {
        break
      }

      # Store items and continue
      datasets = c(datasets, respParsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve datasets list - ", resp$status_code, ":", respParsed)
    }
  }
  datasets
}

getDatasetIdFromName <- function(datasetName) {
  #' Get a datasetId from a datasetName If duplicated name, the first datasetId that match it is retrieved
  #'
  #' @param datasetName name of the dataset we are searching its id from. Can be obtained with getDatasets().
  #'
  #' @return id of the dataset if found.
  #'
  #' @import httr
  #'
  #' @export

  datasetList = getDatasets()
  for (dataset in datasetList) {
    if(dataset$name == datasetName) {
      return(dataset$`_id`)
    }
  }
  stop("There is no datasetId matching the datasetName ", datasetName)
}

getDatasetInfos <- function(datasetId) {
  #' Get a dataset from its id.
  #'
  #' @param datasetId id of the dataset, can be obtained with getDatasets().
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import httr
  #'
  #' @export

  while (T) {
    resp <- previsionioRequest(paste0('/datasets/files/', datasetId), GET)
    ## IF STATUS == 200 BREAK IF DATASET IS "DONE"
    if(resp$status_code == 200) {
      respParsed <- content(resp, 'parsed')
      if(respParsed$ready == "done") {
        break
      }
    }

    ## PRINT MESSAGE AND WAIT 5 SECONDS BEFORE RETRYING
    message('Waiting for dataset')
    Sys.sleep(5)
  }
  respParsed
}

getDatasetHead <- function(datasetId) {
  #' Show the head of a dataset from its id.
  #'
  #' @param datasetId id of the dataset, can be obtained with getDatasets().
  #'
  #' @return head of the dataset as a data.frame object.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasets/files/', datasetId, '/head'), GET)
  respParsed <- content(resp, 'parsed')

  if(length(respParsed$columns) < 1) {
    stop(paste("Dataset error:", respParsed$message))
  } else {
    df <- data.frame(matrix(ncol=length(respParsed$columns), nrow=length(respParsed$rows)))
    for (i in 1:length(respParsed$columns)) {
      names(df)[i] = respParsed$columns[[i]]$name
    }
    for (i in 1:length(respParsed$rows)) {
      for (j in 1:length(respParsed$columns)) {
        df[i,j] = ifelse(is.null(respParsed$rows[[i]][[j]]), NA, respParsed$rows[[i]][[j]])
      }
    }
  }
  df
}

deleteDataset <- function(datasetId) {
  #' Delete a dataset on the platform.
  #'
  #' @param datasetId id of the dataset, can be obtained with getDatasets().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasets/files/', datasetId), DELETE)
  respParsed <- content(resp, 'parsed')
  flog.debug(respParsed)

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", respParsed$message)
    resp$status_code
  } else {
    stop("Delete KO - ", resp$status_code, ":", respParsed$message)
  }
}

createDatasetFromFilename <- function(datasetName, local_path) {
  #' Upload dataset from file name.
  #'
  #' @param datasetName given name of the dataset on the platform.
  #' @param local_path path to the dataset.
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = datasetName, file = upload_file(local_path))

  resp <- previsionioRequest('/datasets/files', POST, params, upload = TRUE)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    getDatasetInfos(respParsed$`_id`)
  } else {
    stop("Dataset upload failure - ", respParsed$status, ":", respParsed$message)
  }
}

createDatasetFromDataframe <- function(datasetName, dataframe, zip = F) {
  #' Upload dataset from data frame.
  #'
  #' @param datasetName given name of the dataset on the platform.
  #' @param dataframe data.frame to upload.
  #' @param zip is the temp file zipped before sending it to Prevision.io?
  #'
  #' @return parsed content of the dataset object
  #'
  #' @importFrom data.table fwrite
  #'
  #' @export

  tf <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  fwrite(dataframe, tf, dateTimeAs = "write.csv")
  if(zip) {
    message("Compressing file ", tf)
    zip(zipfile = paste0(tf, ".zip"), files = tf)
    res <- createDatasetFromFilename(datasetName = datasetName,
                                     local_path = paste0(tf, ".zip"))
    file.remove(tf)
    file.remove(paste0(tf, ".zip"))
  }
  else {
    res <- createDatasetFromFilename(datasetName = datasetName, local_path = tf)
    file.remove(tf)
  }
  res
}

createDatasetFromDatasource <- function(datasetName, datasourceId) {
  #' Create a dataset from an existing datasource.
  #'
  #' @param datasetName given name of the dataset on the platform.
  #' @param datasourceId datasource id.
  #'
  #' @import httr
  #'
  #' @return parsed content of the dataset object
  #'
  #' @export

  params <- list(name = datasetName, datasourceId = datasourceId)

  resp <- previsionioRequest('/datasets/files', POST, params)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    getDatasetInfos(respParsed$`_id`)
  } else {
    stop("Dataset creation failure - ", respParsed$status, ":", respParsed$message)
  }
}

createDataframeFromDataset <- function(datasetId) {
  #' Create a dataframe from a datasetId.
  #'
  #' @param datasetId dataset id.
  #'
  #' @return a R dataframe
  #'
  #' @import httr
  #'
  #' @import data.table
  #'
  #' @export

  path <- downloadDataset(datasetId)
  unzip(path, overwrite = T, exdir = datasetId)
  unlink(path)
  data <- fread(paste0(datasetId, "/", list.files(datasetId)))
  unlink(paste0(datasetId), recursive = T)
  data
}

downloadDataset <- function(datasetId, path = getwd(), isFolder = FALSE) {
  #' Download a dataset from a datasetId and write it in a folder.
  #'
  #' @param datasetId dataset id.
  #' @param path path (without / at the end) were to write the dataset
  #' @param isFolder boolean. TRUE if it's a folder dataset, FALSE (by default) otherwise.
  #'
  #' @return the complete path to the file written
  #'
  #' @import httr
  #'
  #' @export

  datasetName <- getDatasetInfos(datasetId)$name
  fileName <- paste0(datasetName, ".zip")
  completePath <- paste0(path, "/", fileName)

  if(isFolder) {
    resp <- previsionDownload(paste0('/datasets/folders/', datasetId, "/download"), completePath)
  }
  else {
    resp <- previsionDownload(paste0('/datasets/files/', datasetId, "/download"), completePath)
  }

  if(resp$status_code == 200) {
    message("Download of dataset ", datasetId, " done - ", completePath)
    completePath
  }
  else {
    stop("Download of dataset ", datasetId, " failed - ", completePath)
  }
}

startDatasetEmbedding <- function(datasetId) {
  #' Start a dataset embedding from a datasetId.
  #'
  #' @param datasetId dataset id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasets/files/', datasetId, "/start-embedding"), POST)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Embedding started - ", resp$status_code, ":", respParsed$message)
    resp$status_code
  } else {
    stop("Dataset embedding start failure - ", respParsed$status, ":", respParsed$message)
  }
}

getDatasetEmbedding <- function(datasetId) {
  #' Get a dataset embedding from a datasetId.
  #'
  #' @param datasetId dataset id.
  #'
  #' @import httr
  #'
  #' @import data.table
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasets/files/', datasetId, "/explorer"), GET)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Retrieving embedding")

    tensorShape = respParsed[["embeddings"]][[1]][["tensorShape"]]

    # respLabels  = previsionioRequest(paste0('/datasets/files/', datasetId, "/explorer/labels.bytes"), GET)
    respTensors = previsionioRequest(paste0('/datasets/files/', datasetId, "/explorer/tensors.bytes"), GET)

    # labels  = fread(content(respLabels, 'parsed', as = "text"), sep = "\t")
    tensors = data.table(matrix(readBin(respTensors$content, "numeric", n = tensorShape[[1]] * tensorShape[[2]], size = 4), nrow = tensorShape[[1]], ncol = tensorShape[[2]], byrow = T))
    tensors
  } else {
    stop("Can't retrieve dataset embedding - ", respParsed$status, ":", respParsed$message)
  }
}
