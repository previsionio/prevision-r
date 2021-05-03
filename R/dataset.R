get_datasets <- function(project_id) {
  #' Get information of all datasets available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  datasets = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/datasets?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      datasets = c(datasets, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("Can't retrieve datasets list - ", resp$status_code, ":", resp_parsed)
    }
  }
  datasets
}

get_dataset_id_from_name <- function(project_id, dataset_name) {
  #' Get a dataset_id from a dataset_name. If duplicated name, the first dataset_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param dataset_name name of the dataset we are searching its id from. Can be obtained with get_datasets().
  #'
  #' @return id of the dataset if found.
  #'
  #' @import httr
  #'
  #' @export

  dataset_list = get_datasets(project_id)
  for (dataset in dataset_list) {
    if(dataset$name == dataset_name) {
      return(dataset$`_id`)
    }
  }
  stop("There is no dataset_id matching the dataset_name ", dataset_name)
}

get_dataset_info <- function(dataset_id) {
  #' Get a dataset from its id.
  #'
  #' @param dataset_id id of the dataset, can be obtained with get_datasets().
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import httr
  #'
  #' @export

  while (T) {
    resp <- pio_request(paste0('/datasets/', dataset_id), GET)

    ## IF STATUS == 200 BREAK IF DATASET IS "DONE"
    if(resp$status_code == 200) {
      resp_parsed <- content(resp, 'parsed')
      if(resp_parsed$isAvailable) {
        break
      }
    }

    ## PRINT MESSAGE AND WAIT 5 SECONDS BEFORE RETRYING
    message('Waiting for dataset')
    Sys.sleep(5)
  }
  resp_parsed
}

get_dataset_head <- function(dataset_id) {
  #' Show the head of a dataset from its id.
  #'
  #' @param dataset_id id of the dataset, can be obtained with get_datasets().
  #'
  #' @return head of the dataset as a data.frame object.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/datasets/', dataset_id, '/sample'), GET)
  resp_parsed <- content(resp, 'parsed')

  if(length(resp_parsed$columns) < 1) {
    stop(paste("Dataset error:", resp_parsed$message))
  } else {
    df <- data.frame(matrix(ncol=length(resp_parsed$columns), nrow=length(resp_parsed$rows)))
    for (i in 1:length(resp_parsed$columns)) {
      names(df)[i] = resp_parsed$columns[[i]]$name
    }
    for (i in 1:length(resp_parsed$rows)) {
      for (j in 1:length(resp_parsed$columns)) {
        df[i,j] = ifelse(is.null(resp_parsed$rows[[i]][[j]]), NA, resp_parsed$rows[[i]][[j]])
      }
    }
  }
  df
}

delete_dataset <- function(dataset_id) {
  #' Delete an existing dataset.
  #'
  #' @param dataset_id id of the dataset, can be obtained with get_datasets().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/datasets/', dataset_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 204) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
    resp$status_code
  } else {
    stop("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
}

create_dataset_from_file <- function(project_id, dataset_name, file, separator = ",", decimal = ".") {
  #' Upload dataset from file name.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param dataset_name given name of the dataset on the platform.
  #' @param file path to the dataset.
  #' @param separator column separator in the file (default: ",")
  #' @param decimal decimal separator in the file (default: ".")
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = dataset_name, file = upload_file(file), separator = separator, decimal = decimal)

  resp <- pio_request(paste0('/projects/', project_id, '/datasets/file'), POST, params, upload = TRUE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    get_dataset_info(resp_parsed$`_id`)
  } else {
    stop("Dataset upload failed - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

create_dataset_from_dataframe <- function(project_id, dataset_name, dataframe, zip = F) {
  #' Upload dataset from data frame.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param dataset_name given name of the dataset on the platform.
  #' @param dataframe data.frame to upload.
  #' @param zip is the temp file zipped before sending it to Prevision.io (default = F).
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import data.table
  #'
  #' @export

  tf <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
  fwrite(dataframe, tf, dateTimeAs = "write.csv")
  if(zip) {
    message("Compressing file ", tf)
    zip(zipfile = paste0(tf, ".zip"), files = tf)
    res <- create_dataset_from_file(project_id = project_id,
                                    dataset_name = dataset_name,
                                    file = paste0(tf, ".zip"))
    file.remove(tf)
    file.remove(paste0(tf, ".zip"))
  }
  else {
    res <- create_dataset_from_file(project_id = project_id,
                                    dataset_name = dataset_name,
                                    file = tf)
    file.remove(tf)
  }
  res
}

create_dataset_from_datasource <- function(project_id, dataset_name, datasource_id) {
  #' Create a dataset from an existing datasource.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param dataset_name given name of the dataset on the platform.
  #' @param datasource_id datasource id.
  #'
  #' @import httr
  #'
  #' @return parsed content of the dataset.
  #'
  #' @export

  params <- list(name = dataset_name, datasource_id = datasource_id)

  resp <- pio_request(paste0('/projects/', project_id, '/datasets/data-source'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    get_dataset_info(resp_parsed$`_id`)
  } else {
    stop("Dataset creation failed - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

create_dataframe_from_dataset <- function(dataset_id, path = getwd(), is_folder = FALSE) {
  #' Create a dataframe from a dataset_id.
  #'
  #' @param dataset_id dataset id.
  #' @param path path (without / at the end) were to write the downloaded dataset.
  #' @param is_folder TRUE if it's a folder dataset, FALSE (by default) otherwise.
  #'
  #' @return a R dataframe.
  #'
  #' @import httr
  #' @import data.table
  #'
  #' @export

  dataset_name <- get_dataset_info(dataset_id)$name
  file_name <- paste0(dataset_name, ".zip")
  complete_path <- paste0(path, "/", file_name)

  if(is_folder) {
    resp <- pio_download(paste0('/image-folders/', dataset_id, "/download"), complete_path)
  }
  else {
    resp <- pio_download(paste0('/datasets/', dataset_id, "/download"), complete_path)
  }

  if(resp$status_code == 200) {
    message("Download of dataset ", dataset_id, " done - ", complete_path)
    path <- complete_path
  }
  else {
    stop("Download of dataset ", dataset_id, " failed - ", complete_path)
  }

  unzip(path, overwrite = T, exdir = dataset_id)
  unlink(path)
  data <- fread(paste0(dataset_id, "/", list.files(dataset_id)))
  unlink(paste0(dataset_id), recursive = T)
  data
}

create_dataset_embedding <- function(dataset_id) {
  #' Create a dataset embedding from a dataset_id.
  #'
  #' @param dataset_id dataset id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/datasets/', dataset_id, "/analysis"), POST)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Embedding started - ", resp$status_code, ":", resp_parsed$message)
    resp$status_code
  } else {
    stop("Dataset embedding creation failed - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

get_dataset_embedding <- function(dataset_id) {
  #' Get a dataset embedding from a dataset_id.
  #'
  #' @param dataset_id dataset id.
  #'
  #' @import httr
  #'
  #' @import data.table
  #'
  #' @export

  resp <- pio_request(paste0('/datasets/', dataset_id, '/explorer'), GET)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Retrieving embedding")

    tensor_shape = resp_parsed[["embeddings"]][[1]][["tensorShape"]]

    # respLabels  = pio_request(paste0('/datasets/file/', dataset_id, "/explorer/labels.bytes"), GET)
    resp_tensors = pio_request(paste0('/datasets/file/', dataset_id, "/explorer/tensors.bytes"), GET)

    # labels  = fread(content(respLabels, 'parsed', as = "text"), sep = "\t")
    tensors = data.table(matrix(readBin(resp_tensors$content, "numeric", n = tensor_shape[[1]] * tensor_shape[[2]], size = 4), nrow = tensor_shape[[1]], ncol = tensor_shape[[2]], byrow = T))
    tensors
  }
  else {
    stop("Can't retrieve dataset embedding - ", resp_parsed$status, ":", resp_parsed$message)
  }
}
