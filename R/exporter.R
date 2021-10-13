get_exporters <- function(project_id) {
  #' Get information of all exporters available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return list - parsed content of all exporters for the supplied project_id.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  exporters = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/exporters?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      exporters = c(exporters, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve exporters list - ", resp$status_code, ":", resp_parsed)
    }
  }
  exporters
}

get_exporter_info <- function(exporter_id) {
  #' Get an exporter from its id.
  #'
  #' @param exporter_id id of the exporter to be retrieved, can be obtained with get_exporters().
  #'
  #' @return list - parsed content of the exporter.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/exporters/', exporter_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve exporter ", exporter_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_exporter_id_from_name <- function(project_id, exporter_name) {
  #' Get a exporter_id from a exporter_name. If duplicated name, the first exporter_id that match it is retrieved
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param exporter_name name of the exporter we are searching its id from. Can be obtained with get_exporters().
  #'
  #' @return character - id of the exporter if found.
  #'
  #' @import httr
  #'
  #' @export

  exporter_list = get_exporters(project_id)
  for (exporter in exporter_list) {
    if(exporter$name == exporter_name) {
      return(exporter$`_id`)
    }
  }
  stop("there is no exporter_id matching the exporter_name ", exporter_name)
}

create_exporter <- function(project_id, connector_id, name, description = "", filepath = "", file_write_mode = "timestamp", database = "", table = "", database_write_mode = "append", bucket = "") {
  #' Create a new exporter
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param connector_id connector_id linked to the exporter.
  #' @param name exporter name.
  #' @param description description of the exporter.
  #' @param filepath exporter path (for SFTP & FTP connector).
  #' @param file_write_mode writing type when exporting a file (for SFT & FTP connector, among \"timestamp\", \"safe\" or \"replace\")
  #' @param database exporter database (for SQL connector).
  #' @param table exporter table (for SQL connector).
  #' @param database_write_mode writing type when exporting data within a database (for SQL connector, among \"append\" or \"replace\").
  #' @param bucket exporter bucket (for S3 connector).
  #'
  #' @return list - parsed content of the exporter.
  #'
  #' @import httr
  #'
  #' @export

  # CHECKING THAT connector_id IS VALID
  connectors_id = get_connectors(project_id)
  valid_id = NULL
  for (i in 1:length(connectors_id)) {
    valid_id = c(valid_id, connectors_id[[i]]$`_id`)
  }

  if(!connector_id %in% valid_id) {
    stop("connector_id ", connector_id, " is invalid")
  }

  params <- list(connector_id = connector_id,
                 name = name,
                 description = description,
                 filepath = filepath,
                 file_write_mode = file_write_mode,
                 database = database,
                 table = table,
                 database_write_mode = database_write_mode,
                 bucket = bucket)

  resp <- pio_request(paste0('/projects/', project_id, '/exporters'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("creation of exporter ", name, " done")
    resp_parsed
  } else {
    stop("failed to create exporter ", name, " - ", resp$status_code, ":", resp_parsed)
  }
}

delete_exporter <- function(exporter_id) {
  #' Delete an exporter
  #'
  #' @param exporter_id id of the exporter to be deleted, can be obtained with get_exporters().
  #'
  #' @return integer - 204 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/exporters/', exporter_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 204) {
    message("exporter ", exporter_id, " deleted")
    resp$status_code
  } else {
    stop("failed to delete exporter ", exporter_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_exporter_exports <- function(exporter_id) {
  #' Get all exports done from an exporter_id
  #'
  #' @param exporter_id id of the exporter to retrieve information, can be obtained with get_exporters().
  #'
  #' @return list - list of exports of the supplied exporter_id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/exporters/', exporter_id, '/exports'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  } else {
    stop("can't retrieve exports from exporter_id ", exporter_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_export <- function(exporter_id, type, dataset_id = NULL, prediction_id = NULL) {
  #' Export data using an existing exporter and the resource to export
  #'
  #' @param exporter_id id of the exporter, can be obtained with get_exporters().
  #' @param type type of data to export among \"dataset"\, \"validation-prediction\" or \"deployment-prediction\"
  #' @param dataset_id id of the dataset to export (only for type == \"dataset\")
  #' @param prediction_id id of the prediction to export (only for type == \"validation_prediction\" or type == \"deployment-prediction\")
  #'
  #' @return list - parsed content of the export.
  #'
  #' @import httr
  #'
  #' @export

  # CHECKING type
  if(!type %in% c("dataset", "validation-prediction", "deployment-prediction")) {
    stop("type must be either \"dataset\", \"validation-prediction\" or \"deployment-prediction\"")
  }

  # DATASET EXPORT
  if(type == "dataset") {
    resp <- pio_request(paste0('/exporters/', exporter_id, '/dataset/', dataset_id), POST)
  }

  # VALIDATION PREDICTION EXPORT
  if(type == "validation-prediction") {
    resp <- pio_request(paste0('/exporters/', exporter_id, '/prediction/', prediction_id), POST, list(prediction_type = "experiment"))
  }

  # DEPLOYMENT PREDICTION EXPORT
  if(type == "deployment-prediction") {
    resp <- pio_request(paste0('/exporters/', exporter_id, '/prediction/', prediction_id), POST, list(prediction_type = "deployment"))
  }

  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("export of type ", type, " using exporter ", exporter_id, " done")
    resp_parsed
  } else {
    stop("failed to export data of type ", type, " using exporter ", exporter_id, " - ", resp$status_code, ":", resp_parsed)
  }
}
