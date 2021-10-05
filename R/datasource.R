get_datasources <- function(project_id) {
  #' Get information of all data sources available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return parsed content of all data_sources for the suppled project_id.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  data_sources = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/data-sources?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      data_sources = c(data_sources, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve data_sources list - ", resp$status_code, ":", resp_parsed)
    }
  }
  data_sources
}

get_datasource_info <- function(datasource_id) {
  #' Get a datasource from its id.
  #'
  #' @param datasource_id id of the data_sources to be retrieved, can be obtained with get_datasources().
  #'
  #' @return parsed content of the data_sources
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/data-sources/', datasource_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve datasource ", datasource_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_datasource_id_from_name <- function(project_id, datasource_name) {
  #' Get a datasource_id from a datasource_name If duplicated name, the first datasource_id that match it is retrieved
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param datasource_name name of the datasource we are searching its id from. Can be obtained with get_datasources().
  #'
  #' @return id of the datasource if found.
  #'
  #' @import httr
  #'
  #' @export

  datasource_list = get_datasources(project_id)
  for (datasource in datasource_list) {
    if(datasource$name == datasource_name) {
      return(datasource$`_id`)
    }
  }
  stop("there is no datasource_id matching the datasource_name ", datasource_name)
}

create_datasource <- function(project_id, connector_id, name, path= "", database= "", table = "", bucket = "", request = "") {
  #' Create a new datasource
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param connector_id connector_id linked to the datasource.
  #' @param name datasource name.
  #' @param path datasource path (for SFTP & FTP connector).
  #' @param database datasource database (for SQL connector).
  #' @param table datasource table (for SQL connector).
  #' @param bucket datasource bucket (for S3 connector).
  #' @param request datasource request (for SQLconnector).
  #'
  #' @return parsed content of the datasource
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
                 path = path,
                 database = database,
                 table = table,
                 bucket = bucket,
                 request = request)

  resp <- pio_request(paste0('/projects/', project_id, '/data-sources'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("datasource ", name, " created")
    resp_parsed
  } else {
    stop("failed to create datasource ", name, " - ", resp$status_code, ":", resp_parsed)
  }
}

delete_datasource <- function(datasource_id) {
  #' Delete a datasource
  #'
  #' @param datasource_id id of the datasource to be deleted, can be obtained with get_datasources().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/data-sources/', datasource_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("datasource ", datasource_id, " deleted")
    resp$status_code
  } else {
    stop("failed to delete datasource ", datasource_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

test_datasource <- function(datasource_id) {
  #' Test a datasource
  #'
  #' @param datasource_id id of the datasource to be tested, can be obtained with get_datasources().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/data-sources/', datasource_id, "/test"), POST)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("test of datasource ", datasource_id, " successful")
    resp$status_code
  } else {
    stop("failed to test the datasource ", datasource_id, " - ", resp$status_code, ":", resp_parsed)
  }
}
