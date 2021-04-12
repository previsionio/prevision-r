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
    resp <- pio_request(paste0('/project/', project_id, '/data-sources?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      data_sources = c(data_sources, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve data_sources list - ", resp$status_code, ":", resp_parsed)
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
    stop("Can't retrieve datasource ", datasource_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_datasource_id_from_name <- function(project_id, datasource_name) {
  #' Get a datasource_id from a datasource_name If duplicated name, the first datasource_id that match it is retrieved
  #'
  #' @param project_id id of the project, can be obtained with get_projects(project_id).
  #' @param datasource_name name of the connector we are searching its id from. Can be obtained with get_datasources().
  #'
  #' @return id of the datasource if found.
  #'
  #' @import httr
  #'
  #' @export

  datasource_list = get_datasources()
  for (datasource in datasource_list) {
    if(datasource$name == datasource_name) {
      return(datasource$`_id`)
    }
  }
  stop("There is no datasource_id matching the datasource_name ", datasource_name)
}

create_datasource <- function(project_id, connector_id, name, path= "", database= "", table = "", bucket = "", request = "") {
  #' Create a new datasource
  #'
  #' @param project_id id of the project, can be obtained with get_projects(project_id).
  #' @param connector_id connector_id linked to the datasource.
  #' @param name datasource name.
  #' @param path datasource path (for SFTP & FTP connector).
  #' @param database datasource database (for SQL & HIVE & HBASE connector).
  #' @param table datasource table (for SQL & HIVE & HBASE connector).
  #' @param bucket datasource bucket (for S3 connector).
  #' @param request datasource request (for SQL & HIVE connector).
  #'
  #' @return parsed content of the datasource
  #'
  #' @import httr
  #'
  #' @export

  # CHECKING THAT CONNECTORID IS VALID
  connectors_id = get_connectors(project_id)
  valid_id = NULL
  for (i in 1:length(connectors_id)) {
    valid_id = c(valid_id, connectors_id[[i]]$`_id`)
  }

  if(!connector_id %in% valid_id) {
    stop("connector_id ", connector_id, " is invalid")
  }

  params <- list(connectorId = connector_id,
                 name = name,
                 path = path,
                 database = database,
                 table = table,
                 bucket = bucket,
                 request = request)

  resp <- pio_request(paste0('/project/', project_id, '/datasources'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Creation of datasource ", name, " OK - ", resp$status_code, ":", resp_parsed)
    resp_parsed
  } else {
    stop("Creation of datasource ", name, " KO - ", resp$status_code, ":", resp_parsed)
  }
}

delete_datasource <- function(datasource_id) {
  #' Delete a datasource
  #'
  #' @param datasource_id id of the connector to be deleted, can be obtained with listConnectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/datasources/', datasource_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Deletation of datasource ", datasource_id, " OK - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  } else {
    stop("Deletion of datasource ", datasource_id, " KO - ", resp$status_code, ":", resp_parsed)
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

  resp <- pio_request(paste0('/datasources/', datasource_id, "/test"), POST)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Test of datasource ", datasource_id, " OK - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  } else {
    stop("Test of datasource ", datasource_id, " KO - ", resp$status_code, ":", resp_parsed)
  }
}
