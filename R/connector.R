get_connectors <- function() {
  #' Get information of all connectors available.
  #'
  #' @return parsed content of all connectors.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  connectors = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/connectors?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      connectors = c(connectors, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve connectors list - ", resp$status_code, ":", resp_parsed)
    }
  }
  connectors
}

get_connector_info <- function(connector_id) {
  #' Get information about connector from its id.
  #'
  #' @param connector_id id of the connector to be retrieved, can be obtained with get_connectors().
  #'
  #' @return parsed content of the connector.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/connectors/', connector_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve connector ", connector_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_connector_id_from_name <- function(connector_name) {
  #' Get a connector_id from a connector_name. If duplicated name, the first connector_id that match it is retrieved.
  #'
  #' @param connector_name name of the connector we are searching its id from. Can be obtained with get_connectors().
  #'
  #' @return id of the connector if found.
  #'
  #' @import httr
  #'
  #' @export

  connector_list = get_connectors()
  for (connector in connector_list) {
    if(connector$name == connector_name) {
      return(connector$`_id`)
    }
  }
  stop("There is no connector_id matching the connector_name ", connector_name)
}

create_connector <- function(type, name, host, port, username, password, google_credentials = NULL) {
  #' Create a new connector of a supported type (among: "SQL", "HIVE", "FTP", "SFTP", "S3", "GCP").
  #'
  #' @param type connector type.
  #' @param name connector name.
  #' @param host connector host.
  #' @param port connector port.
  #' @param username connector username.
  #' @param password connector password.
  #' @param google_credentials google credentials JSON (for GCP only).
  #'
  #' @return parsed content of the connector.
  #'
  #' @import httr
  #'
  #' @export

  supported_type = c("SQL", "HIVE", "FTP", "SFTP", "S3", "GCP")

  if(!(type %in% supported_type)) {
    stop("Connector type ", type, " is not in supported types : ", supported_type)
  }

  params <- list(type = type,
                 name = name,
                 host = host,
                 port = port,
                 username = username,
                 password = password,
                 googleCredentials = google_credentials)

  resp <- pio_request('/connectors', POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")
  if(resp$status_code == 200) {
    message("Creation of connector ", name, " done - ", resp$status_code, ":", resp_parsed)
    get_connector_info(resp_parsed$`_id`)
  }
  else {
    stop("Creation of connector ", name, " failed - ", resp$status_code, ":", resp_parsed)
  }
}

delete_connector <- function(connector_id) {
  #' Delete an existing connector.
  #'
  #' @param connector_id id of the connector to be deleted, can be obtained with get_connectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/connectors/', connector_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Deletion of connector ", connector_id, " done - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  }
  else {
    stop("Deletion of connector ", connector_id, " failed - ", resp$status_code, ":", resp_parsed)
  }
}

test_connector <- function(connector_id) {
  #' Test an existing connector.
  #'
  #' @param connector_id id of the connector to be tested, can be obtained with get_connectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/connectors/', connector_id, "/test"), POST)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Test of connector ", connector_id, " done - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  } else {
    stop("Test of connector ", connector_id, " failed - ", resp$status_code, ":", resp_parsed)
  }
}
