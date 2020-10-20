getConnectors <- function() {
  #' Get informations of all connectors available.
  #'
  #' @return parsed content of all connectors
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  connectors = c()

  # Looping over page to get all informations
  while(T) {
    resp <- previsionioRequest(paste0('/connectors?page=', page), GET)
    respParsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(respParsed[["items"]])==0) {
        break
      }

      # Store items and continue
      connectors = c(connectors, respParsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve connectors list - ", resp$status_code, ":", respParsed)
    }
  }
  connectors
}

getConnectorInfos <- function(connectorId) {
  #' Get informations about connector from its id.
  #'
  #' @param connectorId id of the connector to be retrieved, can be obtained with getConnectors().
  #'
  #' @return parsed content of the connector.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/connectors/', connectorId), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve connector ", connectorId, " - ", resp$status_code, ":", respParsed)
  }
}

getConnectorIdFromName <- function(connectorName) {
  #' Get a connectorId from a connectorName. If duplicated name, the first connectorId that match it is retrieved
  #'
  #' @param connectorName name of the connector we are searching its id from. Can be obtained with getConnectors().
  #'
  #' @return id of the connector if found.
  #'
  #' @import httr
  #'
  #' @export

  connectorList = getConnectors()
  for (connector in connectorList) {
    if(connector$name == connectorName) {
      return(connector$`_id`)
    }
  }
  stop("There is no connectorId matching the connectorName ", connectorName)
}

createConnector <- function(type, name, host, port, username, password, googleCredentials = NULL) {
  #' Create a new connector of a supported type (among: "SQL", "HIVE", "FTP", "SFTP", "S3", "GCP")
  #'
  #' @param type connector type.
  #' @param name connector name.
  #' @param host connector host.
  #' @param port connector port.
  #' @param username connector username.
  #' @param password connector password.
  #' @param googleCredentials google credentials JSON (for GCP only).
  #'
  #' @return parsed content of the connector.
  #'
  #' @import httr
  #'
  #' @export

  supportedType = c("SQL", "HIVE", "FTP", "SFTP", "S3", "GCP")

  if(!(type %in% supportedType)) {
    stop("Connector type ", type, " is not in supported types : ", supportedType)
  }

  params <- list(type = type,
                 name = name,
                 host = host,
                 port = port,
                 username = username,
                 password = password,
                 googleCredentials = googleCredentials)

  resp <- previsionioRequest('/connectors', POST, params)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")
  if(resp$status_code == 200) {
    message("Creation of connector ", name, " done - ", resp$status_code, ":", respParsed)
    getConnectorInfos(respParsed$`_id`)
  } else {
    stop("Creation of connector ", name, " failed - ", resp$status_code, ":", respParsed)
  }
}

deleteConnector <- function(connectorId) {
  #' Delete a connector.
  #'
  #' @param connectorId id of the connector to be deleted, can be obtained with getConnectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/connectors/', connectorId), DELETE)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Deletation of connector ", connectorId, " done - ", resp$status_code, ":", respParsed)
    resp$status_code
  } else {
    stop("Deletion of connector ", connectorId, " failed - ", resp$status_code, ":", respParsed)
  }
}

testConnector <- function(connectorId) {
  #' Test a connector.
  #'
  #' @param connectorId id of the connector to be tested, can be obtained with getConnectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/connectors/', connectorId, "/test"), POST)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Test of connector ", connectorId, " done - ", resp$status_code, ":", respParsed)
    resp$status_code
  } else {
    stop("Test of connector ", connectorId, " failed - ", resp$status_code, ":", respParsed)
  }
}
