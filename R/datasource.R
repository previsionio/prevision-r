getDatasources <- function() {
  #' Get informations of all data sources available.
  #'
  #' @return parsed content of all datasources
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest('/datasources', GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed[["items"]]
  }
  else {
    stop("Can't retrieve datasources list - ", resp$status_code, ":", respParsed)
  }
}

getDatasourceInfos <- function(datasourceId) {
  #' Get a datasource from its id.
  #'
  #' @param datasourceId id of the datasources to be retrieved, can be obtained with getDatasources().
  #'
  #' @return parsed content of the datasources
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasources/', datasourceId), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve datasource ", datasourceId, " - ", resp$status_code, ":", respParsed)
  }
}

getDatasourceIdFromName <- function(datasourceName) {
  #' Get a datasourceId from a datasourceName If duplicated name, the first datasourceId that match it is retrieved
  #'
  #' @param datasourceName name of the connector we are searching its id from. Can be obtained with getDatasources().
  #'
  #' @return id of the datasource if found.
  #'
  #' @import httr
  #'
  #' @export

  datasourceList = getDatasources()
  for (datasource in datasourceList) {
    if(datasource$name == datasourceName) {
      return(datasource$`_id`)
    }
  }
  stop("There is no datasourceId matching the datasourceName ", datasourceName)
}

createDatasource <- function(connectorId, name, path= "", database= "", table = "", bucket = "", request = "") {
  #' Create a new datasource
  #'
  #' @param connectorId connectorId linked to the datasource.
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
  connectorsId = getConnectors()
  validId = NULL
  for (i in 1:length(connectorsId)) {
    validId = c(validId, connectorsId[[i]]$`_id`)
  }

  if(!connectorId %in% validId) {
    stop("ConnectorId ", connectorId, " is invalid")
  }

  params <- list(connectorId = connectorId,
                 name = name,
                 path = path,
                 database = database,
                 table = table,
                 bucket = bucket,
                 request = request)

  resp <- previsionioRequest('/datasources', POST, params)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Creation of datasource ", name, " OK - ", resp$status_code, ":", respParsed)
    respParsed
  } else {
    stop("Creation of datasource ", name, " KO - ", resp$status_code, ":", respParsed)
  }
}

deleteDatasource <- function(datasourceId) {
  #' Delete a datasource
  #'
  #' @param datasourceId id of the connector to be deleted, can be obtained with listConnectors().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasources/', datasourceId), DELETE)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Deletation of datasource ", datasourceId, " OK - ", resp$status_code, ":", respParsed)
    resp$status_code
  } else {
    stop("Deletion of datasource ", datasourceId, " KO - ", resp$status_code, ":", respParsed)
  }
}

testDatasource <- function(datasourceId) {
  #' Test a datasource
  #'
  #' @param datasourceId id of the datasource to be tested, can be obtained with getDatasources().
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/datasources/', datasourceId, "/test"), POST)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Test of datasource ", datasourceId, " OK - ", resp$status_code, ":", respParsed)
    resp$status_code
  } else {
    stop("Test of datasource ", datasourceId, " KO - ", resp$status_code, ":", respParsed)
  }
}
