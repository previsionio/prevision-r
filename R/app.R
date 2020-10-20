getApps <- function() {
  #' Retrieves all applications available
  #'
  #' @return an app list.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest('/app', GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve app list - ", resp$status_code, ":", respParsed)
  }
}

getApp <- function(appId) {
  #' Get an app from its id.
  #'
  #' @param appId id of the app to be retrieved, can be obtained with listApps().
  #'
  #' @return parsed content of the app.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/app/', appId), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve app ", appId, " - ", resp$status_code, ":", respParsed)
  }
}

getAppIdFromName <- function(appName) {
  #' Get an appId from an appName If duplicated name, the first appId that match it is retrieved
  #'
  #' @param appName name of the app we are searching its id from. Can be obtained with listApps().
  #'
  #' @return id of the app if found.
  #'
  #' @import httr
  #'
  #' @export

  appList = getApps()
  for (app in appList) {
    if(app$appName == appName) {
      return(app$`_id`)
    }
  }
  stop("There is no appId matching the appName ", appName)
}

deleteApp <- function(appId) {
  #' Delete an app
  #'
  #' @param appId id of the app to be deleted, can be obtained with listApps().
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/app/', appId), DELETE)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Deletation of app ", appId, " done - ", respParsed)
    resp$status_code
  } else {
    stop("Deletion of app ", appId, " failed - ", resp$status_code, ":", respParsed)
  }
}

createApp <- function(type, name, tag, route, modelId = NULL, path = NULL) {
  #' Create a new app
  #'
  #' @param type app type (can be "model", "notebook", "app")
  #' @param name app name.
  #' @param tag tag of the app (integer).
  #' @param route app route.
  #' @param modelId id of the model to deploy if you have selected "model" as type.
  #' @param path path of your code if you have selected "notebook" or "app" as type.
  #'
  #' @return parsed content of the app.
  #'
  #' @import httr
  #'
  #' @export

  supportedType = c("model", "notebook", "app")

  if(!(type %in% supportedType)) {
    stop("App type ", type, " is not supported types : ", supportedType)
  }

  if(type == "model" && is.null(modelId)) {
    stop("modelId should be set for model type")
  }

  if(type != "model" && is.null(route)) {
    stop("Route should be set for notebook or app type")
  }

  params <- list(route = route,
                 modelId = modelId,
                 path = path)

  resp <- previsionioRequest(paste0('/app/', type, '/', name, '/', tag), POST, params)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")
  if(resp$status_code == 200) {
    message("Creation of app ", name, " done - ", respParsed)
    getApp(respParsed$`_id`)
  } else {
    stop("Creation of app ", name, " failed - ", resp$status_code, ":", respParsed)
  }
}

updateAppName <- function(appId, appName) {
  #' Update an app name.
  #'
  #' @param appId id of the app to be updated, can be obtained with listApps().
  #' @param appName new name of the app to be set.
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = appName)

  resp <- previsionioRequest(paste0('/app/', appId), PUT, params)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("App id ", appId, " has been renamed to ", appName)
    resp$status_code
  } else {
    stop("Renaming of app ", appId, " failed - ", resp$status_code, ":", respParsed)
  }
}
