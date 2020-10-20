getProfile <- function() {
  #' Get user profile informations
  #'
  #' @return informations about user (email, company...)
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest('/profile', GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve profile informations - ", resp$status_code, ":", respParsed)
  }
}

getVersion <- function() {
  #' Get Prevision.io version number of the given instance
  #'
  #' @return Prevision.io version number
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest('/version', GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve version number - ", resp$status_code, ":", respParsed)
  }
}
