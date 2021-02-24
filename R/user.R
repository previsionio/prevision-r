get_version <- function() {
  #' Get Prevision.io version number of the given instance
  #'
  #' @return Prevision.io version number
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request('/version', GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve version number - ", resp$status_code, ":", resp_parsed)
  }
}

get_users <- function() {
  #' Get Prevision.io users of the given instance
  #'
  #' @return Prevision.io user list
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request('/users', GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve user list - ", resp$status_code, ":", resp_parsed)
  }
}

get_users_usage <- function() {
  #' Get Prevision.io users' usage of the given instance
  #'
  #' @return Prevision.io users usage
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request('/users/usages', GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve users' usage list - ", resp$status_code, ":", resp_parsed)
  }
}
