pio_client <- new.env(parent = emptyenv())
pio_client$apiVersion <- '/ext/v1'

pio_init <- function(token, url) {
  #' Initialization of the connection to your instance Prevision.io.
  #'
  #' @param token your master token, can be found on your instance on the "API KEY" page.
  #' @param url the url of your instance.
  #'
  #' @return list - url and token needed for connecting to the Prevision.io environment.
  #'
  #' @examples \dontrun{pio_init('eyJhbGciOiJIUz', 'https://xxx.prevision.io')}
  #'
  #' @export

  old <- list(old_url = pio_client$url, old_token = pio_client$token)
  pio_client$url <- url
  pio_client$token <- token
  invisible(old)
}

pio_request <- function(endpoint, method, data = NULL, upload = FALSE) {
  #' Request the platform. Thanks to an endpoint, the url and the API, you can create request.
  #'
  #' @param endpoint end of the url of the API call.
  #' @param method the method needed according the API (Available: POST, GET, DELETE).
  #' @param data object to upload when using method POST.
  #' @param upload used parameter when uploading dataset (for encoding in API call), don't use it.
  #'
  #' @return list - response from the request.
  #'
  #' @examples \dontrun{pio_request(paste0('/jobs/', experiment$jobId), DELETE)}
  #'
  #' @import httr
  #' @import futile.logger
  #' @importFrom jsonlite toJSON
  #'
  #' @export

  if (is.null(pio_client$url) || is.null(pio_client$token)) {
    stop('token or url not set')
  }

  request_url <- paste(pio_client$url, pio_client$apiVersion, endpoint, sep = '')
  flog.debug(paste('requesting:', request_url))
  flog.debug(paste('request body:', data))

  if (upload) {
    flog.debug(paste("request complete body", data))
    resp <- method(
      request_url,
      add_headers("Authorization" = pio_client$token),
      body = data,
      config = config(followlocation = 0L)
    )
  } else {
    flog.debug(paste("request complete body", toJSON(data, auto_unbox = TRUE)))
    resp <- method(
      request_url,
      add_headers("Authorization" = pio_client$token, "Content-Type" = "application/json"),
      body = toJSON(data, auto_unbox = TRUE),
      config = config(followlocation = 0L)
    )
  }
  resp
}

pio_download <- function(endpoint, tempFile) {
  #' Download resources according specific parameters.
  #'
  #' @param endpoint end of the url of the API call.
  #' @param tempFile temporary file to download.
  #'
  #' @return list - response from the request.
  #'
  #' @import httr
  #'
  #' @export

  request_url <- paste(pio_client$url, pio_client$apiVersion, endpoint, sep = '')
  GET(request_url,
      write_disk(tempFile, overwrite = TRUE),
      add_headers(Authorization = pio_client$token),
      config = config(followlocation = 0L))
}

pio_list_to_df <- function(list) {
  #' Convert a list returned from APIs to a dataframe. Only working for consistent list (same naming and number of columns).
  #'
  #' @param list named list comming from an API call.
  #'
  #' @return data.frame - cast a consistent list to a data.frame.
  #'
  #' @export

  if(length(list) == 0) {
    return(data.frame())
  }

  df = data.frame(matrix(unlist(list), nrow = length(list), byrow = TRUE), stringsAsFactors = FALSE)
  names(df) = names(list[[1]])
  df
}
