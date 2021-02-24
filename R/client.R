pio_client <- new.env(parent = emptyenv())
pio_client$apiVersion <- '/ext/v1'

pio_init <- function(token, url) {
  #' Initialization of the connection to your instance Prevision.io.
  #'
  #' @param token your master token, can be found on your instance on the "API KEY" page.
  #' @param url the url of your instance.
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
  #' @examples \dontrun{pio_request(paste0('/jobs/', usecase$jobId), DELETE)}
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
  flog.debug(paste("request complete body", toJSON(data, auto_unbox=TRUE)))

  if (upload) {
    resp <- method(
      request_url,
      add_headers(Authorization = pio_client$token),
      body = data,
      config = config(followlocation = 0L)
    )
  } else {
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
  #' @return write on disk the content of the temporary file.
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
