previsionioClient <- new.env(parent = emptyenv())
previsionioClient$apiVersion <- '/ext/v1'

initPrevisionioClient <- function(token, url) {
  #' Initialization of the Client on the platform.
  #'
  #' Thanks to the token and the url, you can create a Prevision.io Client.
  #'
  #' @param token String, your master token available on the platform.
  #' @param url String, the url of your access page to the platform.
  #'
  #' @examples \dontrun{initPrevisionioClient(token, 'https://xxx.prevision.io')}
  #'
  #' @export

  old <- list(old_url = previsionioClient$url, old_token = previsionioClient$token)
  previsionioClient$url <- url
  previsionioClient$token <- token
  invisible(old)
}

previsionioRequest <- function(endpoint, method, data = NULL, upload = FALSE) {
  #' Request the platform
  #'
  #' Thanks to an endpoint, the url and the API, you can create request.
  #'
  #' @param endpoint string, end of the url of the API call.
  #' @param method string, the method needed according the API (Available: POST, GET, DELETE).
  #' @param data, oject to upload when using method POST.
  #' @param upload, used parameter when uploading dataset (for encoding in API call), don't use it.
  #'
  #' @examples \dontrun{previsionioRequest(paste0('/jobs/', usecase$jobId), DELETE)}
  #'
  #' @import httr
  #' @import futile.logger
  #' @importFrom jsonlite toJSON
  #'
  #' @export

  if (is.null(previsionioClient$url) || is.null(previsionioClient$token)) {
    stop('token or url not set')
  }

  requestUrl <- paste(previsionioClient$url, previsionioClient$apiVersion, endpoint, sep = '')
  flog.debug(paste('requesting:', requestUrl))
  flog.debug(paste('request body:', data))
  flog.debug(paste("request complete body", toJSON(data, auto_unbox=TRUE)))

  if (upload) {
    resp <- method(
      requestUrl,
      add_headers(Authorization = previsionioClient$token),
      body = data,
      config = config(followlocation = 0L)
    )
  } else {
    resp <- method(
      requestUrl,
      add_headers("Authorization" = previsionioClient$token, "Content-Type" = "application/json"),
      body = toJSON(data, auto_unbox=TRUE),
      config = config(followlocation = 0L)
    )
  }
  resp
}

previsionDownload <- function(endpoint, tempFile) {
  #'  Download according specific parameters
  #'
  #' @param endpoint string, end of the url of the API call.
  #' @param tempFile file, tempory file to be download.
  #'
  #' @return write on disk the content of the temporary file.
  #'
  #' @import httr
  #'
  #' @export

  requestUrl <- paste(previsionioClient$url, previsionioClient$apiVersion, endpoint, sep = '')
  GET(requestUrl,
      write_disk(tempFile, overwrite = TRUE),
      add_headers(Authorization = previsionioClient$token),
      config = config(followlocation = 0L))
}
