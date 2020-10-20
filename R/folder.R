getFolders <- function() {
  #' Get informations of all folders availables.
  #'
  #' @return parsed content of all folders.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  folders = c()

  # Looping over page to get all informations
  while(T) {
    resp <- previsionioRequest(paste0('/datasets/files?page=', page), GET)
    respParsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(respParsed[["items"]])==0) {
        break
      }

      # Store items and continue
      folders = c(folders, respParsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve folders list - ", resp$status_code, ":", respParsed)
    }
  }
  folders
}

getFolder <- function(folder_id) {
  #' Get a folder from its id.
  #'
  #' @param folder_id id of the image folder, can be obtained with getFolders().
  #'
  #' @return parsed content of the dataset.
  #'
  #' @import httr
  #'
  #' @export

  while (T) {
    resp <- previsionioRequest(paste0('/datasets/folders/', folder_id), GET)
    if(resp$status_code == 200) {
      respParsed <- content(resp, 'parsed')
      if(respParsed$ready == "done") {
        break
      }
      message('Waiting for dataset')
      Sys.sleep(5)
    }
  }
  respParsed
}

createFolder <- function(folder_name, local_path) {
  #' Upload folder from a local file.
  #'
  #' @param folder_name given name of the folder on the platform.
  #' @param local_path path to the folder.
  #'
  #' @return parsed content of the folder.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = dataset_name,
                 file = upload_file(local_path))

  resp <- previsionioRequest('/datasets/folders', POST, params, upload = TRUE)
  respParsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    getFolder(respParsed$`_id`)
  } else {
    stop("Folder upload failure - ", respParsed$status, ":", respParsed$message)
  }
}
