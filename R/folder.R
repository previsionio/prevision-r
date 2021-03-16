get_folders <- function() {
  #' Get information of all folders availables.
  #'
  #' @return parsed content of all folders.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  folders = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/datasets/folders?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      folders = c(folders, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve folders list - ", resp$status_code, ":", resp_parsed)
    }
  }
  folders
}

get_folder_id_from_name <- function(folder_name) {
  #' Get a folder_id from a folder_name. If duplicated name, the first folder_id that match it is retrieved.
  #'
  #' @param folder_name name of the folder we are searching its id from. Can be obtained with get_folders().
  #'
  #' @return id of the folder if found.
  #'
  #' @import httr
  #'
  #' @export

  folder_list = get_folders()
  for (folder in folder_list) {
    if(folder$name == folder_name) {
      return(folder$`_id`)
    }
  }
  stop("There is no folder_id matching the folder_name ", folder_name)
}

get_folder <- function(folder_id) {
  #' Get a folder from its id.
  #'
  #' @param folder_id id of the image folder, can be obtained with get_folders().
  #'
  #' @return parsed content of the folder.
  #'
  #' @import httr
  #'
  #' @export

  while (T) {
    resp <- pio_request(paste0('/datasets/folders/', folder_id), GET)
    if(resp$status_code == 200) {
      resp_parsed <- content(resp, 'parsed')
      if(resp_parsed$ready == "done") {
        break
      }
      message('Waiting for dataset')
      Sys.sleep(5)
    }
  }
  resp_parsed
}

create_folder <- function(folder_name, file) {
  #' Upload folder from a local file.
  #'
  #' @param folder_name given name of the folder on the platform.
  #' @param file path to the folder.
  #'
  #' @return parsed content of the folder.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = folder_name, file = upload_file(file))

  resp <- pio_request('/datasets/folders', POST, params, upload = TRUE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    get_folder(resp_parsed$`_id`)
  } else {
    stop("Folder upload failed - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

delete_folder <- function(folder_id) {
  #' Delete an existing folder.
  #'
  #' @param folder_id id of the folder to be deleted.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/datasets/folders/', folder_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
    resp$status_code
  } else {
    stop("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
}
