get_folders <- function(project_id) {
  #' Get information of all image folders available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return list - parsed content of all folders.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  folders = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/image-folders?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      folders = c(folders, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve folders list - ", resp$status_code, ":", resp_parsed)
    }
  }
  folders
}

get_folder_id_from_name <- function(project_id, folder_name) {
  #' Get a folder_id from a folder_name. If duplicated name, the first folder_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param folder_name name of the folder we are searching its id from. Can be obtained with get_folders().
  #'
  #' @return character - id of the folder if found.
  #'
  #' @import httr
  #'
  #' @export

  folder_list = get_folders(project_id)
  for (folder in folder_list) {
    if(folder$name == folder_name) {
      return(folder$`_id`)
    }
  }
  stop("there is no folder_id matching the folder_name ", folder_name)
}

get_folder <- function(folder_id) {
  #' Get a folder from its id.
  #'
  #' @param folder_id id of the image folder, can be obtained with get_folders().
  #'
  #' @return list - parsed content of the folder.
  #'
  #' @import httr
  #'
  #' @export

  while (T) {
    resp <- pio_request(paste0('/image-folders/', folder_id), GET)
    if(resp$status_code == 200) {
      resp_parsed <- content(resp, 'parsed')
      if(resp_parsed$state == "done") {
        break
      }
      message('waiting for image folder')
      Sys.sleep(5)
    }
  }
  resp_parsed
}

create_folder <- function(project_id, folder_name, file) {
  #' Upload folder from a local file.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param folder_name given name of the folder on the platform.
  #' @param file path to the folder.
  #'
  #' @return list - parsed content of the folder.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = folder_name, file = upload_file(file))

  resp <- pio_request(paste0('/projects/', project_id, '/image-folders'), POST, params, upload = TRUE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("upload of folder ", folder_name, "done")
    resp_parsed
  } else {
    stop("failed to upload folder ", folder_name, " - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

delete_folder <- function(folder_id) {
  #' Delete an existing folder.
  #'
  #' @param folder_id id of the folder to be deleted.
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/image-folders/', folder_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("folder ", folder_id, " deleted with success")
    resp$status_code
  } else {
    stop("failed to delete folder ", folder_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
}
