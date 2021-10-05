get_projects <- function() {
  #' Retrieves all projects.
  #'
  #' @return a project list.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  projects = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      projects = c(projects, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve project list - ", resp$status_code, ":", resp_parsed)
    }
  }
  projects
}

get_project_id_from_name <- function(project_name) {
  #' Get a project_id from a project_name If duplicated name, the first project_id that match it is retrieved.
  #'
  #' @param project_name name of the project we are searching its id from. Can be obtained with get_projects().
  #'
  #' @return project_id of the project_name if found.
  #'
  #' @import httr
  #'
  #' @export

  project_list = get_projects()
  for (project in project_list) {
    if(project$name == project_name) {
      return(project$`_id`)
    }
  }
  stop("there is no project_id matching the project_name ", project_name)
}

get_project_info <- function(project_id) {
  #' Get a project from its project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return parsed content of the project
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/projects/', project_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve information from project ", project_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_project <- function(name, description = NULL, color = "#a748f5") {
  #' Create a new project.
  #'
  #' @param name name of the project.
  #' @param description description of the project.
  #' @param color color of the project (#a748f5 by default).
  #'
  #' @import httr
  #'
  #' @export

  if(!color %in% c("#4876be", "#4ab6eb", "#49cf7d", "#dc8218", "#ecba35", "#f45b69", "#a748f5", "#b34ca2", "#2fe6d0")) {
    stop("color should be either #4876be, #4ab6eb, #49cf7d, #dc8218, #ecba35, #f45b69, #a748f5, #b34ca2 or #2fe6d0")
  }

  params <- list(name = name,
                 description = description,
                 color = color)

  params <- params[!sapply(params, is.null)]

  resp <- pio_request('/projects/', POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("project ", project_id, " created with sucess")
    get_project_info(resp_parsed$`_id`)
  } else {
    stop("project creation failed - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

delete_project <- function(project_id) {
  #' Delete an existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return 200 on success
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/projects/', project_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 204) {
    message("project ", project_id, " deleted")
    resp$status_code
  } else {
    stop("failed to delete project ", project_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
}

get_project_users <- function(project_id) {
  #' Get users from a project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return parsed content of the project's users
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/projects/', project_id, '/users'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve project users' list - ", resp$status_code, ":", resp_parsed)
  }
}

create_project_user <- function(project_id, user_mail, user_role) {
  #' Add user in and existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param user_mail email of the user to be add, can be obtained with get_users().
  #' @param user_role role to grand to the user among "admin", "contributor" and "viewer"
  #'
  #' @return list of users in the project
  #'
  #' @import httr
  #'
  #' @export

  if(!user_role %in% c("admin", "contributor", "viewer")) {
    stop("user_role must be either \"admin\", \"contributor\" or \"viewer\"")
  }

  params <- list(email = user_mail, projectRole = user_role)

  resp <- pio_request(paste0('/projects/', project_id, '/users'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("user ", user_mail, " added with the role ", user_role, " to the project ", project_id)
    get_project_users(resp_parsed$`_id`)
  } else {
    stop("user ", user_mail, " wasn't added to the project - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

update_project_user_role <- function(project_id, user_id, user_role) {
  #' Update user role in and existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param user_id user_id of the user to be delete, can be obtained with get_project_users().
  #' @param user_role role to grand to the user among "admin", "contributor" and "viewer"
  #'
  #' @return list of users in the project
  #'
  #' @import httr
  #'
  #' @export

  if(!user_role %in% c("admin", "contributor", "viewer")) {
    stop("user_role must be either \"admin\", \"contributor\" or \"viewer\"")
  }

  params <- list(projectRole = user_role)

  resp <- pio_request(paste0('/projects/', project_id, '/users/', user_id), PUT, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("user updated with the role", user_role, " to the project ", project_id)
    get_project_users(resp_parsed$`_id`)
  } else {
    stop("user role hasn't been updated - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

delete_project_user <- function(project_id, user_id) {
  #' Delete user in and existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param user_id user_id of the user to be delete, can be obtained with get_project_users().
  #'
  #' @return 200 on success
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/projects/', project_id, '/users/', user_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("user ", user_id, " deleted from project ", project_id)
    resp$status_code
  } else {
    stop("failed to delete user ", user_id, " from project ", project_id, " - ", resp$status_code, ":", resp_parsed$message)
  }
}
