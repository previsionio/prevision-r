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
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      projects = c(projects, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve project list - ", resp$status_code, ":", resp_parsed)
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
      return(project$project_id)
    }
  }
  stop("There is no project_id matching the project_name ", project_name)
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

  resp <- pio_request(paste0('/project/', project_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve information from project ", project_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_project <- function(project_name) {
  #' Create a new project.
  #'
  #' @param project_name name of the project.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = project_name)

  resp <- pio_request('/projects/', POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    get_project_info(resp_parsed$`_id`)
  } else {
    stop("Project creation failed - ", resp_parsed$status, ":", resp_parsed$message)
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

  resp <- pio_request(paste0('/project/', dataset_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
    resp$status_code
  } else {
    stop("Delete KO - ", resp$status_code, ":", resp_parsed$message)
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

  page = 1
  project_users = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/users?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      project_users = c(project_users, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve project users' list - ", resp$status_code, ":", resp_parsed)
    }
  }
  project_users
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

  if(user_role %in% c("admin", "contributor", "viewer")) {
    stop("user_role must be either \"admin\", \"contributor\" or \"viewer\"")
  }

  params <- list(name = project_name, projectRole = user_role)

  resp <- pio_request(paste0('/projects/', project_id, '/users'), POST, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("User ", user_mail, " added with the role", user_role, " to the project ", project_id)
    get_project_users(resp_parsed$`_id`)
  } else {
    stop("User wasn't added to the project - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

update_project_user_role <- function(project_id, user_mail, user_role) {
  #' Update user role in and existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param user_mail email of the user to be edit, can be obtained with get_users().
  #' @param user_role role to grand to the user among "admin", "contributor" and "viewer"
  #'
  #' @return list of users in the project
  #'
  #' @import httr
  #'
  #' @export

  if(user_role %in% c("admin", "contributor", "viewer")) {
    stop("user_role must be either \"admin\", \"contributor\" or \"viewer\"")
  }

  params <- list(projectRole = user_role)

  resp <- pio_request(paste0('/projects/', project_id, '/users/', user_id), PUT, params)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("User ", user_mail, " updated with the role", user_role, " to the project ", project_id)
    get_project_users(resp_parsed$`_id`)
  } else {
    stop("User role hasn't been updated - ", resp_parsed$status, ":", resp_parsed$message)
  }
}

delete_project_user <- function(project_id, user_mail) {
  #' Delete user in and existing project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param user_mail email of the user to be delete, can be obtained with get_users().
  #'
  #' @return 200 on success
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/projects/', project_id, '/users/', user_id), DELETE)
  resp_parsed <- content(resp, 'parsed')

  if(resp$status_code == 200) {
    message("Delete OK - ", resp$status_code, ":", resp_parsed$message)
    resp$status_code
  } else {
    stop("Delete KO - ", resp$status_code, ":", resp_parsed$message)
  }
}

get_project_datasets <- function(project_id) {
  #' Get datasets from a project.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #'
  #' @return parsed content of the project's datasets
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  project_datasets = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/projects/', project_id, '/datasets?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Stop when no new entry appears
      if(length(resp_parsed[["items"]])==0) {
        break
      }

      # Store items and continue
      project_datasets = c(project_datasets, resp_parsed[["items"]])
      page = page + 1
    }
    else {
      stop("Can't retrieve project datasets' list - ", resp$status_code, ":", resp_parsed)
    }
  }
  project_datasets
}
