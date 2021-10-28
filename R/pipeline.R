test_pipeline_type <- function(type) {
  #' Check if a type of a pipeline is supported
  #'
  #' @param type type of the pipeline among "component", "template", "run".
  #'
  #' @return no return value, called for side effects.
  #'
  #' @import httr
  #'
  #' @export

  if(!type %in% c("component", "template", "run")) {
    stop("type should be either \"component\", \"template\" or \"run\"")
  }
}

get_pipelines <- function(project_id, type) {
  #' Get information of all pipelines of a given type available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param type type of the pipeline to retrieve among "component", "template", or "run".
  #'
  #' @return list - parsed content of all pipelines of the given type for the supplied project_id.
  #'
  #' @import httr
  #'
  #' @export

  test_pipeline_type(type)

  page = 1
  pipelines = c()

  # Looping over page to get all information
  while(T) {
    if(type == "component") {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-components?page=', page), GET)}
    if(type == "template")  {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-templates?page=', page), GET)}
    if(type == "run")       {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-scheduled-runs?page=', page), GET)}
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      pipelines = c(pipelines, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve pipelines list - ", resp$status_code, ":", resp_parsed)
    }
  }
  pipelines
}

get_pipeline_info <- function(pipeline_id, type) {
  #' Get information about a pipeline from its id and its type.
  #'
  #' @param pipeline_id id of the pipeline to be retrieved, can be obtained with get_pipelines().
  #' @param type type of the pipeline to be retrieved among "component", "template", "run".
  #'
  #' @return list - parsed content of the pipeline.
  #'
  #' @import httr
  #'
  #' @export

  test_pipeline_type(type)

  if(type == "component") {resp <- pio_request(paste0('/pipeline-components/', pipeline_id), GET)}
  if(type == "template")  {resp <- pio_request(paste0('/pipeline-templates/', pipeline_id), GET)}
  if(type == "run")       {resp <- pio_request(paste0('/pipeline-scheduled-runs/', pipeline_id), GET)}

  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve pipeline ", pipeline_id, " of type ", type, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_pipeline_id_from_name <- function(project_id, name, type) {
  #' Get a pipeline_id from a pipeline_name and type for a given project_id. If duplicated name, the first pipeline_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param name name of the pipeline we are searching its id from.
  #' @param type type of the pipeline to be retrieved among "component", "template", "run".
  #'
  #' @return character - id of the connector if found.
  #'
  #' @import httr
  #'
  #' @export

  pipeline_list = get_pipelines(project_id, type)
  for (pipineline in pipeline_list) {
    if(type != "component") {
      if(pipineline$name == name) {
        return(pipineline$`_id`)
      }
    }
    if(type == "component") {
      if(pipineline$metadata$name == name) {
        return(pipineline$`_id`)
      }
    }
  }
  stop("there is no pipeline matching the name ", name, " for the type ", type)
}

create_pipeline <- function(project_id, type, name, git_url = NULL, git_branch = NULL, repository_name = NULL, broker = NULL, config_dataset_id = NULL, nodes = NULL, pipeline_template_id = NULL, pipeline_parameters = NULL) {
  #' [BETA] Create a new pipeline of a supported type among "component", "template", "run".
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param type type of the pipeline to be retrieved among "component", "template", "run".
  #' @param name name of the pipeline.
  #' @param git_url url of the git repository than contains the component.
  #' @param git_branch branch of the git repository than contains the component.
  #' @param repository_name name of the git repository that contains the component.
  #' @param broker broker of the git repository that contains the component.
  #' @param config_dataset_id only for templates.
  #' @param nodes list, only for templates.
  #' @param pipeline_template_id id of the pipeline template to add for a run.
  #' @param pipeline_parameters list of parameters for the run.
  #'
  #' @return list - parsed content of the pipeline.
  #'
  #' @import httr
  #'
  #' @export

  test_pipeline_type(type)

  params <- list(name = name,
                 git_branch = git_branch,
                 git_url = git_url,
                 repository_name = repository_name,
                 broker = broker,
                 config_dataset_id = config_dataset_id,
                 nodes = nodes,
                 pipeline_template_id = pipeline_template_id,
                 pipeline_parameters = pipeline_parameters)

  params <- params[!sapply(params, is.null)]

  if(type == "component") {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-components/'), POST, params)}
  if(type == "template")  {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-templates/'), POST, params)}
  if(type == "run")       {resp <- pio_request(paste0('/projects/', project_id, '/pipeline-scheduled-runs/'), POST, params)}

  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")
  if(resp$status_code == 200) {
    message("pipeline ", name, " created")
    get_pipeline_info(resp_parsed$`_id`, type)
  }
  else {
    stop("failed to create pipeline ", name, " - ", resp$status_code, ":", resp_parsed)
  }
}

delete_pipeline <- function(pipeline_id, type) {
  #' Delete an existing pipeline
  #'
  #' @param pipeline_id id of the pipeline to be retrieved, can be obtained with get_pipelines().
  #' @param type type of the pipeline to be retrieved among "component", "template", "run".
  #'
  #' @return integer - 204 on success.
  #'
  #' @import httr
  #'
  #' @export

  test_pipeline_type(type)

  if(type == "component") {resp <- pio_request(paste0('/pipeline-components/', pipeline_id), DELETE)}
  if(type == "template")  {resp <- pio_request(paste0('/pipeline-templates/', pipeline_id), DELETE)}
  if(type == "run")       {resp <- pio_request(paste0('/pipeline-scheduled-runs/', pipeline_id), DELETE)}

  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 204) {
    message("pipeline ", pipeline_id, " of type ", type, " deleted - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  }
  else {
    stop("failed to delete pipeline ", pipeline_id, " of type ", type, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_pipeline_trigger <- function(pipeline_id) {
  #' Trigger an existing pipeline run.
  #'
  #' @param pipeline_id id of the pipeline run to trigger, can be obtained with get_pipelines().
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/pipeline-scheduled-runs/', pipeline_id, '/trigger'), POST, NULL)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("pipeline ", pipeline_id, " triggered - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  }
  else {
    stop("failed to trigger pipeline ", pipeline_id, " - ", resp$status_code, ":", resp_parsed)
  }
}
