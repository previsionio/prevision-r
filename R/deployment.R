test_deployment_type <- function(type) {
  #' Check if a type of a deployment is supported
  #'
  #' @param type type of the deployment among "model" or "app".
  #'
  #' @return no return value, called for side effects.
  #'
  #' @import httr
  #'
  #' @export

  if(!type %in% c("model", "app")) {
    stop("type should be either \"model\" or \"app\"")
  }
}

get_deployments <- function(project_id, type) {
  #' Get information of all deployments of a given type available for a given project_id.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param type type of the deployment to retrieve among "model" or "app".
  #'
  #' @return list - parsed content of all deployments of the given type for the supplied project_id.
  #'
  #' @import httr
  #'
  #' @export

  test_deployment_type(type)

  page = 1
  deployments = c()

  # Looping over page to get all information
  while(T) {
    if(type == "model") {resp <- pio_request(paste0('/projects/', project_id, '/model-deployments?page=', page), GET)}
    if(type == "app")   {resp <- pio_request(paste0('/projects/', project_id, '/application-deployments?page=', page), GET)}
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      deployments = c(deployments, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve deployments list - ", resp$status_code, ":", resp_parsed)
    }
  }
  deployments
}

get_deployment_info <- function(deployment_id) {
  #' Get information about a deployment from its id.
  #'
  #' @param deployment_id id of the deployment to be retrieved, can be obtained with get_deployments().
  #'
  #' @return list - parsed content of the deployment.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployments/', deployment_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_id_from_name <- function(project_id, name, type) {
  #' Get a deployment_id from a name and type for a given project_id. If duplicated name, the first deployment_id that match it is retrieved.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param name name of the deployment we are searching its id from.
  #' @param type type of the deployment to be retrieved among "model" or "app".
  #'
  #' @return character - id of the connector if found.
  #'
  #' @import httr
  #'
  #' @export

  deployment_list = get_deployments(project_id, type)
  for (deployment in deployment_list) {
    if(deployment$name == name) {
      return(deployment$`_id`)
    }
  }
  stop("there is no deployment matching the name ", name, " for the type ", type)
}

create_deployment_model <- function(project_id, name, experiment_id, main_model_experiment_version_id, challenger_model_experiment_version_id = NULL, access_type = c("fine_grained", "private", "public"), description = NULL, main_model_id, challenger_model_id = NULL) {
  #' [BETA] Create a new deployment for a model.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param name name of the deployment.
  #' @param experiment_id id of the experiment to deploy, can be obtained with get_experiment_id_from_name().
  #' @param main_model_experiment_version_id id of the experiment_version to deploy, can be obtained with get_experiment_version_id().
  #' @param challenger_model_experiment_version_id id of the challenger experiment_version to deploy, can be obtained with get_experiment_version_id().
  #' @param access_type type of access of the deployment among "fine_grained" (project defined, default), "private" (instance) or "public" (everyone).
  #' @param description description of the deployment.
  #' @param main_model_id id of the model to deploy (will be removed in 11.3.0+)
  #' @param challenger_model_id id of the challenger model to deploy (will be removed in 11.3.0+)
  #'
  #' @return list - parsed content of the deployment.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = name,
                 experiment_id = experiment_id,
                 main_model_experiment_version_id = main_model_experiment_version_id,
                 challenger_model_experiment_version_id = challenger_model_experiment_version_id,
                 access_type = access_type,
                 description = description,
                 main_model_id = main_model_id,
                 challenger_model_id = challenger_model_id)

  params <- params[!sapply(params, is.null)]

  resp <- pio_request(paste0('/projects/', project_id, '/model-deployments/'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("creation of deployment ", name, " done - ", resp$status_code, ":", resp_parsed)
    get_deployment_info(resp_parsed$`_id`)
  }
  else {
    stop("failed to create deployment ", name, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_deployment_app <- function(project_id, name, git_url, git_branch, type, broker, app_cpu = 1, app_ram = "128Mi", app_replica_count = 1, env_vars = list(), access_type = "fine_grained", description = NULL) {
  #' [BETA] Create a new deployment for an application.
  #'
  #' @param project_id id of the project, can be obtained with get_projects().
  #' @param name name of the deployment.
  #' @param git_url url of the git repository than contains the app to be deployed.
  #' @param git_branch branch of the git repository than contains the app to be deployed.
  #' @param type type of language in which the app is written among "r", "python" or "node".
  #' @param broker broker of the git repository (gitlab, github) that contains the application.
  #' @param app_cpu number of CPU that is allocated for the application deployment (1 default, 2 or 4)
  #' @param app_ram quantity of RAM that is allocated for the application deployment (128Mi default, 256Mi, 512Mi, 1Gi, 2Gi, 4Gi or 8Gi)
  #' @param app_replica_count number of replica allocated for the application deployment (1 default, 2, 3, 4, 5, 6, 7, 8, 9 or 10)
  #' @param env_vars list of environment variables (optional).
  #' @param access_type type of access of the deployment among "fine_grained" (project defined, default), "private" (instance) or "public" (everyone).
  #' @param description description of the deployment (optional).
  #'
  #' @return list - parsed content of the deployment.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(name = name,
                 git_url = git_url,
                 git_branch = git_branch,
                 type = type,
                 broker = broker,
                 app_cpu = app_cpu,
                 app_ram = app_ram,
                 app_replica_count = app_replica_count,
                 env_vars = env_vars,
                 access_type = access_type,
                 description = description)

  resp <- pio_request(paste0('/projects/', project_id, '/application-deployments/'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("deployment ", name, " created")
    get_deployment_info(resp_parsed$`_id`)
  }
  else {
    stop("failed to create deployment ", name, " - ", resp$status_code, ":", resp_parsed)
  }
}

delete_deployment <- function(deployment_id) {
  #' Delete an existing deployment
  #'
  #' @param deployment_id id of the deployment, can be obtained with get_deployments().
  #'
  #' @return integer - 204 on success.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployments/', deployment_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 204) {
    message("deployment ", deployment_id, " deleted")
    resp$status_code
  }
  else {
    stop("failed to delete deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_app_logs <- function(deployment_id, log_type) {
  #' Get logs from a deployed app.
  #'
  #' @param deployment_id id of the deployment to get the log, can be obtained with get_deployments().
  #' @param log_type type of logs we want to get among "build", "deploy" or "run".
  #'
  #' @return list - logs from deployed apps.
  #'
  #' @import httr
  #' @import XML
  #'
  #' @export

  if(!log_type %in% c("build", "deploy", "run")) {
    stop("log_type should be either \"build\", \"deploy\" or \"run\"")
  }

  resp <- pio_request(paste0('/deployments/', deployment_id, '/logs/', log_type), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    html = htmlTreeParse(resp_parsed, useInternalNodes = TRUE)
    html_cleaned = xpathApply(html, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", xmlValue)
    #cat(unlist(html_cleaned))
    paste(unlist(html_cleaned), collapse="\n")
  }
  else {
    stop("can't retrieve ", log_type, " logs for deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_api_keys <- function(deployment_id) {
  #' Get API keys for a deployed model.
  #'
  #' @param deployment_id id of the deployment to get API keys, can be obtained with get_deployments().
  #'
  #' @return data.frame - API keys available for deployment_id.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployments/', deployment_id, '/api-keys/'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    pio_list_to_df(resp_parsed[["items"]])
  }
  else {
    stop("can't retrieve API keys for deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_deployment_api_key <- function(deployment_id) {
  #' Create a new API key for a deployed model.
  #'
  #' @param deployment_id id of the deployment to create an API key on, can be obtained with get_deployments().
  #'
  #' @return list - API key information.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployments/', deployment_id, '/api-keys/'), POST)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("api key created for deployment ", deployment_id)
    get_deployment_api_keys(deployment_id)
  }
  else {
    stop("failed to create api key for deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_predictions <- function(deployment_id) {
  #' Get listing of predictions related to a deployment_id.
  #'
  #' @param deployment_id id of the deployment, can be obtained with get_deployments().
  #'
  #' @return list - predictions available for a deployed model.
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  predictions = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/deployments/', deployment_id, '/deployment-predictions?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      predictions = c(predictions, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve predictions list - ", resp$status_code, ":", resp_parsed)
    }
  }
  predictions
}

get_deployment_prediction_info <- function(prediction_id) {
  #' Get information related to predictions of a prediction_id.
  #'
  #' @param prediction_id id of the prediction returned by create_deployment_predictions or that can be obtained with get_deployment_predictions().
  #'
  #' @return list - prediction information for a deployed model.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployment-predictions/', prediction_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve predictions for prediction_id ", prediction_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

create_deployment_predictions <- function(deployment_id, dataset_id) {
  #' Create predictions on a deployed model using a dataset.
  #'
  #' @param deployment_id id of the deployment, can be obtained with get_deployments().
  #' @param dataset_id id of the dataset to predict, can be obtained with get_dataset_id_from_name().
  #'
  #' @return integer - 200 on success.
  #'
  #' @import httr
  #'
  #' @export

  params <- list(dataset_id = dataset_id)

  resp <- pio_request(paste0('/deployments/', deployment_id, '/deployment-predictions'), POST, params)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("deployment prediction started")
    resp_parsed$`_id`
  }
  else {
    stop("failed to create prediction for deployment ", deployment_id, " and dataset ", dataset_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_usage <- function(deployment_id, usage_type) {
  #' Get usage (calls, errors and response time) of the last version of a deployed model.
  #'
  #' @param deployment_id id of the deployment to get usage, can be obtained with get_deployments().
  #' @param usage_type type of usage to get, among "calls", "errors", "response_time".
  #'
  #' @return list - plotly object.
  #'
  #' @import httr
  #' @importFrom magrittr %>%
  #' @importFrom plotly plot_ly
  #' @importFrom plotly add_trace
  #' @importFrom plotly layout
  #'
  #' @export

  if(!usage_type %in% c("calls", "errors", "response_time")) {
    stop("usage_type should be among \"calls\", \"errors\", \"response_time\"")
  }

  resp <- pio_request(paste0('/deployments/', deployment_id, '/usage/'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    # Stop if no monitoring data is available
    if(length(resp_parsed$usage_chart[[usage_type]]) == 0) {
      stop("there is no ", usage_type, " data available for the current version of deployment ", deployment_id)
    }

    # Get the "main" data an prepare the chart
    df = pio_list_to_df(resp_parsed$usage_chart[[usage_type]]$main)
    df$x = as.Date(df$x)
    df$y = as.numeric(df$y)
    names(df) = c("date", usage_type)

    to_plot = plot_ly(df, x = ~date, y = ~get(usage_type), name = "main", type = "scatter", mode = "lines+markers") %>% layout(title = paste(usage_type, "~ date for version", resp_parsed$meta_data$versions, "of deployment", get_deployment_info(deployment_id)$name),
                                                                                                                               xaxis = list(title = "date"),
                                                                                                                               yaxis = list (title = usage_type))

    # If challenger is missing
    if(length(resp_parsed$usage_chart[[usage_type]]$challenger) < 1) {
      to_plot
    }
    # If challenger is present
    else {
      df_chall = pio_list_to_df(resp_parsed$usage_chart[[usage_type]]$challenger)
      df_chall$x = as.Date(df_chall$x)
      df_chall$y = as.numeric(df_chall$y)
      names(df_chall) = c("date", usage_type)
      to_plot %>% add_trace(y = ~get(usage_type), name = "challenger", mode = "lines+markers")
    }
  }
  else {
    stop("can't retrieve usage type ", usage_type, " for deployment ", deployment_id, " - ", resp$status_code, ":", resp_parsed)
  }
}
