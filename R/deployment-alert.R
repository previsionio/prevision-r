get_deployment_alerts <- function(deployment_id) {
  #' Get information of all alerts related to a deployment_id.
  #'
  #' @param deployment_id id of the project, can be obtained with get_deployments().
  #'
  #' @return list - parsed content of all alerts for the supplied deployment_id
  #'
  #' @import httr
  #'
  #' @export

  page = 1
  alerts = c()

  # Looping over page to get all information
  while(T) {
    resp <- pio_request(paste0('/deployments/', deployment_id, '/alerts?page=', page), GET)
    resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

    if(resp$status_code == 200) {
      # Store information
      alerts = c(alerts, resp_parsed[["items"]])
      page = page + 1

      # Stop if next page == FALSE
      if(resp_parsed[["metaData"]]$nextPage==FALSE) {
        break
      }
    }
    else {
      stop("can't retrieve alerts list - ", resp$status_code, ":", resp_parsed)
    }
  }
  alerts
}

get_deployment_alert_info <- function(deployment_id, deployment_alert_id) {
  #' Get information about a deployment_alert for a given deployed model.
  #'
  #' @param deployment_id id of the deployment, can be obtained with get_deployments().
  #' @param deployment_alert_id id of the deployment_alert to be retrieved, can be obtained with get_deployment_alerts().
  #'
  #' @return list - parsed content of the deployment_alert.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/deployments/', deployment_id, '/alerts/', deployment_alert_id), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("can't retrieve deployment_alert ", deployment_alert_id, " - ", resp$status_code, ":", resp_parsed)
  }
}

get_deployment_alert_id_from_name <- function(deployment_id, name) {
  #' Get a deployment_alert_id from a name and type for a given deployment_id.
  #'
  #' @param deployment_id id of the deployment, can be obtained with get_deployments().
  #' @param name name of the deployment_alert we are searching its id from.
  #'
  #' @return character - id of the deployment_alert if found.
  #'
  #' @import httr
  #'
  #' @export

  deployment_alert_list = get_deployment_alerts(deployment_id)
  for (deployment_alert in deployment_alert_list) {
    if(deployment_alert$name == name) {
      return(deployment_alert$`_id`)
    }
  }
  stop("there is no deployment_alert matching the name ", name, " for the deployment_id ", deployment_id)
}
