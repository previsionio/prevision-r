get_scheduler <- function() {
  #' Retrieves all scheduler tasks
  #'
  #' @return a scheduler tasks list.
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request('/schedulers', GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't retrieve scheduler tasks list - ", resp$status_code, ":", resp_parsed)
  }
}

get_scheduler_id_from_name <- function(scheduler_name) {
  #' Get an scheduler_id from an scheduler_name If duplicated name, the first scheduler_id that match it is retrieved.
  #'
  #' @param scheduler_name name of the scheduler task we are searching its id from. Can be obtained with get_scheduler().
  #'
  #' @return id of the scheduler task if found.
  #'
  #' @import httr
  #'
  #' @export

  schedulerList = get_scheduler()
  for (scheduler in schedulerList) {
    if(scheduler$name == scheduler_name) {
      return(scheduler$`_id`)
    }
  }
  stop("There is no scheduler_id matching the scheduler_name ", scheduler_name)
}

run_scheduler <- function(scheduler_id) {
  #' Run a scheduled task.
  #'
  #' @param scheduler_id id of the scheduler task to be run. Can be obtained with get_scheduler().
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/schedulers/', scheduler_id, '/run'), GET)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    resp_parsed
  }
  else {
    stop("Can't run scheduler - ", resp$status_code, ":", resp_parsed)
  }
}

delete_scheduler <- function(scheduler_id) {
  #' Delete a scheduler task.
  #'
  #' @param scheduler_id id of the scheduler task to be deleted, can be obtained with get_scheduler().
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  resp <- pio_request(paste0('/schedulers/', scheduler_id), DELETE)
  resp_parsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Delete of task ", scheduler_id, " OK - ", resp$status_code, ":", resp_parsed)
    resp$status_code
  } else {
    stop("Delete of task ", scheduler_id, " KO - ", resp$status_code, ":", resp_parsed)
  }
}
