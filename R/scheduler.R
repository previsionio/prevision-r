getScheduler <- function() {
  #' Retrieves all scheduler tasks
  #'
  #' @return a scheduler tasks list.
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest('/schedulers', GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't retrieve scheduler tasks list - ", resp$status_code, ":", respParsed)
  }
}

getSchedulerIdFromName <- function(schedulerName) {
  #' Get an schedulerId from an schedulerName If duplicated name, the first schedulerId that match it is retrieved
  #'
  #' @param schedulerName name of the scheduler task we are searching its id from. Can be obtained with listScheduler().
  #'
  #' @return id of the scheduler task if found.
  #'
  #' @import httr
  #'
  #' @export

  schedulerList = getScheduler()
  for (scheduler in schedulerList) {
    if(scheduler$name == schedulerName) {
      return(scheduler$`_id`)
    }
  }
  stop("There is no schedulerId matching the schedulerName ", schedulerName)
}

startScheduler <- function(schedulerId) {
  #' Start a scheduled task
  #'
  #' @param schedulerId id of the scheduler task to be run. Can be obtained with listScheduler().
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/schedulers/', schedulerId, '/run'), GET)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    respParsed
  }
  else {
    stop("Can't run scheduler - ", resp$status_code, ":", respParsed)
  }
}

deleteScheduler <- function(schedulerId) {
  #' Delete a scheduler task
  #'
  #' @param schedulerId id of the scheduler task to be deleted, can be obtained with listScheduler().
  #'
  #' @return response status code (200 on success)
  #'
  #' @import httr
  #'
  #' @export

  resp <- previsionioRequest(paste0('/schedulers/', schedulerId), DELETE)
  respParsed <- content(resp, 'parsed', encoding = "UTF-8")

  if(resp$status_code == 200) {
    message("Delete of task ", schedulerId, " OK - ", resp$status_code, ":", respParsed)
    resp$status_code
  } else {
    stop("Delete of task ", schedulerId, " KO - ", resp$status_code, ":", respParsed)
  }
}
