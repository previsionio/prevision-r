% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployment.R
\name{get_deployment_app_logs}
\alias{get_deployment_app_logs}
\title{Get logs from a deployed app.}
\usage{
get_deployment_app_logs(deployment_id, log_type)
}
\arguments{
\item{deployment_id}{id of the deployment to get the log, can be obtained with get_deployments().}

\item{log_type}{type of logs we want to get among "build", "deploy" or "run".}
}
\value{
list - logs from deployed apps.
}
\description{
Get logs from a deployed app.
}
