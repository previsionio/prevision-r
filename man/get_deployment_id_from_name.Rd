% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deployment.R
\name{get_deployment_id_from_name}
\alias{get_deployment_id_from_name}
\title{Get a deployment_id from a name and type for a given project_id. If duplicated name, the first deployment_id that match it is retrieved.}
\usage{
get_deployment_id_from_name(project_id, name, type)
}
\arguments{
\item{project_id}{id of the project, can be obtained with get_projects().}

\item{name}{name of the deployment we are searching its id from.}

\item{type}{type of the deployment to be retrieved among "model" or "app".}
}
\value{
character - id of the deployment if found.
}
\description{
Get a deployment_id from a name and type for a given project_id. If duplicated name, the first deployment_id that match it is retrieved.
}
