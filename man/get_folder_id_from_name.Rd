% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/folder.R
\name{get_folder_id_from_name}
\alias{get_folder_id_from_name}
\title{Get a folder_id from a folder_name. If duplicated name, the first folder_id that match it is retrieved.}
\usage{
get_folder_id_from_name(project_id, folder_name)
}
\arguments{
\item{project_id}{id of the project, can be obtained with get_projects().}

\item{folder_name}{name of the folder we are searching its id from. Can be obtained with get_folders().}
}
\value{
character - id of the folder if found.
}
\description{
Get a folder_id from a folder_name. If duplicated name, the first folder_id that match it is retrieved.
}
