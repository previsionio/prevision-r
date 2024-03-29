% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\name{get_experiment_version_id}
\alias{get_experiment_version_id}
\title{Get a experiment version id from a experiment_id and its version number.}
\usage{
get_experiment_version_id(experiment_id, version_number = 1)
}
\arguments{
\item{experiment_id}{id of the experiment, can be obtained with get_experiments().}

\item{version_number}{number of the version of the experiment. 1 by default}
}
\value{
character - experiment version id.
}
\description{
Get a experiment version id from a experiment_id and its version number.
}
