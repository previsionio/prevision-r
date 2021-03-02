% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/usecase.R
\name{delete_prediction}
\alias{delete_prediction}
\title{Delete a prediction of a given model and its version number.}
\usage{
delete_prediction(usecase_id, prediction_id, version_number = 1)
}
\arguments{
\item{usecase_id}{id of the usecase, can be obtained with get_usecases().}

\item{prediction_id}{id of the prediction to be retrieved, can be obtained with usecasePredictions().}

\item{version_number}{number of the version of the usecase. 1 by default.}
}
\value{
list of predictions of usecase_id.
}
\description{
Delete a prediction of a given model and its version number.
}