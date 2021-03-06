% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{create_dataframe_from_dataset}
\alias{create_dataframe_from_dataset}
\title{Create a dataframe from a dataset_id.}
\usage{
create_dataframe_from_dataset(dataset_id, path = getwd(), is_folder = FALSE)
}
\arguments{
\item{dataset_id}{dataset id.}

\item{path}{path (without / at the end) were to write the downloaded dataset.}

\item{is_folder}{TRUE if it's a folder dataset, FALSE (by default) otherwise.}
}
\value{
a R dataframe.
}
\description{
Create a dataframe from a dataset_id.
}
