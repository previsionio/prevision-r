% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline.R
\name{get_pipeline_info}
\alias{get_pipeline_info}
\title{Get information about a pipeline from its id and its type.}
\usage{
get_pipeline_info(pipeline_id, type)
}
\arguments{
\item{pipeline_id}{id of the pipeline to be retrieved, can be obtained with get_pipelines().}

\item{type}{type of the pipeline to be retrieved among "component", "template", "run".}
}
\value{
list - parsed content of the pipeline.
}
\description{
Get information about a pipeline from its id and its type.
}
