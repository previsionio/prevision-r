% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\name{get_model_feature_importance}
\alias{get_model_feature_importance}
\title{Get feature importance corresponding to a model_id.}
\usage{
get_model_feature_importance(model_id, mode = "raw")
}
\arguments{
\item{model_id}{id of the model, can be obtained with get_experiment_models().}

\item{mode}{character indicating the type of feature importance among "raw" (default) or "engineered".}
}
\value{
data.frame - dataset of the model's feature importance.
}
\description{
Get feature importance corresponding to a model_id.
}
