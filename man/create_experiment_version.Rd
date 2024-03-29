% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\name{create_experiment_version}
\alias{create_experiment_version}
\title{Create a new version of an existing experiment.}
\usage{
create_experiment_version(
  experiment_id,
  dataset_id = NULL,
  target_column = NULL,
  holdout_dataset_id = NULL,
  id_column = NULL,
  drop_list = NULL,
  profile = NULL,
  experiment_description = NULL,
  metric = NULL,
  fold_column = NULL,
  normal_models = NULL,
  lite_models = NULL,
  simple_models = NULL,
  with_blend = NULL,
  weight_column = NULL,
  features_engineering_selected_list = NULL,
  features_selection_count = NULL,
  features_selection_time = NULL,
  folder_dataset_id = NULL,
  filename_column = NULL,
  ymin = NULL,
  ymax = NULL,
  xmin = NULL,
  xmax = NULL,
  time_column = NULL,
  start_dw = NULL,
  end_dw = NULL,
  start_fw = NULL,
  end_fw = NULL,
  group_list = NULL,
  apriori_list = NULL,
  content_column = NULL,
  queries_dataset_id = NULL,
  queries_dataset_content_column = NULL,
  queries_dataset_id_column = NULL,
  queries_dataset_matching_id_description_column = NULL,
  top_k = NULL,
  lang = NULL,
  models_params = NULL,
  name = NULL,
  onnx_file = NULL,
  yaml_file = NULL
)
}
\arguments{
\item{experiment_id}{id of the experiment that will host the new version.}

\item{dataset_id}{id of the dataset used for the training phase.}

\item{target_column}{name of the TARGET column.}

\item{holdout_dataset_id}{id of the holdout dataset.}

\item{id_column}{name of the id column.}

\item{drop_list}{list of names of features to drop.}

\item{profile}{chosen profil among "quick", "normal", "advanced".}

\item{experiment_description}{experiment description.}

\item{metric}{name of the metric to optimise.}

\item{fold_column}{name of the fold column.}

\item{normal_models}{list of (normal) models to select with full FE & hyperparameters search (among "LR", "RF", "ET", "XGB", "LGB", "NN", "CB").}

\item{lite_models}{list of (lite) models to select with lite FE & default hyperparameters (among "LR", "RF", "ET", "XGB", "LGB", "NN", "CB", "NBC").}

\item{simple_models}{list of simple models to select (among "LR", "DT").}

\item{with_blend}{boolean, do we allow to include blend in the modelisation.}

\item{weight_column}{name of the weight columns.}

\item{features_engineering_selected_list}{list of feature engineering to select (among "Counter", "Date", "freq", "text_tfidf", "text_word2vec", "text_embedding", "tenc", "poly", "pca", "kmean").}

\item{features_selection_count}{number of features to keep after the feature selection process.}

\item{features_selection_time}{time budget in minutes of the feature selection process.}

\item{folder_dataset_id}{id of the dataset folder (images).}

\item{filename_column}{name of the file name path (images).}

\item{ymin}{name of the column matching the lower y value of the image (object detection).}

\item{ymax}{name of the column matching the higher y value of the image (object detection).}

\item{xmin}{name of the column matching the lower x value of the image (object detection).}

\item{xmax}{name of the column matching the higher x value of the image (object detection).}

\item{time_column}{name of column containing the timestamp (time series).}

\item{start_dw}{value of the start of derivative window (time series), should be a strict negative integer.}

\item{end_dw}{value of the end of derivative window (time series), should be a negative integer greater than start_dw.}

\item{start_fw}{value of the start of forecast window (time series), should be a strict positive integer.}

\item{end_fw}{value of the end of forecast window (time series), should be a strict positive integer greater than start_fw.}

\item{group_list}{list of name of feature that describes groups (time series).}

\item{apriori_list}{list of name of feature that are a priori (time series).}

\item{content_column}{content column name (text-similarity).}

\item{queries_dataset_id}{id of the dataset containing queries (text-similarity).}

\item{queries_dataset_content_column}{name of the column containing queries in the query dataset (text-similarity).}

\item{queries_dataset_id_column}{name of the ID column in the query dataset (text-similarity).}

\item{queries_dataset_matching_id_description_column}{name of the column matching id in the description dataset (text-similarity).}

\item{top_k}{top k individual to find (text-similarity).}

\item{lang}{lang of the text (text-similarity).}

\item{models_params}{parameters of the model (text-similarity).}

\item{name}{name of the external model (external model).}

\item{onnx_file}{path to the onnx file (external model).}

\item{yaml_file}{path to the yaml file (external model).}
}
\value{
list - experiment information.
}
\description{
Create a new version of an existing experiment.
}
