tabular_dataset <- function(type_problem, n_row = 100, n_features = 5){
  df = data.frame(replicate(n_features, runif(n_row, min = 0, max = 10)))
  if (type_problem == "regression") {
    df$TARGET = runif(n_row, min = 0, max = 10)
  }
  else if (type_problem == "classification") {
    df$TARGET = sample(0:1, n_row, rep = TRUE)
  }
  else if (type_problem == "multiclassification") {
    df$TARGET = sample(0:2, n_row, rep = TRUE)
  }
  df
}

timeseries_dataset <- function(n_row = 100, n_features = 5){
  df = data.frame(replicate(n_features+1, runif(n_row, min = 0, max = 10)))
  dt = as.Date("2018-01-01")
  df$TS = seq(from = dt, to = dt+n_row-1, length.out = n_row)
  names(df)[1] = "TARGET"
  df
}
