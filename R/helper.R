### FUNCTIONS THAT SIMPLIFIES WORKING WITH RESSOURCES FROM PREVISION.IO BUT NOT LINKED TO API DIRECTLY ###

cvClassifAnalysis <- function(actual, predicted, fold, thresh = NULL, step = 1000) {
  #' Get metrics on a CV file retrieved by Prevision.io for a binary classification use case
  #'
  #' @param actual target comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param predicted prediction comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param fold fold number comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param thresh threshold to use. If not provided optimal threshold given F1 score will be computed
  #' @param step number of iteration done to find optimal thresh (1000 by default = 0.1% resolution per fold)
  #'
  #' @return data.frame with metrics computed on the CV
  #'
  #' @import data.table
  #' @import Metrics
  #'
  #' @export

  if(length(actual) != length(predicted)) {
    stop("actual and predicted don't have the same length")
  }

  if(length(actual) != length(fold)) {
    stop("actual and fold don't have the same length")
  }

  fold_unique     = unique(fold)
  auc             = NULL
  f1              = NULL
  f1_fold         = NULL
  precision_fold  = NULL
  recall_fold     = NULL
  threshold       = NULL

  for(i in sort(fold_unique)) {
    message("Computing for fold #", i)

    # COMPUTE AUC PER FOLD
    auc = c(auc, auc(actual[fold==i], predicted[fold==i]))

    # FIND OPTIMAL THRESHOLD FOR F1 SCORE PER FOLD
    for (j in 1:step) {
      f1 = rbind(f1, cbind(j, fbeta_score(actual[fold==i], ifelse(predicted[fold==i] < j/step, 0, 1))))
    }

    # BEST THRESHOLD IS THE ONE THAT MAXIMISE F1 EXCEPT IF PROVIDED BY THE USER
    if(is.null(thresh)) {
      threshold = c(threshold, f1[,1][which.max(f1[,2])]/step)
    }
    else {
      threshold = rep(thresh, length(fold_unique))
    }

    # RESET F1 FOR LOOP
    f1 = NULL

    # STORE F1 SCORE / RECALL / PRECISION / SPECIFITY FOR OPTI THRESH PER FOLD
    f1_fold         = c(f1_fold, fbeta_score(actual[fold==i], ifelse(predicted[fold==i] < threshold[i], 0, 1)))
    recall_fold     = c(recall_fold, recall(actual[fold==i], ifelse(predicted[fold==i] < threshold[i], 0, 1)))
    precision_fold  = c(precision_fold, precision(actual[fold==i], ifelse(predicted[fold==i] < threshold[i], 0, 1)))
  }

  print("F1 by fold")
  print(f1_fold)
  print("=======================================")
  print("Precision by fold")
  print(precision_fold)
  print("=======================================")
  print("Recall by fold")
  print(recall_fold)
  print("=======================================")
  print("Threshold by fold")
  print(threshold)
  print("=======================================")

  res = cbind(round(mean(auc), 4),
              round(mean(f1_fold), 4),
              round(mean(precision_fold), 4),
              round(mean(recall_fold), 4),
              round(mean(threshold), 4))

  res = data.frame(res)
  names(res) = c("AUC", "F1", "PRECISION", "RECALL", "THRESHOLD")

  return(res)
}

plotClassifAnalysis <- function(actual, predicted, top) {
  #' Plot RECALL, PRECISION & F1 SCORE versus top n predictions for a binary classification use case
  #'
  #' @param actual true value (0 or 1 only)
  #' @param predicted prediction vector (probability)
  #' @param top top individual to analyse
  #'
  #' @return data.frame with metrics computed on the CV
  #'
  #' @import data.table
  #' @import Metrics
  #'
  #' @export

  if(length(actual) != length(predicted)) {
    stop("actual and predicted don't have the same length")
  }

  rec   = NULL
  prec  = NULL
  f1    = NULL

  # SORT THE 2 VECTORS BY PREDICTED DECREASING
  idx       = order(predicted, decreasing = T)
  actual    = actual[idx]
  predicted = predicted[idx]

  # COMPUTE INDICATORS
  for (i in 1:top) {
    predicted_1_0 = ifelse(predicted >= predicted[i], 1, 0)
    rec   = c(rec, recall(actual, predicted_1_0))
    prec  = c(prec, precision(actual, predicted_1_0))
    f1    = c(f1, fbeta_score(actual, predicted_1_0))
  }

  # SELECT THE ITERATION NUMBER THAT MAXIMISES F1
  top = which.max(f1)

  # PLOT RESULTS
  par(mfrow=c(3,1))

  plot(rec, type="l", ylab = "RECALL", xlab = "TOP INDIVIDUS", main = "Recall en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  plot(prec, type="l", ylab = "PRECISION", xlab = "TOP INDIVIDUS", main = "Precision en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  plot(f1, type="l", ylab = "F1", xlab = "TOP INDIVIDUS", main = "F1 Score en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  res = cbind(round(f1[top], 4),
              round(prec[top], 4),
              round(rec[top], 4),
              round(top, 4))

  res = data.frame(res)
  names(res) = c("F1", "PRECISION", "RECALL", "TOP")

  return(res)
}

optimalPrediction <- function(usecaseId, modelId, df, actionable, nbSample, maximize, zip = F) {
  #' Compute the optimal prediction for each rows in a data frame, for a given model, a list of actionable features and a number of samples for each features to be tested
  #'
  #' @param usecaseId the id of the usecase to be predicted on
  #' @param modelId the id of the model to be predicted on
  #' @param df a data frame to be predicted on
  #' @param actionable a list of actionables features contained in the names of the data frame
  #' @param nbSample a vector of number of sample for each actionable features
  #' @param maximize a boolean indicating if we maximize or minimize the predicted target
  #' @param zip a boolean indicating if the data frame to predict should be zipped prior sending to the instance
  #'
  #' @return row data.frame with the optimal vector and the prediction associated with for each rows in the original data frame
  #'
  #' @import data.table
  #' @import Metrics
  #'
  #' @export

  # PRELIMINARY CHECKS
  if(all(!actionable %in% names(df))) {
    stop("actionable features should be included in the data frame column names")
  }

  if(length(actionable) != length(nbSample)) {
    stop("the number of actionable features should match the length of number of sample")
  }

  # GET THE TRAIN SET USED FOR MODELLING
  datasetId = getUsecaseInfos(usecaseId)$datasetId
  train     = createDataframeFromDataset(datasetId)

  # SELECT RANDOM SAMPLES FROM THE TRAIN SET GIVEN ACTIONNABLES FEATURES & NBSAMPLES
  temp = list()
  for(i in 1:length(actionable)) {
    temp[[i]] = sample(x = train[[actionable[i]]], size =  nbSample[i], replace = T)
  }

  # EXPEND THE ORIGINAL DATASET WITH CARTESIAN PRODUCT OF ACTIONNABLE FEATURES
  cartesian = merge(temp[[1]], temp[[2]])
  names(cartesian) = actionable

  # REMOVED ACTIONNABLE FEATURES FROM THE SUBMITTED DATASET
  df[, actionable] = NULL

  # MERGE DATASET WITH EXPENDED FEATURES
  df = as.data.frame(df)
  df = merge(df, cartesian)

  # START PREDICTION
  message("the optimisation process will make ", nrow(df)," predictions")

  df_prevision = createDatasetFromDataframe("expensed_df", df, zip = zip)
  pred         = startPrediction(usecaseId = usecaseId,
                                 modelId = modelId,
                                 datasetId = df_prevision$`_id`,
                                 confidence = F)

  # GET PREDICTION
  Sys.sleep(30) ## SUBOPTIMAL, SHOULD BE A "WAIT UNTIL PREDICTION DONE"
  res = getPrediction(usecaseId, pred$`_id`)
  res = res[, ncol(res), with = F]
  res = cbind(df, res)

  # RETURN MAX OR MIN OF PREDICTION GROUPED BY EVERY NON ACTIONNABLE FEATURE
  res = data.table(res)
  by  = names(res)[!names(res) %in% c(actionable, names(res)[ncol(res)])]

  if(maximize) {
    res = res[, .(optimal_pred = max(get(names(res)[ncol(res)]))), by = by]
  }
  else {
    res = res[, .(optimal_pred = min(get(names(res)[ncol(res)]))), by = by]
  }

  return(res)
}
