### FUNCTIONS THAT SIMPLIFIES WORKING WITH RESSOURCES FROM PREVISION.IO BUT NOT LINKED TO API DIRECTLY ###

helper_cv_classif_analysis <- function(actual, predicted, fold, thresh = NULL, step = 1000) {
  #' Get metrics on a CV file retrieved by Prevision.io for a binary classification use case
  #'
  #' @param actual target comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param predicted prediction comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param fold fold number comming from the cross Validation dataframe retrieved by Prevision.io
  #' @param thresh threshold to use. If not provided optimal threshold given F1 score will be computed
  #' @param step number of iteration done to find optimal thresh (1000 by default = 0.1\% resolution per fold)
  #'
  #' @return data.frame with metrics computed on the CV
  #'
  #' @import data.table
  #' @import Metrics
  #' @import utils
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
    message("\nComputing for fold #", i)

    # COMPUTE AUC PER FOLD
    auc = c(auc, auc(actual[fold==i], predicted[fold==i]))

    # INITIALISATION OF PROGRESS BAR
    pb = txtProgressBar(1, 100, style = 3)

    # FIND OPTIMAL THRESHOLD FOR F1 SCORE PER FOLD
    for (j in 1:step) {
      ## UPDATE PROGRESS BAR
      setTxtProgressBar(pb, 100*j/step)

      ## COMPUTE F1 SCORE AT EACH STEP
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
  print(round(f1_fold, 4))
  print("=======================================")
  print("Precision by fold")
  print(round(precision_fold, 4))
  print("=======================================")
  print("Recall by fold")
  print(round(recall_fold,4))
  print("=======================================")
  print("Threshold by fold")
  print(threshold)
  print("=======================================")
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

helper_plot_classif_analysis <- function(actual, predicted, top, compute_every_n = 1) {
  #' Plot RECALL, PRECISION & F1 SCORE versus top n predictions for a binary classification use case
  #'
  #' @param actual true value (0 or 1 only)
  #' @param predicted prediction vector (probability)
  #' @param top top individual to analyse
  #' @param compute_every_n compute indicators every n individuals (1 by default)
  #'
  #' @return data.frame with metrics computed on the CV
  #'
  #' @import data.table
  #' @import Metrics
  #' @import utils
  #' @importFrom graphics abline
  #' @importFrom graphics par
  #'
  #' @export

  # PRELIMINARY CHECKS
  if(length(actual) != length(predicted)) {
    stop("actual and predicted don't have the same length")
  }

  if(compute_every_n < 1) {
    stop("compute_every_n should be an integer >= 1")
  }

  if(compute_every_n >= top) {
    stop("compute_every_n should be strictly lesser than top")
  }

  if(compute_every_n > 1) {
    message("indicators may be approximated due to compute_every_n beeing greater than 1")
  }

  rec   = NULL
  prec  = NULL
  f1    = NULL

  # SORT THE 2 VECTORS BY PREDICTED DECREASING
  idx       = order(predicted, decreasing = T)
  actual    = actual[idx]
  predicted = predicted[idx]

  # INITIALISATION OF PROGRESS BAR
  pb = txtProgressBar(1, 100, style = 3)

  # COMPUTE INDICATORS
  for (i in seq(1, top, compute_every_n)) {
    ## UPDATE PROGRESS BAR
    setTxtProgressBar(pb, 100*i/max(seq(1, top, compute_every_n)))

    ## COMPUTE KPIS
    predicted_1_0 = ifelse(predicted >= predicted[i], 1, 0)
    rec   = rbind(rec, c(i, recall(actual, predicted_1_0)))
    prec  = rbind(prec, c(i, precision(actual, predicted_1_0)))
    f1    = rbind(f1, c(i, fbeta_score(actual, predicted_1_0)))
  }

  # SELECT THE ITERATION NUMBER THAT MAXIMISES F1
  top = f1[which.max(f1[,2]),1]

  # PLOT RESULTS
  par(mfrow=c(3,1))

  plot(rec, type="l", ylab = "RECALL", xlab = "TOP INDIVIDUS", main = "Recall en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  plot(prec, type="l", ylab = "PRECISION", xlab = "TOP INDIVIDUS", main = "Precision en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  plot(f1, type="l", ylab = "F1", xlab = "TOP INDIVIDUS", main = "F1 Score en fonction du top n individus", ylim = c(0,1), col = I("#27818D"))
  abline(v=top, col = "red", lty = 2)

  res = cbind(round(f1[round(top/compute_every_n), 2], 4),
              round(prec[round(top/compute_every_n), 2], 4),
              round(rec[round(top/compute_every_n), 2], 4),
              top)

  res = data.frame(res)
  names(res) = c("F1", "PRECISION", "RECALL", "TOP")

  return(res)
}

helper_optimal_prediction <- function(project_id, usecase_id, model_id, df, actionable_features, nb_sample, maximize, zip = F, version = 1) {
  #' [BETA] Compute the optimal prediction for each rows in a data frame, for a given model, a list of actionable features and a number of samples for each features to be tested.
  #'
  #' @param project_id id of the project containing the use case.
  #' @param usecase_id id of the usecase to be predicted on.
  #' @param model_id id of the model to be predicted on.
  #' @param df a data frame to be predicted on.
  #' @param actionable_features a list of actionable_featuress features contained in the names of the data frame.
  #' @param nb_sample a vector of number of sample for each actionable_features features.
  #' @param maximize a boolean indicating if we maximize or minimize the predicted target.
  #' @param zip a boolean indicating if the data frame to predict should be zipped prior sending to the instance.
  #' @param version version of the use case we want to make the prediction on.
  #'
  #' @return row data.frame with the optimal vector and the prediction associated with for each rows in the original data frame.
  #'
  #' @import data.table
  #'
  #' @export

  # PRELIMINARY CHECKS
  if(all(!actionable_features %in% names(df))) {
    stop("actionable_features features should be included in the data frame column names")
  }

  if(length(actionable_features) != length(nb_sample)) {
    stop("the number of actionable_features features should match the length of number of sample")
  }

  # GET THE TRAIN SET USED FOR MODELLING
  dataset_id = get_usecase_info(usecase_id)[[1]]$dataset_id
  train      = create_dataframe_from_dataset(dataset_id)

  # SELECT RANDOM SAMPLES FROM THE TRAIN SET GIVEN ACTIONNABLES FEATURES & nb_sampleS
  temp = list()
  for(i in 1:length(actionable_features)) {
    temp[[i]] = sample(x = train[[actionable_features[i]]], size =  nb_sample[i], replace = T)
  }

  # EXPEND THE ORIGINAL DATASET WITH CARTESIAN PRODUCT OF ACTIONNABLE FEATURES
  cartesian = merge(temp[[1]], temp[[2]])
  names(cartesian) = actionable_features

  # REMOVED ACTIONNABLE FEATURES FROM THE SUBMITTED DATASET
  df[, actionable_features] = NULL

  # MERGE DATASET WITH EXPENDED FEATURES
  df = as.data.frame(df)
  df = merge(df, cartesian)

  # START PREDICTION
  message("the optimisation process will make ", nrow(df)," predictions")

  df_prevision = create_dataset_from_dataframe(project_id, "expensed_df", df, zip = zip)
  pred         = create_prediction(usecase_version_id = get_usecase_version_id(usecase_id, version),
                                   model_id = model_id,
                                   dataset_id = df_prevision$`_id`,
                                   confidence = F)

  # GET PREDICTION
  Sys.sleep(30) ## SUBOPTIMAL, SHOULD BE A "WAIT UNTIL PREDICTION DONE"
  res = get_prediction(pred$`_id`)
  res = res[, ncol(res), with = F]
  res = cbind(df, res)

  # RETURN MAX OR MIN OF PREDICTION GROUPED BY EVERY NON ACTIONNABLE FEATURE
  res = data.table(res)
  by  = names(res)[!names(res) %in% c(actionable_features, names(res)[ncol(res)])]

  if(maximize) {
    res = res[, list(optimal_pred = max(get(names(res)[ncol(res)]))), by = by]
  }
  else {
    res = res[, list(optimal_pred = min(get(names(res)[ncol(res)]))), by = by]
  }

  return(res)
}

helper_drift_analysis <- function(dataset_1, dataset_2, p_value = 0.05, features = NULL) {
  #' [BETA] Return a data.frame that contains features, a boolean indicating if the feature may have a different distribution between the submitted datasets (if p-value < threshold), their exact p-value and the test used to compute it.
  #'
  #' @param dataset_1 the first data set
  #' @param dataset_2 the second data set
  #' @param p_value a p-value that will be the decision criteria for deciding if a feature is suspicious 5\% by default
  #' @param features a vector of features names that should be tested. If NULL, only the intersection of the names() will be kept
  #'
  #' @return a vector of suspicious features
  #'
  #' @import data.table
  #' @import utils
  #' @import stats
  #'
  #' @export

  if(is.null(features)) {
    features = intersect(names(dataset_1), names(dataset_2))
  }

  # INITIALISATION OF RESULTS DATAFRAME
  suspicious = data.frame(matrix(ncol = 4, nrow = length(features)))
  names(suspicious) = c("feature", "is_suspicious", "p_value", "test")

  # INITIALISATION OF PROGRESS BAR
  pb = txtProgressBar(1, 100, style = 3)

  # TESTING FEATURES
  for(f in features) {
    ## UPDATE PROGRESS BAR
    setTxtProgressBar(pb, 100*match(f, features)/length(features))

    ## KS TEST FOR NUMERICAL FEATURE WITH HIGH CARDINALITY [> 20 UNIQUE]
    if(is.numeric(dataset_1[[f]]) && length(unique(dataset_1[[f]])) > 20) {
      res = suppressWarnings(ks.test(dataset_1[[f]], dataset_2[[f]])) # REMOVE WARNINGS BECAUSE OF EVENTUAL TIES
      suspicious[match(f, features),] = c(f, ifelse(res$p.value < p_value, 1, 0), res$p.value, "ks.test")
    }

    ## KHI SQUARED FOR CATEGORICAL FEATURES OR NUMERIC FEATURES WITH LOW CARDINALITY [<= 20 UNIQUE]
    else {
      ### KEEP ONLY MATCHING VALUES TO AVOID REPLICA THAT WILL LEAD TO INCORRECT COMPUTATION
      val = intersect(dataset_1[[f]], dataset_2[[f]])

      ### COMPUTE CONTAGENCY TABLE AND MAKE TEST ON IT ONLY IF AT LEAST 2 MODALITIES
      if(length(val) > 1) {
        mat = cbind(table(dataset_1[[f]][dataset_1[[f]] %in% val]), table(dataset_2[[f]][dataset_2[[f]] %in% val]))
        res = chisq.test(mat, simulate.p.value = T, B = 5000)
        suspicious[match(f, features),] = c(f, ifelse(res$p.value < p_value, 1, 0), res$p.value, "chisq.test")
      }
    }
  }

  # RETURN ORDERED DATA.FRAME BY p_value WITH GOOD TYPE
  suspicious$is_suspicious = as.numeric(suspicious$is_suspicious)
  suspicious$p_value = as.numeric(suspicious$p_value)
  return(suspicious[order(suspicious$p_value),])
}
