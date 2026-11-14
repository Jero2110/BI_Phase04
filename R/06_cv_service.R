# =============================================================================
# R/06_cv_service.R — Service 6: 10-fold cross-validation (baseline models)
# Input : training set (Service 5)
# Output: fitted caret models, resamples object, fold summary table, stability
# =============================================================================

#' Mean and SD per model and metric across the folds.
fold_summary_table <- function(res) {
  res$values |>
    tidyr::pivot_longer(-Resample, names_to = c("Model", "Metric"),
                        names_sep = "~", values_to = "value") |>
    dplyr::filter(Metric %in% c("Accuracy", "Recall", "F1")) |>
    dplyr::group_by(Model, Metric) |>
    dplyr::summarise(mean = mean(value, na.rm = TRUE),
                     sd   = stats::sd(value, na.rm = TRUE),
                     .groups = "drop")
}

#' The model with the smallest SD of F1 across folds is the most stable.
stability_verdict <- function(fold_table) {
  f1 <- dplyr::filter(fold_table, Metric == "F1")
  f1$Model[which.min(f1$sd)]
}

run_cv_service <- function(train, cfg = CONFIG) {
  ctrl <- caret::trainControl(
    method = "cv",
    number = cfg$cv_folds,
    classProbs = FALSE,
    summaryFunction = f1_summary,
    savePredictions = "final"
  )

  set.seed(cfg$seed)
  fit_lr <- caret::train(
    churn ~ ., data = train, method = "glm",
    trControl = ctrl, metric = "F1",
    preProcess = c("center", "scale")
  )

  set.seed(cfg$seed)
  svm_grid <- data.frame(sigma = 1 / (ncol(train) - 1), C = 1)  # same settings as the baseline SVM
  fit_svm <- caret::train(
    churn ~ ., data = train, method = "svmRadial",
    tuneGrid = svm_grid,
    trControl = ctrl, metric = "F1",
    preProcess = c("center", "scale")
  )

  res <- caret::resamples(list(Logistic = fit_lr, SVM = fit_svm))
  fold_tbl <- fold_summary_table(res)

  list(
    fits       = list(logistic = fit_lr, svm = fit_svm),
    resamples  = res,
    fold_table = fold_tbl,
    stability  = stability_verdict(fold_tbl)
  )
}
