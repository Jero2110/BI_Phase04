# =============================================================================
# R/07_tuning_service.R — Service 7: hyperparameter tuning and final evaluation
# Input : training set, test set, baseline metrics (Service 5)
# Output: best parameters, tuned models, test metrics, gains, recommendation
#
# Logistic regression: glmnet, alpha (mixing, the "∝" in the brief) and lambda.
# SVM: radial kernel, sigma (kernel width) and C (cost).
# Selection criterion: F1-score from 5-fold CV repeated 3 times.
# caret refits each final model on the full training set with the best tune.
# =============================================================================

evaluate_on_test <- function(name, fit, test, cfg = CONFIG) {
  pred <- stats::predict(fit, newdata = test)
  list(
    name      = name,
    metrics   = dplyr::mutate(compute_metrics(test$churn, pred, cfg$positive_label),
                              model = name, .before = 1),
    confusion = confusion_table(test$churn, pred)
  )
}

#' Baseline vs tuned metrics on the same test set (rows must be in the same model order).
tuning_gains <- function(baseline, tuned) {
  stopifnot(nrow(baseline) == 2, nrow(tuned) == 2)
  tibble::tibble(
    model            = c("Logistic regression", "SVM (radial)"),
    f1_baseline      = baseline$f1,
    f1_tuned         = tuned$f1,
    f1_gain          = tuned$f1 - baseline$f1,
    recall_gain      = tuned$recall - baseline$recall,
    specificity_gain = tuned$specificity - baseline$specificity
  )
}

run_tuning_service <- function(train, test, baseline_metrics, cfg = CONFIG) {
  ctrl <- caret::trainControl(
    method = "repeatedcv",
    number = cfg$tune_folds,
    repeats = cfg$tune_repeats,
    classProbs = FALSE,
    summaryFunction = f1_summary
  )

  lr_grid <- expand.grid(
    alpha  = seq(0, 1, by = 0.25),
    lambda = 10^seq(-4, 0, length.out = 10)
  )

  svm_grid <- expand.grid(
    sigma = c(0.01, 0.05, 0.1, 0.25, 0.5, 1),
    C     = c(0.25, 0.5, 1, 2, 4, 8)
  )

  set.seed(cfg$seed)
  fit_lr <- caret::train(
    churn ~ ., data = train, method = "glmnet",
    tuneGrid = lr_grid, trControl = ctrl, metric = "F1",
    preProcess = c("center", "scale")
  )

  set.seed(cfg$seed)
  fit_svm <- caret::train(
    churn ~ ., data = train, method = "svmRadial",
    tuneGrid = svm_grid, trControl = ctrl, metric = "F1",
    preProcess = c("center", "scale")
  )

  ev_lr  <- evaluate_on_test("Logistic (tuned)", fit_lr, test, cfg)
  ev_svm <- evaluate_on_test("SVM radial (tuned)", fit_svm, test, cfg)

  tuned_metrics <- dplyr::bind_rows(ev_lr$metrics, ev_svm$metrics)

  list(
    fits          = list(logistic = fit_lr, svm = fit_svm),
    best_params   = list(logistic = fit_lr$bestTune, svm = fit_svm$bestTune),
    cv_results    = list(logistic = fit_lr$results, svm = fit_svm$results),
    evaluations   = list(logistic = ev_lr, svm = ev_svm),
    tuned_metrics = tuned_metrics,
    gains         = tuning_gains(baseline_metrics, tuned_metrics),
    recommendation = tuned_metrics$model[which.max(tuned_metrics$f1)]
  )
}
