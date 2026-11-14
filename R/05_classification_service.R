# =============================================================================
# R/05_classification_service.R — Service 5: churn classification (baseline)
# Input : model-ready RFM features
# Output: split, models, confusion matrices, metrics table, balanced choice
#
# LEAKAGE NOTE: churn is defined as recency > 90 days, so recency would
# separate the classes perfectly and make the model meaningless. Recency is
# therefore EXCLUDED from the predictors. The model learns churn from
# purchase frequency and spend behaviour only.
# =============================================================================

CLASS_FEATURES <- c("frequency", "monetary_total", "monetary_min", "monetary_avg")

prepare_classification_data <- function(rfm_features, cfg = CONFIG) {
  rfm_features |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(CLASS_FEATURES)),
      churn = factor(
        dplyr::if_else(churn == 1, cfg$positive_label, cfg$negative_label),
        levels = c(cfg$negative_label, cfg$positive_label)
      )
    )
}

#' Stratified train/test split: createDataPartition keeps the churn ratio.
split_stratified <- function(df, cfg = CONFIG) {
  set.seed(cfg$seed)
  idx <- as.vector(caret::createDataPartition(df$churn, p = cfg$train_prop, list = FALSE))
  list(train = df[idx, ], test = df[-idx, ])
}

class_balance <- function(train, test) {
  dplyr::bind_rows(
    dplyr::count(train, churn) |> dplyr::mutate(set = "Train"),
    dplyr::count(test, churn)  |> dplyr::mutate(set = "Test")
  ) |>
    dplyr::group_by(set) |>
    dplyr::mutate(pct = round(100 * n / sum(n), 2)) |>
    dplyr::ungroup()
}

fit_logistic <- function(train) {
  stats::glm(churn ~ ., data = train, family = stats::binomial())
}

#' glm models P(second factor level) = P(Churned).
predict_logistic <- function(model, newdata, cfg = CONFIG) {
  p <- stats::predict(model, newdata = newdata, type = "response")
  factor(
    ifelse(p >= cfg$decision_threshold, cfg$positive_label, cfg$negative_label),
    levels = levels(newdata$churn)
  )
}

#' Radial SVM with default-style hyperparameters (cost = 1, gamma = 1/p).
fit_svm_baseline <- function(train, cfg = CONFIG) {
  set.seed(cfg$seed)
  e1071::svm(churn ~ ., data = train, kernel = "radial",
             cost = 1, gamma = 1 / (ncol(train) - 1), scale = TRUE)
}

evaluate_classifier <- function(name, actual, predicted, cfg = CONFIG) {
  m <- compute_metrics(actual, predicted, cfg$positive_label)
  list(
    name      = name,
    metrics   = dplyr::mutate(m, model = name, .before = 1),
    confusion = confusion_table(actual, predicted)
  )
}

#' Model with the best balance between false positives and false negatives,
#' measured as balanced accuracy = (recall + specificity) / 2.
select_balanced_model <- function(metrics) {
  metrics$model[which.max(metrics$balanced_accuracy)]
}

run_classification_service <- function(rfm_features, cfg = CONFIG) {
  df <- prepare_classification_data(rfm_features, cfg)
  sp <- split_stratified(df, cfg)

  lr  <- fit_logistic(sp$train)
  svm <- fit_svm_baseline(sp$train, cfg)

  ev_lr  <- evaluate_classifier("Logistic regression",
                                sp$test$churn, predict_logistic(lr, sp$test, cfg), cfg)
  ev_svm <- evaluate_classifier("SVM (radial)",
                                sp$test$churn, stats::predict(svm, sp$test), cfg)

  metrics <- dplyr::bind_rows(ev_lr$metrics, ev_svm$metrics)

  list(
    split         = sp,
    class_balance = class_balance(sp$train, sp$test),
    models        = list(logistic = lr, svm = svm),
    evaluations   = list(logistic = ev_lr, svm = ev_svm),
    metrics       = metrics,
    best          = select_balanced_model(metrics)
  )
}
