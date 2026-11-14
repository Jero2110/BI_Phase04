# =============================================================================
# R/utils.R — Shared helpers used by every service
# =============================================================================

#' Binary classification metrics. `positive` is the class of interest (churn).
compute_metrics <- function(actual, predicted, positive) {
  actual    <- as.factor(actual)
  predicted <- factor(predicted, levels = levels(actual))
  negative  <- setdiff(levels(actual), positive)

  TP <- sum(predicted == positive & actual == positive)
  TN <- sum(predicted == negative & actual == negative)
  FP <- sum(predicted == positive & actual == negative)
  FN <- sum(predicted == negative & actual == positive)

  precision   <- if (TP + FP == 0) NA_real_ else TP / (TP + FP)
  recall      <- if (TP + FN == 0) NA_real_ else TP / (TP + FN)
  specificity <- if (TN + FP == 0) NA_real_ else TN / (TN + FP)
  f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) {
    0
  } else {
    2 * precision * recall / (precision + recall)
  }

  tibble::tibble(
    accuracy          = (TP + TN) / length(actual),
    recall            = recall,
    specificity       = specificity,
    precision         = precision,
    f1                = f1,
    balanced_accuracy = mean(c(recall, specificity)),
    TP = TP, TN = TN, FP = FP, FN = FN
  )
}

#' 2x2 confusion table (rows = predicted, columns = actual).
confusion_table <- function(actual, predicted) {
  table(Predicted = predicted, Actual = actual)
}

#' Custom caret summary function (class predictions, not probabilities).
#' Levels must be ordered c(negative, positive), so lev[2] is the positive class.
f1_summary <- function(data, lev = NULL, model = NULL) {
  m <- compute_metrics(data$obs, data$pred, positive = lev[2])
  c(Accuracy = m$accuracy, Recall = m$recall,
    Specificity = m$specificity, F1 = m$f1)
}

#' Stop early with a helpful message if packages are missing.
require_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Install missing packages first: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Save a ggplot, a data frame (CSV) or any other object (RDS) to artifacts/.
save_artifact <- function(obj, name, cfg = CONFIG) {
  dir.create(cfg$artifacts_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(cfg$artifacts_dir, name)
  if (inherits(obj, "ggplot")) {
    ggplot2::ggsave(path, obj, width = 10, height = 6, dpi = 150)
  } else if (is.data.frame(obj)) {
    readr::write_csv(obj, sub("\\.[^.]+$", ".csv", path))
  } else {
    saveRDS(obj, sub("\\.[^.]+$", ".rds", path))
  }
  invisible(path)
}
