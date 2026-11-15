# =============================================================================
# config.R — Central configuration shared by every service
# =============================================================================

CONFIG <- list(
  # ---- Data -----------------------------------------------------------------
  data_path            = "data/Appendix 3 - Database.csv",  # .xlsx or .csv (UCI Online Retail)
  snapshot_offset_days = 1,        # analysis date = last invoice date + offset
  churn_days           = 90,       # churn = no purchase within the last 90 days

  # ---- Preprocessing --------------------------------------------------------
  log_transform        = TRUE,     # log1p() on skewed RFM variables before scale()

  # ---- Clustering -----------------------------------------------------------
  k_initial            = 3,        # initial K-means choice
  k_range              = 2:10,     # search range for optimal k
  cluster_sample_size  = 3000,     # NULL = all customers (hclust needs an n x n distance matrix)
  nstart               = 25,       # K-means random starts
  k_override           = NULL,     # NULL = choose k by silhouette; set e.g. 4 to force k

  # ---- Classification -------------------------------------------------------
  train_prop           = 0.70,
  positive_label       = "Churned",
  negative_label       = "Retained",
  decision_threshold   = 0.5,

  # ---- Resampling / tuning --------------------------------------------------
  cv_folds             = 10,       # Exercise 4: 10-fold CV
  tune_folds           = 5,        # Exercise 6: 5-fold repeated CV
  tune_repeats         = 3,        # Exercise 6: 3 repetitions

  # ---- Reproducibility & output --------------------------------------------
  seed                 = 2024,
  artifacts_dir        = "artifacts"
)
