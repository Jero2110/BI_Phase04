# =============================================================================
# R/03b_eda_compare_service.R — Before/after view of the log1p transformation
# Input : raw RFM table (Service 2, rfm$rfm) and transformed table
#         (Service 2, rfm$rfm_features)
# Output: histograms, densities and boxplots for both stages, boxplots by
#         churn status, and a skewness table showing the effect of the transform
#
# If log_transform = FALSE in config.R, the "after" stage equals the raw stage.
# =============================================================================

COMPARE_VARS <- c("recency", "frequency", "monetary_total", "monetary_min", "monetary_avg")
STAGE_LEVELS <- c("Before (raw)", "After (transformed)")

#' Long format with one row per customer x variable x stage.
stage_long <- function(rfm_raw, rfm_features, vars = COMPARE_VARS) {
  to_long <- function(df, stage_name) {
    df |>
      dplyr::select(dplyr::all_of(vars)) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
      dplyr::mutate(stage = stage_name)
  }

  dplyr::bind_rows(
    to_long(rfm_raw, STAGE_LEVELS[1]),
    to_long(rfm_features, STAGE_LEVELS[2])
  ) |>
    dplyr::mutate(stage = factor(stage, levels = STAGE_LEVELS))
}

#' Histograms: raw on top, transformed below, one column per variable.
plot_hist_before_after <- function(long, bins = 40) {
  ggplot2::ggplot(long, ggplot2::aes(value)) +
    ggplot2::geom_histogram(bins = bins, fill = "steelblue", colour = "white") +
    ggplot2::facet_grid(stage ~ variable, scales = "free") +
    ggplot2::labs(title = "Histograms: raw vs transformed", x = NULL, y = "Customers") +
    ggplot2::theme_minimal()
}

#' Density plots, same layout as the histograms.
plot_density_before_after <- function(long) {
  ggplot2::ggplot(long, ggplot2::aes(value)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
    ggplot2::facet_grid(stage ~ variable, scales = "free") +
    ggplot2::labs(title = "Density: raw vs transformed", x = NULL, y = "Density") +
    ggplot2::theme_minimal()
}

#' Boxplots: raw on top, transformed below. Each variable has its own y-scale.
plot_box_before_after <- function(long) {
  ggplot2::ggplot(long, ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
    ggplot2::facet_grid(stage ~ variable, scales = "free_y") +
    ggplot2::labs(title = "Boxplots: raw vs transformed (points beyond whiskers = outliers)",
                  x = NULL, y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}

#' Boxplots of the transformed variables split by churn status.
plot_box_by_churn <- function(rfm_features, vars = COMPARE_VARS) {
  df <- rfm_features |>
    dplyr::select(churn, dplyr::all_of(vars)) |>
    tidyr::pivot_longer(-churn, names_to = "variable", values_to = "value") |>
    dplyr::mutate(status = dplyr::if_else(churn == 1, "Churned", "Active"))

  ggplot2::ggplot(df, ggplot2::aes(status, value, fill = status)) +
    ggplot2::geom_boxplot(show.legend = FALSE, outlier.alpha = 0.3) +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(title = "Transformed RFM variables by churn status",
                  x = NULL, y = "Value (transformed)") +
    ggplot2::theme_minimal()
}

#' Sample skewness: (E[(x - mean)^3]) / sd^3. Values near 0 indicate symmetry.
skewness <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  s <- stats::sd(x)
  mean((x - m)^3) / s^3
}

skew_compare <- function(rfm_raw, rfm_features, vars = COMPARE_VARS) {
  tibble::tibble(
    variable    = vars,
    skew_before = vapply(vars, function(v) skewness(rfm_raw[[v]]), numeric(1)),
    skew_after  = vapply(vars, function(v) skewness(rfm_features[[v]]), numeric(1))
  )
}

run_eda_compare_service <- function(rfm_raw, rfm_features, vars = COMPARE_VARS) {
  long <- stage_long(rfm_raw, rfm_features, vars)
  list(
    plots = list(
      hist_before_after    = plot_hist_before_after(long),
      density_before_after = plot_density_before_after(long),
      box_before_after     = plot_box_before_after(long),
      box_by_churn         = plot_box_by_churn(rfm_features, vars)
    ),
    skewness = skew_compare(rfm_raw, rfm_features, vars)
  )
}
