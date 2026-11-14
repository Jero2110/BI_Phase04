# =============================================================================
# R/03_eda_service.R — Service 3: descriptive EDA
# Input : raw RFM table (Service 2)
# Output: list(plots, stats, outliers, churn_rate)
#
# Plots use log1p() on the x-axis for the monetary and frequency variables,
# because their raw distributions are extremely right-skewed.
# =============================================================================

EDA_VARS <- c("recency", "frequency", "monetary_total", "monetary_min", "monetary_avg")

eda_long <- function(rfm, vars = EDA_VARS) {
  rfm |>
    dplyr::select(dplyr::all_of(vars)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
    dplyr::mutate(value_plot = log1p(value))
}

plot_histograms <- function(long) {
  ggplot2::ggplot(long, ggplot2::aes(value_plot)) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(title = "Histograms of RFM variables", x = "log1p(value)", y = "Customers") +
    ggplot2::theme_minimal()
}

plot_densities <- function(long) {
  ggplot2::ggplot(long, ggplot2::aes(value_plot)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(title = "Density plots of RFM variables", x = "log1p(value)", y = "Density") +
    ggplot2::theme_minimal()
}

plot_churn_bar <- function(rfm) {
  df <- rfm |>
    dplyr::count(status = dplyr::if_else(churn == 1, "Churned (>90 days)", "Active (<=90 days)"))

  ggplot2::ggplot(df, ggplot2::aes(status, n, fill = status)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4) +
    ggplot2::labs(title = "Customer status", x = NULL, y = "Customers") +
    ggplot2::theme_minimal()
}

plot_frequency_bars <- function(rfm) {
  df <- rfm |>
    dplyr::mutate(frequency_band = cut(
      frequency,
      breaks = c(0, 1, 2, 5, 10, 20, Inf),
      labels = c("1", "2", "3-5", "6-10", "11-20", "21+"),
      include.lowest = TRUE
    )) |>
    dplyr::count(frequency_band)

  ggplot2::ggplot(df, ggplot2::aes(frequency_band, n)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(title = "Customers by number of orders", x = "Orders", y = "Customers") +
    ggplot2::theme_minimal()
}

descriptive_stats <- function(rfm, vars = EDA_VARS) {
  dplyr::bind_rows(lapply(vars, function(v) {
    x <- rfm[[v]]
    tibble::tibble(variable = v, mean = mean(x), median = stats::median(x),
                   sd = stats::sd(x), min = min(x), max = max(x))
  }))
}

#' IQR rule (1.5 * IQR) outlier counts per variable.
outlier_summary <- function(rfm, vars = EDA_VARS) {
  dplyr::bind_rows(lapply(vars, function(v) {
    x  <- rfm[[v]]
    q  <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
    iq <- diff(q)
    n_out <- sum(x < q[1] - 1.5 * iq | x > q[2] + 1.5 * iq)
    tibble::tibble(variable = v, q1 = q[1], q3 = q[2],
                   outliers = n_out, outlier_pct = round(100 * n_out / length(x), 1))
  }))
}

run_eda_service <- function(rfm) {
  long <- eda_long(rfm)
  list(
    plots = list(
      histograms = plot_histograms(long),
      densities  = plot_densities(long),
      churn_bar  = plot_churn_bar(rfm),
      frequency_bar = plot_frequency_bars(rfm)
    ),
    stats     = descriptive_stats(rfm),
    outliers  = outlier_summary(rfm),
    churn_rate = mean(rfm$churn)
  )
}
