# =============================================================================
# R/03_eda_service.R — Service 3: descriptive EDA
# Input : raw RFM table (Service 2)
# Output: list(plots, stats, outliers, churn_rate)
#
# Plots use log1p() on the x-axis for the monetary and frequency variables,
# because their raw distributions are extremely right-skewed.
# =============================================================================

EDA_VARS <- c("recency", "frequency", "monetary_total", "monetary_min", "monetary_avg")

#' Long format with one row per customer x variable
eda_long <- function(rfm, apply_log1p, vars = EDA_VARS) {
  rfm |>
    dplyr::select(dplyr::all_of(vars)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
    dplyr::mutate(
      value_plot = if (apply_log1p) log1p(value) else value
    )
}

#' Histograms: one column per variable
plot_histograms <- function(long, x_label) {
  ggplot2::ggplot(long, ggplot2::aes(value_plot)) +
    ggplot2::geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(title = "Histograms of RFM variables", x = x_label, y = "Customers") +
    ggplot2::theme_minimal()
}

#' Density plots, same layout as the histograms.
plot_densities <- function(long, x_label) {
  ggplot2::ggplot(long, ggplot2::aes(value_plot)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.5) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(title = "Density plots of RFM variables", x = x_label, y = "Density") +
    ggplot2::theme_minimal()
}

# Churn plot with the amount of customers active and inactive
plot_churn_bar <- function(rfm) {
  df <- rfm |>
    dplyr::count(status = dplyr::if_else(churn == 1, "Churned (>90 days)", "Active (<=90 days)"))

  ggplot2::ggplot(df, ggplot2::aes(status, n, fill = status)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4) +
    ggplot2::labs(title = "Customer status", x = NULL, y = "Customers") +
    ggplot2::theme_minimal()
}

# Frequency bars by the frequency of the clients
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

# General descriptive statistics
descriptive_stats <- function(rfm, vars = EDA_VARS) {
  dplyr::bind_rows(lapply(vars, function(v) {
    x <- rfm[[v]]
    tibble::tibble(variable = v, mean = mean(x), median = stats::median(x),
                   sd = stats::sd(x), min = min(x), max = max(x))
  }))
}

#' Boxplots: raw on top, transformed below. Each variable has its own y-scale.
plot_box <- function(long) {
  ggplot2::ggplot(long, ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.3) +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggplot2::labs(title = "Boxplots (points beyond whiskers = outliers)",
                  x = NULL, y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank())
}

#' Boxplots of the transformed variables split by churn status.
plot_box_by_churn <- function(rfm_features, vars = EDA_VARS) {
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

skew_compare <- function(rfm_raw, rfm_features, vars = EDA_VARS) {
  tibble::tibble(
    variable    = vars,
    skew_before = vapply(vars, function(v) skewness(rfm_raw[[v]]), numeric(1)),
    skew_after  = vapply(vars, function(v) skewness(rfm_features[[v]]), numeric(1))
  )
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

run_eda_service <- function(rfm_raw, rfm_features, apply_log1p = FALSE, vars = EDA_VARS) {
  long <- eda_long(rfm_raw, apply_log1p)
  x_label <- if (apply_log1p) "log1p(value)" else "Raw scale"
  list(
    plots = list(
      histograms    = plot_histograms(long, x_label),
      densities     = plot_densities(long, x_label),
      churn_bar     = plot_churn_bar(rfm_raw),
      frequency_bar = plot_frequency_bars(rfm_raw),
      box           = plot_box(long),
      box_by_churn  = plot_box_by_churn(rfm_features, vars)
    ),
    skewness = skew_compare(rfm_raw, rfm_features, vars),
    stats     = descriptive_stats(rfm_raw),
    outliers  = outlier_summary(rfm_raw),
    churn_rate = mean(rfm_raw$churn)
  )
}
