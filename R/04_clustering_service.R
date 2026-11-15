# =============================================================================
# R/04_clustering_service.R — Service 4: segmentation
# Input : model-ready RFM features + raw RFM (for interpretable profiles)
# Output: models, optimal k values, cross-tab, profiles and plots
#
# Both K-means and hierarchical clustering run on the SAME standardised
# matrix and the SAME distance matrix, so the two solutions are comparable.
# If the customer base is larger than cfg$cluster_sample_size, a random sample
# is used (hclust needs an n x n distance matrix in memory).
# =============================================================================

CLUSTER_VARS <- c("recency", "frequency", "monetary_total", "monetary_min", "monetary_avg")

#' Scatter plot of two variables coloured by cluster, one facet per method.
plot_cluster_scatter <- function(assign_df, x_var, y_var, title,
                                 log_x = FALSE, log_y = FALSE) {
  long <- assign_df |>
    tidyr::pivot_longer(c(kmeans_cluster, hclust_cluster),
                        names_to = "method", values_to = "cluster") |>
    dplyr::mutate(method = dplyr::recode(method,
                                         kmeans_cluster = "K-means",
                                         hclust_cluster = "Hierarchical"))

  p <- ggplot2::ggplot(long, ggplot2::aes(.data[[x_var]], .data[[y_var]], colour = cluster)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~method) +
    ggplot2::labs(title = title, colour = "Cluster", x = x_var, y = y_var) +
    ggplot2::theme_minimal()

  if (log_x) p <- p + ggplot2::scale_x_log10()
  if (log_y) p <- p + ggplot2::scale_y_log10()
  p
}

#' Interpretable segment profile (uses raw, untransformed values).
cluster_profile <- function(assign_df, cluster_col) {
  total <- nrow(assign_df)
  assign_df |>
    dplyr::group_by(cluster = .data[[cluster_col]]) |>
    dplyr::summarise(
      customers          = dplyr::n(),
      share_pct          = round(100 * dplyr::n() / total, 1),
      mean_recency_days  = round(mean(recency), 1),
      median_frequency   = stats::median(frequency),
      mean_total_spend   = round(mean(monetary_total), 1),
      mean_avg_order     = round(mean(monetary_avg), 1),
      churn_rate_pct     = round(100 * mean(churn), 1),
      .groups = "drop"
    )
}

#' Choose the number of clusters.
#' If `k_override` is set in config.R, it is used (after validation).
#' Otherwise the k with the highest average silhouette in `k_range` is used.
resolve_k <- function(k_override, k_range, avg_silhouette) {
  if (!is.null(k_override)) {
    if (!is.numeric(k_override) || length(k_override) != 1 ||
        k_override < 2 || k_override != round(k_override)) {
      stop("k_override must be a single whole number >= 2 (or NULL).", call. = FALSE)
    }
    return(as.integer(k_override))
  }
  k_range[which.max(avg_silhouette)]
}

run_clustering_service <- function(rfm_features, rfm_raw, cfg = CONFIG) {
  set.seed(cfg$seed)
  n <- nrow(rfm_features)

  idx <- if (!is.null(cfg$cluster_sample_size) && n > cfg$cluster_sample_size) {
    sort(sample.int(n, cfg$cluster_sample_size))
  } else {
    seq_len(n)
  }

  x    <- scale(as.matrix(rfm_features[idx, CLUSTER_VARS]))  # standardise: mean 0, sd 1
  base <- rfm_raw[idx, ]
  d    <- stats::dist(x)                                       # shared distance matrix

  # ---- 1) K-means with the initial k --------------------------------------
  km_initial <- stats::kmeans(x, centers = cfg$k_initial, nstart = cfg$nstart)

  # ---- 2) Elbow (WSS) and average silhouette to choose k -------------------
  elbow_plot <- factoextra::fviz_nbclust(
    x, stats::kmeans, method = "wss",
    k.max = max(cfg$k_range), nstart = cfg$nstart
  )

  sil_km <- vapply(cfg$k_range, function(k) {
    cl <- stats::kmeans(x, centers = k, nstart = cfg$nstart)$cluster
    mean(cluster::silhouette(cl, d)[, "sil_width"])
  }, numeric(1))

  k_opt <- resolve_k(cfg$k_override, cfg$k_range, sil_km)
  km    <- stats::kmeans(x, centers = k_opt, nstart = cfg$nstart)  # retrain at chosen k

  sil_plot <- ggplot2::ggplot(
    tibble::tibble(k = cfg$k_range, avg_silhouette = sil_km),
    ggplot2::aes(k, avg_silhouette)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_vline(xintercept = k_opt, linetype = "dashed") +
    ggplot2::scale_x_continuous(breaks = cfg$k_range) +
    ggplot2::labs(title = "Average silhouette width by k (K-means)",
                  x = "k", y = "Average silhouette") +
    ggplot2::theme_minimal()

  # ---- 3) Hierarchical agglomerative clustering (Ward.D2) -----------------
  hc <- stats::hclust(d, method = "ward.D2")

  sil_hc <- vapply(cfg$k_range, function(k) {
    mean(cluster::silhouette(stats::cutree(hc, k), d)[, "sil_width"])
  }, numeric(1))

  k_hc  <- resolve_k(cfg$k_override, cfg$k_range, sil_hc)
  hc_cl <- stats::cutree(hc, k = k_hc)

  dend_plot <- factoextra::fviz_dend(
    hc, k = k_hc, show_labels = FALSE, rect = TRUE,
    main = "Hierarchical clustering (Ward.D2) dendrogram"
  )

  # ---- 4) Comparison -------------------------------------------------------
  assignments <- base |>
    dplyr::mutate(kmeans_cluster = factor(km$cluster),
                  hclust_cluster = factor(hc_cl))

  list(
    idx                = idx,
    x                  = x,
    dist               = d,
    k_initial          = cfg$k_initial,
    km_initial         = km_initial,
    k_optimal_kmeans   = k_opt,
    kmeans             = km,
    silhouette_kmeans  = sil_km,
    k_optimal_hclust   = k_hc,
    hclust             = hc,
    silhouette_hclust  = sil_hc,
    crosstab           = table(KMeans = km$cluster, Hierarchical = hc_cl),
    assignments        = assignments,
    profiles = list(
      kmeans = cluster_profile(assignments, "kmeans_cluster"),
      hclust = cluster_profile(assignments, "hclust_cluster")
    ),
    plots = list(
      elbow       = elbow_plot,
      silhouette  = sil_plot,
      dendrogram  = dend_plot,
      scatter_frequency_spend = plot_cluster_scatter(
        assignments, "frequency", "monetary_total",
        "Frequency vs total spend, coloured by cluster", log_x = TRUE, log_y = TRUE),
      scatter_recency_frequency = plot_cluster_scatter(
        assignments, "recency", "frequency",
        "Recency vs frequency, coloured by cluster", log_y = TRUE)
    )
  )
}
