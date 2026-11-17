# =============================================================================
# R/04b_cluster_comparison_service.R — Compare two K-means solutions:
#   (a) k chosen automatically by the silhouette, and
#   (b) a manually chosen k (e.g. from business judgement or the dendrogram).
#
# Input : rfm_features, rfm_raw (Service 2), k_manual, optionally the result
#         of an automatic run (`auto`) to avoid recomputing it
# Output: summary metrics, cross-tab, adjusted Rand index, profiles, plot
#
# Both runs use the same seed and the same customer sample, so the comparison
# is like-for-like.
# =============================================================================

#' Adjusted Rand index: agreement between two partitions, corrected for chance.
#' 1 = identical partitions, around 0 = agreement no better than random.
adjusted_rand <- function(a, b) {
  tab   <- table(a, b)
  n     <- sum(tab)
  comb2 <- function(v) sum(choose(v, 2))

  index     <- comb2(tab)
  sum_rows  <- comb2(rowSums(tab))
  sum_cols  <- comb2(colSums(tab))
  expected  <- sum_rows * sum_cols / choose(n, 2)
  max_index <- (sum_rows + sum_cols) / 2

  (index - expected) / (max_index - expected)
}

compare_k_solutions <- function(rfm_features, rfm_raw, k_manual,
                                auto = NULL, cfg = CONFIG) {
  if (is.null(auto)) {
    auto <- run_clustering_service(rfm_features, rfm_raw,
                                   modifyList(cfg, list(k_override = NULL)))
  }
  manual <- run_clustering_service(rfm_features, rfm_raw,
                                   modifyList(cfg, list(k_override = k_manual)))

  stopifnot(identical(auto$idx, manual$idx))   # both must cluster the same customers

  sol_labels <- c(
    auto   = paste0("Silhouette (k=", auto$k_optimal_kmeans, ")"),
    manual = paste0("Manual (k=", manual$k_optimal_kmeans, ")")
  )
  sols <- list(auto = auto, manual = manual)

  summary_tbl <- tibble::tibble(
    solution = unname(sol_labels),
    k        = c(auto$k_optimal_kmeans, manual$k_optimal_kmeans),
    avg_silhouette = unname(vapply(sols, function(s) {
      mean(cluster::silhouette(s$kmeans$cluster, s$dist)[, "sil_width"])
    }, numeric(1))),
    explained_variance_pct = unname(vapply(sols, function(s) {
      100 * s$kmeans$betweenss / s$kmeans$totss
    }, numeric(1))),
    smallest_segment_pct = unname(vapply(sols, function(s) {
      100 * min(table(s$kmeans$cluster)) / length(s$kmeans$cluster)
    }, numeric(1)))
  )

  plot_df <- dplyr::bind_rows(
    auto$assignments   |> dplyr::mutate(solution = sol_labels[["auto"]],
                                        cluster = kmeans_cluster),
    manual$assignments |> dplyr::mutate(solution = sol_labels[["manual"]],
                                        cluster = kmeans_cluster)
  )

  comparison_plot1 <- ggplot2::ggplot(plot_df,
                                     ggplot2::aes(frequency, monetary_total, colour = cluster)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~solution) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(title = "K-means: automatic k vs manual k",
                  x = "Frequency (log scale)", y = "Total spend (log scale)",
                  colour = "Cluster") +
    ggplot2::theme_minimal()

  comparison_plot2 <- ggplot2::ggplot(plot_df,
                                      ggplot2::aes(frequency, recency, colour = cluster)) +
    ggplot2::geom_point(alpha = 0.5, size = 1.2) +
    ggplot2::facet_wrap(~solution) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(title = "K-means: automatic k vs manual k",
                  x = "Frequency (log scale)", y = "Total spend (log scale)",
                  colour = "Cluster") +
    ggplot2::theme_minimal()
  
  list(
    summary    = summary_tbl,
    crosstab   = table(Silhouette = auto$kmeans$cluster, Manual = manual$kmeans$cluster),
    ari        = adjusted_rand(auto$kmeans$cluster, manual$kmeans$cluster),
    profiles   = list(auto = auto$profiles$kmeans, manual = manual$profiles$kmeans),
    plot1      = comparison_plot1,
    plot2      = comparison_plot2,
    auto       = auto,
    manual     = manual
  )
}
