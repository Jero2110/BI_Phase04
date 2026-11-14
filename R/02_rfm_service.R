# =============================================================================
# R/02_rfm_service.R — Service 2: customer-level RFM and churn label
# Input : cleaned transactions (Service 1)
# Output: list(rfm = raw customer table, rfm_features = model-ready table)
#
# Monetary values are computed per INVOICE first (sum of line values), then
# aggregated per customer, so min / avg refer to order value, not line value.
# =============================================================================

build_rfm <- function(tx, cfg = CONFIG) {
  snapshot <- max(tx$invoice_date) + lubridate::days(cfg$snapshot_offset_days)

  invoices <- tx |>
    dplyr::group_by(customer_id, invoice_no) |>
    dplyr::summarise(
      invoice_date  = max(invoice_date),
      invoice_value = sum(line_value),
      .groups = "drop"
    )

  invoices |>
    dplyr::group_by(customer_id) |>
    dplyr::summarise(
      last_purchase  = max(invoice_date),
      frequency      = dplyr::n(),
      monetary_total = sum(invoice_value),
      monetary_min   = min(invoice_value),
      monetary_avg   = mean(invoice_value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      recency = as.numeric(difftime(snapshot, last_purchase, units = "days")),
      churn   = as.integer(recency > cfg$churn_days)
    ) |>
    dplyr::select(customer_id, recency, frequency, monetary_total,
                  monetary_min, monetary_avg, churn, last_purchase)
}

#' Log-transform the right-skewed RFM variables (if enabled in config).
transform_rfm <- function(rfm, cfg = CONFIG) {
  if (!cfg$log_transform) return(rfm)
  rfm |>
    dplyr::mutate(dplyr::across(
      c(recency, frequency, monetary_total, monetary_min, monetary_avg), log1p
    ))
}

run_rfm_service <- function(tx, cfg = CONFIG) {
  rfm <- build_rfm(tx, cfg)
  list(
    rfm          = rfm,
    rfm_features = transform_rfm(rfm, cfg),
    churn_rate   = mean(rfm$churn)
  )
}
