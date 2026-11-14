# =============================================================================
# R/01_import_service.R — Service 1: ingestion & cleaning
# Input : path to the Online Retail file (.xlsx or .csv)
# Output: list(transactions = tibble, quality_log = tibble)
#
# Customer identifier: the dataset already provides CustomerID, so no ID is
# generated. Rows without CustomerID are removed because they cannot be
# attributed to a customer.
# =============================================================================

NON_PRODUCT_CODES <- c("POST", "D", "M", "BANK CHARGES", "PADS", "DOT",
                       "CRUK", "C2", "AMAZONFEE")

parse_invoice_date <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  lubridate::parse_date_time(x, orders = c("mdy HM", "mdy HMS", "ymd HMS"), tz = "UTC")
}

import_transactions <- function(path) {
  if (!file.exists(path)) stop("Data file not found: ", path, call. = FALSE)

  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    readxl::read_excel(path)
  } else {
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "latin1"),
      show_col_types = FALSE,
      col_types = readr::cols(
        InvoiceNo   = readr::col_character(),
        StockCode   = readr::col_character(),
        CustomerID  = readr::col_character(),
        InvoiceDate = readr::col_character(),
        .default    = readr::col_guess()
      )
    )
  }
}

clean_transactions <- function(raw) {
  df <- raw
  names(df) <- tolower(gsub("[^A-Za-z]", "", names(df)))  # "CustomerID" -> "customerid"

  df |>
    dplyr::transmute(
      invoice_no   = as.character(invoiceno),
      stock_code   = as.character(stockcode),
      description  = as.character(description),
      quantity     = as.numeric(quantity),
      invoice_date = parse_invoice_date(invoicedate),
      unit_price   = as.numeric(unitprice),
      customer_id  = as.character(customerid),
      country      = as.character(country)
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      !is.na(customer_id),
      !is.na(invoice_date),
      !is.na(invoice_no),
      !grepl("^C", invoice_no),          # cancellation invoices
      !stock_code %in% NON_PRODUCT_CODES,
      quantity > 0,
      unit_price > 0
    ) |>
    dplyr::mutate(line_value = quantity * unit_price)
}

run_import_service <- function(path = CONFIG$data_path) {
  raw <- import_transactions(path)
  tx  <- clean_transactions(raw)

  quality_log <- tibble::tibble(
    metric = c("Raw rows", "Clean transaction rows", "Unique customers", "Rows removed (%)"),
    value  = c(nrow(raw), nrow(tx), dplyr::n_distinct(tx$customer_id),
               round(100 * (1 - nrow(tx) / nrow(raw)), 1))
  )

  list(transactions = tx, quality_log = quality_log)
}
