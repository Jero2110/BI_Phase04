# Customer Retention & Churn Analysis — UK Online Retail

R project that turns the UCI *Online Retail* transactional dataset (01/12/2010 – 09/12/2011) into a churn analysis: RFM segmentation, clustering, churn classification, cross-validation and hyperparameter tuning.

## Structure

```
churn_project/
├── config.R                        # all parameters in one place
├── main.Rmd                        # integrated report: orchestrates all services
├── README.md
├── R/
│   ├── utils.R                     # metrics, caret summary, artifact saving
│   ├── 01_import_service.R         # ingestion & cleaning
│   ├── 02_rfm_service.R            # customer-level RFM, churn label
│   ├── 03_eda_service.R            # histograms, densities, bar charts, outliers
│   ├── 04_clustering_service.R     # scale(), K-means, hclust, comparison
│   ├── 05_classification_service.R # stratified split, logistic regression, SVM
│   ├── 06_cv_service.R             # 10-fold CV, fold statistics
│   └── 07_tuning_service.R         # glmnet + radial SVM tuning, test evaluation
├── data/                           # place "Online Retail.xlsx" (or .csv) here
└── artifacts/                      # plots and tables written by the report
```

## Service contracts

Each file in `R/` is a self-contained service: it has one entry point, takes explicit inputs and returns a list. The report is the only place where services are chained together, so any service can be replaced or deployed on its own.

| Service | Entry point | Input | Output |
|---|---|---|---|
| 1 Import | `run_import_service(path)` | file path | `transactions`, `quality_log` |
| 2 RFM | `run_rfm_service(tx)` | transactions | `rfm`, `rfm_features`, `churn_rate` |
| 3 EDA | `run_eda_service(rfm)` | raw RFM | `plots`, `stats`, `outliers` |
| 4 Clustering | `run_clustering_service(features, raw)` | RFM features | models, `k_optimal_*`, profiles, plots |
| 5 Classification | `run_classification_service(features)` | RFM features | split, models, metrics, confusion matrices |
| 6 Cross-validation | `run_cv_service(train)` | training set | fits, `resamples`, `fold_table` |
| 7 Tuning | `run_tuning_service(train, test, baseline)` | train / test | best params, tuned metrics, gains |

## Requirements

R ≥ 4.1 (native pipe `|>`) and these packages:

```r
install.packages(c("tidyverse", "readxl", "lubridate", "factoextra", "cluster",
                   "caret", "glmnet", "e1071", "kernlab", "knitr", "rmarkdown"))
```

## Running

1. Put `Online Retail.xlsx` in `data/` (or change `data_path` in `config.R` to a `.csv`).
2. From the project root:

```r
rmarkdown::render("main.Rmd")   # produces main.html and artifacts/
```

## Key decisions

- **Customer identifier.** `CustomerID` is present in the dataset, so no identifier is generated. Rows without it are dropped.
- **Cancellations and non-product rows.** Invoices starting with `C`, non-product codes (postage, bank charges, etc.) and rows with non-positive quantity or price are removed.
- **Monetary variables.** Computed per invoice first, then aggregated per customer. Minimum and average therefore refer to order value.
- **Churn.** `churn = 1` when recency > 90 days, measured from the last invoice date plus one day.
- **Transformation.** `log1p()` is applied to the right-skewed RFM variables before `scale()`. Set `log_transform = FALSE` in `config.R` to disable it.
- **Leakage guard.** Recency is excluded from the classifiers' predictors because it defines the label. Including it would give perfect separation and meaningless metrics.
- **Clustering sample.** Hierarchical clustering needs an n × n distance matrix. Above `cluster_sample_size` (default 3,000), a random sample is used, and K-means and hclust run on the same sample so the comparison is fair. Set it to `NULL` to use all customers if memory allows.
- **Balanced choice.** The "best balance" between false positives and false negatives is measured as balanced accuracy, the mean of recall and specificity.
- **Tuning criterion.** F1-score throughout, as requested. The glmnet `alpha` is the "∝" mixing parameter in the brief.
- **Stratification.** `caret::createDataPartition()` keeps the churn ratio in both train and test sets.
