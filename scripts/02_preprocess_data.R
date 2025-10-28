# ================================================================
# 02_preprocess_data.R
# Data smoothing, trimming, baseline alignment, and cleaning
# ================================================================

source("scripts/00_setup.R")

# ---- Load merged data ----
data_path <- here::here("data", "merged_df.rds")
if (!file.exists(data_path)) stop("Merged data not found. Please run 01_import_data.R first.")
df <- readRDS(data_path)
cat("Loaded merged data with", nrow(df), "rows.\n")

# ---- Parameters ----
smooth_factor <- 10          # smoothing window size
quantile_threshold <- 0.5    # baseline quantile
margin <- 0.05               # tolerance around baseline
k <- 20                      # consecutive frames near baseline
time_window <- c(-10, 200)   # trimming window around baseline (in seconds)

# ---- Step 1: smoothing ----
library(zoo)
df <- df %>%
  group_by(id, timepoint) %>%
  arrange(time) %>%
  mutate(
    psi  = stats::filter(psi, rep(1 / smooth_factor, smooth_factor), circular = TRUE),
    flow = stats::filter(flow, rep(1 / smooth_factor, smooth_factor), circular = TRUE)
  ) %>%
  ungroup()
cat("Applied smoothing with factor =", smooth_factor, "\n")

# ---- Step 2: baseline detection and trimming ----
df <- df %>%
  group_by(id, timepoint) %>%
  arrange(time) %>%
  mutate(
    baseline_value = quantile(flow, probs = quantile_threshold, na.rm = TRUE),
    is_near_baseline = abs(flow - baseline_value) < abs(baseline_value + margin),
    stable_run = zoo::rollapply(is_near_baseline, width = k, FUN = all, fill = NA, align = "right"),
    t_baseline_flow = min(time[stable_run == TRUE], na.rm = TRUE),
    time_adjusted = time - t_baseline_flow
  ) %>%
  filter(time >= t_baseline_flow + time_window[1],
         time <= t_baseline_flow + time_window[2]) %>%
  ungroup()
cat("Trimmed data around baseline. Range =", paste(time_window, collapse = " to "), "seconds\n")

# ---- Step 3: noise cleanup ----
df <- df %>%
  mutate(
    psi = pmax(psi, 0),
    flow = pmax(flow, 0)
  )
cat("Removed negative psi/flow values.\n")

# ---- Step 4: Save processed data ----
processed_path <- here::here("data", "processed_df.rds")
saveRDS(df, processed_path)
cat("Saved processed data to:", processed_path, "\n")

# ---- Clean up environment ----
rm(df)
gc()
cat("Environment cleared.\n")

# ================================================================
