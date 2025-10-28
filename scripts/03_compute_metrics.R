# ================================================================
# 03_compute_metrics.R
# Compute AUCs, mean resistance, and diagnostic plots
# ================================================================

source("scripts/00_setup.R")

library(pracma)
library(ggplot2)
library(dplyr)

# ---- Load processed data ----
proc_path <- here::here("data", "processed_df.rds")
if (!file.exists(proc_path)) stop("Processed data not found. Run 02_preprocess_data.R first.")
df <- readRDS(proc_path)
cat("Loaded processed data with", nrow(df), "rows.\n")

# ---- Step 1: Compute AUCs ----
auc_res <- df %>%
  mutate(
    time = as.numeric(time),
    psi  = as.numeric(psi),
    flow = as.numeric(flow)
  ) %>%
  group_by(id, timepoint) %>%
  arrange(time) %>%
  summarise(
    auc_psi  = pracma::trapz(time, psi),
    auc_flow = pracma::trapz(time, flow),
    .groups  = "drop"
  ) %>%
  mutate(
    mean_resistance = auc_psi / auc_flow
  ) %>%
  filter(is.finite(mean_resistance))

cat("Computed AUCs and mean resistance for", nrow(auc_res), "ID–timepoint pairs.\n")

# ---- Step 2: Diagnostic plots ----
# AUC relationship plot
r2_values <- auc_res %>%
  group_by(timepoint) %>%
  summarise(r_squared = summary(lm(auc_flow ~ auc_psi))$r.squared)

p1 <- ggplot(auc_res, aes(x = auc_psi, y = auc_flow)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  facet_wrap(~timepoint) +
  labs(title = "Flow vs Pressure (AUC relationship)",
       x = "AUC Pressure", y = "AUC Flow") +
  theme_minimal()

print(p1)

# Mean ± SE over timepoints
summary_data <- auc_res %>%
  group_by(timepoint) %>%
  summarise(
    mean_value = mean(mean_resistance),
    se_value   = sd(mean_resistance) / sqrt(n()),
    .groups = "drop"
  )

p2 <- ggplot(summary_data, aes(x = timepoint, y = mean_value)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), width = 0.2) +
  labs(title = "Mean Resistance ± SE over Timepoints",
       x = "Timepoint", y = "Mean Resistance") +
  theme_bw()

print(p2)

# Density plot for threshold visualization
thresholds <- c(6, 70)
p3 <- ggplot(auc_res, aes(x = mean_resistance)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(xintercept = thresholds, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Mean Resistance with Thresholds",
       x = "Mean Resistance", y = "Density") +
  theme_minimal()

print(p3)

# ---- Step 3: Save outputs ----
auc_path <- here::here("data", "auc_results.rds")
saveRDS(auc_res, auc_path)
cat("Saved AUC results to:", auc_path, "\n")

# ---- Optional cleanup ----
rm(df)
gc()

# ================================================================
