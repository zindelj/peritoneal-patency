# ================================================================
# 04_visualize_curves.R
# Visualize smoothed and trimmed flow/pressure time series
# ================================================================

source("scripts/00_setup.R")
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)

# ---- Load data ----
processed_path <- here::here("data", "processed_df.rds")
auc_path       <- here::here("data", "auc_results.rds")

if (!file.exists(processed_path) || !file.exists(auc_path)) {
  stop("Required data missing. Run 02_preprocess_data.R and 03_compute_metrics.R first.")
}

df      <- readRDS(processed_path)
auc_res <- readRDS(auc_path)
cat("Loaded processed data and AUC results.\n")

# ---- Merge and normalize ----
df <- df %>%
  left_join(auc_res, by = c("id", "timepoint")) %>%
  mutate(
    flow_z = (flow - min(flow, na.rm = TRUE)) / (max(flow, na.rm = TRUE) - min(flow, na.rm = TRUE)),
    psi_z  = (psi  - min(psi,  na.rm = TRUE)) / (max(psi,  na.rm = TRUE) - min(psi,  na.rm = TRUE))
  )

# ---- Prepare long format ----
df_long <- df %>%
  pivot_longer(cols = c(psi_z, flow_z), names_to = "measurement", values_to = "value") %>%
  mutate(group = paste(measurement, "_", timepoint))

# ---- Define palettes ----
n_tp <- length(unique(df$timepoint))
blue_palette <- brewer.pal(min(n_tp, 9), "Blues")
red_palette  <- brewer.pal(min(n_tp, 9), "Reds")

color_mapping <- setNames(
  c(blue_palette, red_palette),
  c(unique(df_long$group[df_long$measurement == "psi_z"]),
    unique(df_long$group[df_long$measurement == "flow_z"]))
)

# ---- Select timepoints to visualize ----
nt <- c(1, 3, 5, 7)

# ---- Plot ----
p <- df_long %>%
  filter(timepoint %in% nt) %>%
  ggplot() +
  geom_line(data = ~ filter(.x, measurement == "psi_z"),
            aes(x = time_adjusted, y = value, color = group, linetype = measurement), size = 0.8) +
  geom_line(data = ~ filter(.x, measurement == "flow_z"),
            aes(x = time_adjusted, y = value, color = group, linetype = measurement), size = 0.8) +
  scale_y_continuous(name = "Normalized Pressure [0–1]",
                     sec.axis = sec_axis(~ ., name = "Normalized Flow [0–1]")) +
  scale_color_manual(values = color_mapping) +
  labs(x = "Time (s)", color = "Timepoint", linetype = "Measurement",
       title = "Flow and Pressure Curves per Mouse and Timepoint") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top") +
  facet_wrap(~ id)

print(p)

# ---- Save figure ----
fig_path <- here::here("figures", "Exp23_Curves_TP1_3_5_7.png")
ggsave(fig_path, plot = p, width = 20, height = 8, dpi = 300)
cat("Saved curve plot to:", fig_path, "\n")

# ---- Clean up ----
rm(df, df_long, auc_res, p)
gc()

# ================================================================
