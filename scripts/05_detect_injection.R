# ================================================================
# 05_detect_injection.R
# Detect injection start (t_inj) per mouse/timepoint (validated)
# ================================================================

# ---- Setup ----
source("scripts/00_setup.R")
library(ggplot2)
library(patchwork)
library(zoo)
library(dplyr)
library(lubridate)

# ---- Load detection functions ----
source("scripts/detect_tinj.R")


# ---- Load merged data ----
df <- read.csv(here::here("data", "merged_df.csv"))

# Convert HH:MM:SS to seconds
# Replace the lubridate line with this safe base-R conversion
df$time_s <- sapply(strsplit(as.character(df$time), ":"), function(x) {
  if (length(x) == 3) {
    as.numeric(x[1]) * 3600 + as.numeric(x[2]) * 60 + as.numeric(x[3])
  } else {
    NA_real_
  }
})


# Ensure required columns exist and are the right types
df$id        <- as.integer(df$id)
df$timepoint <- as.integer(df$timepoint)
cat("✅ Loaded merged data with", nrow(df), "rows.\n")


# ---- Split into groups ----
groups <- split(df, list(df$id, df$timepoint), drop = TRUE)


# ---- Run detection ----
res_list <- lapply(groups, function(g)
  detect_tinj(g,
              baseline_sec = 10,
              sustain_sec  = 5,
              z_thr        = 3)
)

tinj_table <- do.call(rbind, res_list)
rownames(tinj_table) <- NULL
cat("✅ Detected injection starts for", sum(!is.na(tinj_table$t_inj_start)),
    "out of", nrow(tinj_table), "groups.\n")


# ---- Save results ----
saveRDS(tinj_table, here::here("data", "tinj_results.rds"))
write.csv(tinj_table, here::here("data", "tinj_results.csv"), row.names = FALSE)
cat("💾 Saved t_inj_start results to data/tinj_results.*\n")


# ---- Plot QC ----
df_plot <- df %>% left_join(tinj_table, by = c("id", "timepoint"))


pdf_path <- here::here("figures", "QC_injection_starts.pdf")
pdf(pdf_path, width = 12, height = 6)

ids <- sort(unique(df_plot$id))
for (i in ids) {
  df_i <- df_plot %>% filter(id == i)
  p <- ggplot(df_i, aes(x = time_s)) +
    geom_line(aes(y = flow, color = "Flow")) +
    geom_line(aes(y = psi, color = "Pressure")) +
    geom_vline(aes(xintercept = t_inj_start),
               data = df_i %>% distinct(timepoint, t_inj_start),
               linetype = "dashed", color = "black", linewidth = 0.6) +
    facet_wrap(~ timepoint, scales = "free_y") +
    scale_color_manual(values = c("Flow" = "#1f78b4", "Pressure" = "#e31a1c")) +
    labs(title = paste("ID", i, "- Flow & Pressure with Detected Injection Start"),
         x = "Time [s]", y = "Signal", color = "") +
    theme_bw(base_size = 12) +
    theme(legend.position = "top")
  print(p)
}

dev.off()
cat("📈 QC PDF saved to:", pdf_path, "\n")

# ---- Done ----
cat("🎯 05_detect_injection.R completed successfully.\n")














### debugging stuff

subdf <- df %>% filter(id == 210, timepoint == 1)



ggplot(subdf, aes(time_s, flow)) +
  geom_line() +
  geom_line(aes(y = psi), color = "red") +
  theme_minimal()

t_max <- max(subdf$time_s, na.rm = TRUE)

subdf_rev <- subdf %>%
  arrange(desc(time_s)) %>%
  mutate(time_s_rev = t_max - time_s) %>%
  select(-time_s) %>%
  rename(time_s = time_s_rev)




ggplot(subdf_rev, aes(time_s, flow)) +
  geom_line() +
  geom_line(aes(y = psi), color = "red") +
  theme_minimal()

detect_tinj_forward(subdf_rev,
            baseline_sec = 10,
            sustain_sec  = 5,
            z_thr = 3)


detect_tinj_forward(subdf,
                    baseline_sec = 10,
                    sustain_sec  = 5,
                    z_thr = 3)







# Split into (id, timepoint) groups
groups <- split(df, list(df$id, df$timepoint), drop = TRUE)

# Run detection on each group
res_list <- lapply(groups, function(g)
  detect_tinj(g, baseline_sec = 20, sustain_sec = 3, z_thr = 3)
)

# Combine
tinj_table <- do.call(rbind, res_list)
rownames(tinj_table) <- NULL

head(tinj_table, 10)
table(is.na(tinj_table$t_inj_detected))


## sanity check for missing tinj
tinj_missing <- tinj_table %>% filter(is.na(t_inj_detected))
df %>%
  semi_join(tinj_missing, by = c("id", "timepoint")) %>%
  ggplot(aes(time_s, flow)) +
  geom_line() +
  facet_wrap(~ id + timepoint)


# ---- Save ----
saveRDS(tinj_table, here::here("data", "tinj_table.rds"))
write.csv(tinj_table, here::here("data", "tinj_table.csv"), row.names = FALSE)

cat("✅ Injection start detection complete.\n")
print(head(tinj_table, 10))
