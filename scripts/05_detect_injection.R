# ================================================================
# 05_detect_injection.R
# Detect injection start (t_inj) per mouse/timepoint (validated)
# ================================================================

source("scripts/00_setup.R")
library(dplyr)
library(zoo)
library(lubridate)

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


# ---- Parameters ----
baseline_sec <- 10      # baseline duration
z_thr         <- 3      # z threshold for deviation
sustain_sec   <- 5      # required sustained deviation



## to test function
# subdf <- df %>%
#   filter(id == 209, timepoint == 1)



detect_tinj <- function(subdf,
                        baseline_sec = 10,
                        sustain_sec  = 3,
                        z_thr        = 2.5,
                        stable_scan_sec = 80,   # region to scan for stability
                        stable_win_sec = 4,     # window size for SD/slope
                        sd_thr = 0.004,         # threshold for SD stability
                        slope_thr = 0.001) {

  subdf <- subdf %>% arrange(time_s)

  # Get identifiers early
  this_id <- unique(subdf$id)[1]
  this_tp <- unique(subdf$timepoint)[1]


  # Sampling interval
  dt <- median(diff(subdf$time_s), na.rm = TRUE)
  sustain_n <- max(3L, round(sustain_sec / dt))
  win_n <- max(3L, round(stable_win_sec / dt))
  stable_min_n <- max(3L, round(stable_win_sec / dt))

  # Restrict to early region for baseline stability scan
  subdf_stable <- subdf %>% filter(time_s <= min(time_s) + stable_scan_sec)
  if (nrow(subdf_stable) < win_n)
    return(data.frame(id = this_id, timepoint = this_tp, t_inj_detected = NA_real_))

  # Rolling SD & slope
  subdf_stable <- subdf_stable %>%
    mutate(
      flow_roll_sd  = zoo::rollapply(flow, width = win_n, FUN = sd, fill = NA, align = "right"),
      psi_roll_sd   = zoo::rollapply(psi,  width = win_n, FUN = sd, fill = NA, align = "right"),
      flow_roll_slope = zoo::rollapply(flow, width = win_n,
                                       FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
                                       fill = NA, align = "right"),
      psi_roll_slope  = zoo::rollapply(psi,  width = win_n,
                                       FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
                                       fill = NA, align = "right")
    )

  # Identify stable baseline region
  subdf_stable$stable <- (abs(subdf_stable$flow_roll_slope) < slope_thr &
                            abs(subdf_stable$psi_roll_slope)  < slope_thr &
                            subdf_stable$flow_roll_sd < sd_thr &
                            subdf_stable$psi_roll_sd  < sd_thr)

  stable_run <- zoo::rollapply(subdf_stable$stable, width = stable_min_n, FUN = all, fill = FALSE, align = "right")
  last_stable_idx <- tail(which(stable_run), 1)
  t_baseline_end <- if (length(last_stable_idx)) subdf_stable$time_s[last_stable_idx] else NA

  # Baseline segment
  if (!is.na(t_baseline_end)) {
    baseline <- subdf %>% filter(time_s <= t_baseline_end)
  } else {
    baseline <- subdf %>% filter(time_s <= min(time_s) + baseline_sec)
  }



  mu_flow <- mean(baseline$flow, na.rm = TRUE)
  sd_flow <- sd(baseline$flow, na.rm = TRUE)
  mu_psi  <- mean(baseline$psi,  na.rm = TRUE)
  sd_psi  <- sd(baseline$psi,  na.rm = TRUE)
  if (sd_flow == 0) sd_flow <- 1e-6
  if (sd_psi == 0) sd_psi <- 1e-6

  subdf <- subdf %>%
    mutate(
      flow_z = (flow - mu_flow) / sd_flow,
      psi_z  = (psi  - mu_psi)  / sd_psi,
      flow_slope = c(NA, diff(flow)) / dt,
      psi_slope  = c(NA, diff(psi)) / dt
    )

  cond <- ((subdf$flow_z > z_thr & subdf$flow_slope > 0) |
             (subdf$psi_z  > z_thr & subdf$psi_slope  > 0))
  cond[is.na(cond)] <- FALSE

  sustained <- zoo::rollapply(cond, width = sustain_n, FUN = all, fill = FALSE, align = "left")
  idx <- which(sustained)[1]
  t_inj <- if (length(idx)) subdf$time_s[idx] else NA_real_

  data.frame(id = this_id, timepoint = this_tp, t_inj_detected = t_inj)
}



# Ensure required columns exist and are the right types
df$id        <- as.integer(df$id)
df$timepoint <- as.integer(df$timepoint)

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
