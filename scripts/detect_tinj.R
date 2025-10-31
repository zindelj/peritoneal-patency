# ================================================================
# detect_tinj.R
# Detect injection start (t_inj_start) from flow/pressure curves
# ================================================================

detect_tinj <- function(subdf,
                        baseline_sec = 10,
                        sustain_sec  = 3,
                        z_thr        = 3,
                        stable_scan_sec = 80,
                        stable_win_sec = 4,
                        sd_thr = 0.004,
                        slope_thr = 0.001,
                        median_sec = 2,     # robust de-spiking
                        mean_sec   = 6) {   # heavy trend smoothing

  require(dplyr)
  require(zoo)

  subdf <- subdf %>% arrange(time_s)
  this_id <- unique(subdf$id)[1]
  this_tp <- unique(subdf$timepoint)[1]

  # ---- Sampling interval ----
  dt <- median(diff(subdf$time_s), na.rm = TRUE)
  sustain_n <- max(3L, round(sustain_sec / dt))
  win_n <- max(3L, round(stable_win_sec / dt))
  stable_min_n <- max(3L, round(stable_win_sec / dt))

  # ---- Heavy smoothing (robust) ----
  med_n <- max(3L, round(median_sec / dt))
  avg_n <- max(3L, round(mean_sec   / dt))

  flow_med <- zoo::rollmedian(subdf$flow, k = med_n, fill = "extend", align = "center")
  psi_med  <- zoo::rollmedian(subdf$psi,  k = med_n, fill = "extend", align = "center")

  flow_sm <- zoo::rollmean(flow_med, k = avg_n, fill = "extend", align = "center")
  psi_sm  <- zoo::rollmean(psi_med,  k = avg_n, fill = "extend", align = "center")

  # ---- Restrict to early region for baseline stability scan ----
  subdf_stable <- subdf %>% filter(time_s <= min(time_s) + stable_scan_sec)
  if (nrow(subdf_stable) < win_n)
    return(data.frame(id = this_id, timepoint = this_tp, t_inj_detected = NA_real_))

  # ---- Rolling SD & slope for stability check ----
  subdf_stable <- subdf_stable %>%
    mutate(
      flow_sm = flow_sm[match(time_s, subdf$time_s)],
      psi_sm  = psi_sm[match(time_s, subdf$time_s)],
      flow_roll_sd  = zoo::rollapply(flow_sm, width = win_n, FUN = sd, fill = NA, align = "right"),
      psi_roll_sd   = zoo::rollapply(psi_sm,  width = win_n, FUN = sd, fill = NA, align = "right"),
      flow_roll_slope = zoo::rollapply(flow_sm, width = win_n,
                                       FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
                                       fill = NA, align = "right"),
      psi_roll_slope  = zoo::rollapply(psi_sm, width = win_n,
                                       FUN = function(x) coef(lm(x ~ seq_along(x)))[2],
                                       fill = NA, align = "right")
    )

  subdf_stable$stable <- (abs(subdf_stable$flow_roll_slope) < slope_thr &
                            abs(subdf_stable$psi_roll_slope)  < slope_thr &
                            subdf_stable$flow_roll_sd < sd_thr &
                            subdf_stable$psi_roll_sd  < sd_thr)

  stable_run <- zoo::rollapply(subdf_stable$stable, width = stable_min_n, FUN = all, fill = FALSE, align = "right")
  last_stable_idx <- tail(which(stable_run), 1)
  t_baseline_end <- if (length(last_stable_idx)) subdf_stable$time_s[last_stable_idx] else NA

  # ---- Baseline segment ----
  if (!is.na(t_baseline_end)) {
    baseline_idx <- which(subdf$time_s <= t_baseline_end)
  } else {
    baseline_idx <- which(subdf$time_s <= min(subdf$time_s) + baseline_sec)
  }

  mu_flow <- mean(flow_sm[baseline_idx], na.rm = TRUE)
  sd_flow <- sd(  flow_sm[baseline_idx], na.rm = TRUE)
  mu_psi  <- mean( psi_sm[baseline_idx], na.rm = TRUE)
  sd_psi  <- sd(   psi_sm[baseline_idx], na.rm = TRUE)
  if (sd_flow == 0) sd_flow <- 1e-6
  if (sd_psi == 0) sd_psi <- 1e-6

  # ---- Z-scores and slopes ----
  subdf <- subdf %>%
    mutate(
      flow_z    = (flow_sm - mu_flow) / sd_flow,
      psi_z     = (psi_sm  - mu_psi)  / sd_psi,
      flow_slope = c(NA, diff(flow_sm)) / dt,
      psi_slope  = c(NA, diff(psi_sm))  / dt
    )

  # ---- Detection condition (flow OR pressure rise) ----
  cond <- ((subdf$flow_z > z_thr & subdf$flow_slope > 0) |
             (subdf$psi_z  > z_thr & subdf$psi_slope  > 0))
  cond[is.na(cond)] <- FALSE

  sustained <- zoo::rollapply(cond, width = sustain_n, FUN = all, fill = FALSE, align = "left")
  idx <- which(sustained)[1]
  t_inj <- if (length(idx)) subdf$time_s[idx] else NA_real_

  data.frame(id = this_id, timepoint = this_tp, t_inj_detected = t_inj)
}
