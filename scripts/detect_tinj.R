# ================================================================
# detect_tinj_forward()
# Detect start and end of injection period (forward direction)
# ================================================================

detect_tinj <- function(subdf,
                                baseline_sec = 10,
                                sustain_sec  = 5,
                                z_thr        = 3,
                                stable_scan_sec = 40,
                                stable_win_sec  = 5,
                                sd_thr = 0.002,
                                slope_thr = 0.0005) {

  subdf <- subdf %>% arrange(time_s)
  this_id <- unique(subdf$id)[1]
  this_tp <- unique(subdf$timepoint)[1]

  # Sampling and window sizes
  dt <- median(diff(subdf$time_s), na.rm = TRUE)
  sustain_n <- max(3L, round(sustain_sec / dt))
  win_n <- max(3L, round(stable_win_sec / dt))
  stable_min_n <- win_n

  # ----- Baseline detection (forward) -----
  subdf_stable <- subdf %>% filter(time_s <= min(time_s) + stable_scan_sec)

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

  subdf_stable$stable <- (abs(subdf_stable$flow_roll_slope) < slope_thr &
                            abs(subdf_stable$psi_roll_slope)  < slope_thr &
                            subdf_stable$flow_roll_sd < sd_thr &
                            subdf_stable$psi_roll_sd  < sd_thr)

  stable_run <- zoo::rollapply(subdf_stable$stable, width = stable_min_n,
                               FUN = all, fill = FALSE, align = "right")
  last_stable_idx <- tail(which(stable_run), 1)
  t_baseline_end <- if (length(last_stable_idx)) subdf_stable$time_s[last_stable_idx] else min(subdf$time_s) + baseline_sec

  # ----- Baseline statistics -----
  baseline <- subdf %>% filter(time_s <= t_baseline_end)
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

  # ----- Injection start detection -----
  cond_start <- ((subdf$flow_z > z_thr & subdf$flow_slope > 0) |
                   (subdf$psi_z  > z_thr & subdf$psi_slope  > 0))
  cond_start[is.na(cond_start)] <- FALSE

  sustained_start <- zoo::rollapply(cond_start, width = sustain_n, FUN = all,
                                    fill = FALSE, align = "left")
  idx_start <- which(sustained_start)[1]
  t_inj_start <- if (length(idx_start)) subdf$time_s[idx_start] else NA_real_

  # ----- Injection end detection -----
  if (!is.na(t_inj_start)) {
    post_df <- subdf %>% filter(time_s > t_inj_start)
    post_df <- post_df %>%
      mutate(
        flow_roll_sd = zoo::rollapply(flow, width = win_n, FUN = sd, fill = NA, align = "right"),
        psi_roll_sd  = zoo::rollapply(psi,  width = win_n, FUN = sd, fill = NA, align = "right")
      )
    post_df$stable <- (post_df$flow_roll_sd < sd_thr & post_df$psi_roll_sd < sd_thr)
    sustained_post <- zoo::rollapply(post_df$stable, width = stable_min_n, FUN = all,
                                     fill = FALSE, align = "right")
    idx_end <- which(sustained_post)[1]
    t_inj_end <- if (length(idx_end)) post_df$time_s[idx_end] else NA_real_
  } else {
    t_inj_end <- NA_real_
  }

  data.frame(id = this_id,
             timepoint = this_tp,
             t_inj_start = t_inj_start,
             t_inj_end   = t_inj_end)
}
