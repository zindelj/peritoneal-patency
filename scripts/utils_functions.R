# ================================================================
# utils_functions.R
# Helper functions for data import and preprocessing (debug-aligned)
# ================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(hms)
  library(zoo)
})

# ---- Import pressure data (.csv files) ----
  import_pressure_data <- function(path, nskip, verbose = FALSE) {
  csv_filenames <- list.files(path, pattern = ".csv", full.names = TRUE)

  pressure_df <- csv_filenames %>%
    map_dfr(~ read_csv(.x, skip = nskip, show_col_types = FALSE) %>%
              mutate(filename = basename(.x)))


  ## Mutate pressure values
  colnames(pressure_df)
  pressure_df <- pressure_df %>%
    rename(time = "TIME (hh:mm:ss)",
           tempcount = "RAW TEMPERATURE COUNT",
           pressurecount = "RAW PRESSURE COUNT",
           celcius = "TEMPERATURE(\xb0C )",
           psi = "PRESSURE (PSI G)",
           other = "...6",
           filename = filename) %>%
    select(-c(other, tempcount))

  pressure_aggregated <- pressure_df %>%
    group_by(filename, time) %>%
    summarise(psi = mean(psi), .groups = "drop")

  pressure_aggregated$id <- strsplit(pressure_aggregated$filename, split = "_") %>% sapply("[[", 2)
  pressure_aggregated$id <- gsub("[^0-9]", "", pressure_aggregated$id)
  pressure_aggregated$timepoint <- strsplit(pressure_aggregated$filename, split = "_") %>% sapply("[[", 3)
  pressure_aggregated$timepoint <- gsub("[^0-9]", "", pressure_aggregated$timepoint)
  pressure_aggregated$matchid <- paste(pressure_aggregated$id,pressure_aggregated$timepoint,sep = "_")

  return(pressure_aggregated)
}

# ---- Import flow data (.edf / .tsv files) ----
import_flow_data <- function(path, nskip, verbose = FALSE) {

  tsv_filenames <- list.files(path, pattern = ".edf", full.names = TRUE)

  flow_df <- tsv_filenames %>%
    map_dfr(~ read_tsv(.x, skip = nskip, show_col_types = FALSE) %>% mutate(filename = basename(.x)))


  ## mutate flow df
  colnames(flow_df)
  flow_df <- flow_df %>%
    rename(time = "Local_Date_Time",
           flow = "F_SLF3S_1300_2443000233") %>%
    select(c(time, flow, filename))

  flow_df <- flow_df %>%
    group_by(filename) %>%  # Split by filename
    mutate(time = hms::as_hms(round(difftime(time, min(time), units = "secs")))) %>%
    ungroup()  # Combine back

  flow_aggregated <- flow_df %>%
    group_by(filename, time) %>%
    summarise(flow = mean(flow), .groups = "drop")

  flow_aggregated$id <- strsplit(flow_aggregated$filename, split = "_") %>% sapply("[[", 2)
  flow_aggregated$id <- gsub("[^0-9]", "", flow_aggregated$id)
  flow_aggregated$timepoint <- strsplit(flow_aggregated$filename, split = "_") %>% sapply("[[", 3)
  flow_aggregated$timepoint <- gsub("[^0-9]", "", flow_aggregated$timepoint)
  flow_aggregated$matchid <- paste(flow_aggregated$id,flow_aggregated$timepoint,sep = "_")



  return(flow_aggregated)
}

# ---- Merge flow and pressure data ----
merge_flow_pressure <- function(flow_df, pressure_df, verbose = FALSE) {
  df <- full_join(flow_df, pressure_df, by = c("matchid", "time")) %>%
    select(filename.x, filename.y, time, flow, psi, id.x, timepoint.x) %>%
    rename(
      id        = id.x,
      timepoint = timepoint.x
    ) %>%
    drop_na()

  if (verbose) message("Merged dataset with ", nrow(df), " rows.")
  return(df)
}

# ================================================================
