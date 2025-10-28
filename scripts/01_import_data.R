# ================================================================
# 01_import_data.R
# Import and clean flow and pressure data from catheter experiments
# ================================================================

source("scripts/00_setup.R")
source("scripts/utils_functions.R")  # will contain reusable helpers

# ---- Define paths ----
data_raw_dir <- here::here("data_raw")
metadata_dir <- here::here("metadata")  # optional if metadata later used

# ---- Import pressure data (.csv) ----
message("📥 Importing pressure data...")
pressure_df <- suppressWarnings(
  suppressMessages(import_pressure_data(data_raw_dir, nskip = 3))
)
cat("✅ Imported pressure data with", nrow(pressure_df), "rows.\n")

# ---- Import flow data (.edf / .tsv) ----
message("📥 Importing flow data...")
flow_df <- import_flow_data(data_raw_dir, nskip = 10)
cat("✅ Imported flow data with", nrow(flow_df), "rows.\n")

# ---- Merge data ----
message("🔗 Merging flow and pressure data...")
merged_df <- merge_flow_pressure(flow_df, pressure_df)
cat("✅ Merged dataset with", nrow(merged_df), "rows and",
    length(unique(merged_df$id)), "unique IDs.\n")

# ---- Save merged dataset ----
data_path <- here::here("data", "merged_df.rds")
saveRDS(merged_df, data_path)
cat("💾 Saved merged data to:", data_path, "\n")

# Clean up environment
rm(pressure_df, flow_df, merged_df)
gc()
cat("All temporary data removed from environment.\n")



# ================================================================
