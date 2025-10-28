# ================================================================
# 00_setup.R
# Project setup script for TizzuLab projects
# Author: Joel Zindel
# ================================================================

# ---- Project paths ----
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)
cat("📁 Project root:", here::here(), "\n")

# ---- Package management with renv ----
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
library(renv)

# Initialize renv if not yet initialized
if (!file.exists("renv.lock")) {
  message("🆕 Initializing new renv environment...")
  renv::init(bare = TRUE)
} else {
  # If renv.lock exists, ensure environment matches it
  status <- renv::status()
  if (!status$synchronized) {
    message("📦 Restoring packages from renv.lock to match project environment...")
    renv::restore(prompt = FALSE)
  } else {
    message("✅ renv environment already up to date.")
  }
}

# ---- Core packages ----
packages <- c(
  "tidyverse",
  "readxl",
  "here",
  "janitor",
  "ggpubr",
  "patchwork"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# ---- Global options ----
options(stringsAsFactors = FALSE)
theme_set(theme_bw())
set.seed(1234)

# ---- Folder setup ----
# Define key folders for the TizzuLab project structure
data_raw_dir <- here("data_raw")     # raw, unprocessed data
data_dir     <- here("data")         # processed / cleaned data
metadata_dir <- here("metadata")     # sample metadata
scripts_dir  <- here("scripts")      # R scripts
results_dir  <- here("results")      # plots, tables
figures_dir  <- here("figures")      # exported figures

dirs <- c(data_raw_dir, data_dir, metadata_dir, scripts_dir, results_dir, figures_dir)

for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

cat("Project folders verified/created:\n")
print(dirs)

# ---- Environment logging ----
log_file <- file.path(here::here(), "session_info.log")

get_git_commit <- function() {
  commit <- tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) NA
  )
  if (length(commit) == 0) commit <- NA
  commit
}

session_info <- list(
  timestamp   = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  R_version   = R.version.string,
  git_commit  = get_git_commit(),
  project_dir = here::here(),
  packages    = packages
)

log_entry <- paste0(
  "\n---- Session started: ", session_info$timestamp, " ----\n",
  "R version: ", session_info$R_version, "\n",
  "Git commit: ", session_info$git_commit, "\n",
  "Project: ", session_info$project_dir, "\n",
  "Loaded packages: ", paste(session_info$packages, collapse = ", "), "\n"
)

cat(log_entry, file = log_file, append = TRUE)
cat("🧾 Session info logged to:", log_file, "\n")

cat("✅ Environment ready. Packages loaded:\n")
print(packages)
cat("📂 Folders verified/created.\n")
# ================================================================
