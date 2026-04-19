# =============================================================================
# Script 00: Master Run Script
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
# =============================================================================
#
# Run this script to execute the full analysis pipeline in order.
#
# IMPORTANT: GUiNZ data is not included in this repository.
# You must have an approved data access agreement with the GUiNZ team
# and place the data files in the data/raw/ directory before running.
#
# Pipeline order:
#   01_data_cleaning.R       — load, clean, recode, reshape
#   02_descriptive_analysis.R — summary stats, density plots, bar charts
#   03_inferential_models.R   — ANOVA, regression, mixed-effects, ordinal logistic
#   04_random_forest.R        — random forest regression and classification
# =============================================================================

# Install required packages if not already installed
required_packages <- c(
  "tidyverse", "haven", "janitor",
  "ggplot2", "scales", "patchwork",
  "car", "lme4", "lmerTest", "performance", "broom", "broom.mixed",
  "randomForest", "caret",
  "psych", "MASS"
)

new_packages <- required_packages[
  !(required_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages)
}

# Load core packages
library(tidyverse)

# Set global options
options(scipen = 999)
set.seed(42)

# Create output directories
dir.create("data/processed",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# RUN PIPELINE
# =============================================================================

cat("=== Starting Analysis Pipeline ===\n\n")

cat("Step 1: Data Cleaning...\n")
source("code/01_data_cleaning.R")

cat("\nStep 2: Descriptive Analysis...\n")
source("code/02_descriptive_analysis.R")

cat("\nStep 3: Inferential Modelling...\n")
source("code/03_inferential_models.R")

cat("\nStep 4: Random Forest Modelling...\n")
source("code/04_random_forest.R")

cat("\n=== Pipeline Complete ===\n")
cat("Check outputs/figures/ and outputs/tables/ for results.\n")
