# =============================================================================
# Script 01: Data Cleaning and Preparation
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
# =============================================================================

library(tidyverse)
library(haven)      # for reading SPSS/Stata files
library(janitor)    # for clean_names()

# =============================================================================
# NOTE: GUiNZ data is accessed under a data access agreement.
# Raw data files are NOT included in this repository.
# This script documents the cleaning steps applied to the GUiNZ dataset.
# To replicate, request data access from the GUiNZ team at University of Auckland.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LOAD RAW DATA (placeholder — replace with your actual file paths)
# -----------------------------------------------------------------------------

# Example: data_raw <- read_sav("path/to/guinz_data.sav")
# Or:      data_raw <- read_csv("path/to/guinz_data.csv")

# For reproducibility, a simulated/anonymised version can be used:
# source("00_simulate_data.R")  # optional simulation script


# -----------------------------------------------------------------------------
# 2. SELECT RELEVANT VARIABLES
# -----------------------------------------------------------------------------

# Sleep variables across waves
sleep_vars <- c(
  "ChildID",
  # Age 2
  "SLP1_Hours_Y2CM",       # Night-time sleep duration (hrs) - Age 2
  "SLP2_Hours_Y2CM",       # Daytime nap duration (hrs) - Age 2
  # Age 4
  "SLP1_hr_m45CM",         # Night-time sleep - Age 4
  "SLP2n_hr_m45Cm",        # Nap duration - Age 4
  # Age 8
  "SLP1_HR_Y8CM",          # Night-time sleep - Age 8
  # Age 12
  "SLP1_HR_Y12CM",         # Night-time sleep - Age 12
  "SLP10_Y12CM",           # Mother-reported sleep quality
  "SLP16_Y12C"             # Child-reported sleep quality
)

# Mental wellbeing variables (Age 12)
wellbeing_vars <- c(
  "DEPRESS_SCORE_10_Y12C",  # CES-D-10 depression score (0-30)
  "PAS_T_SCORE2_Y12C",      # PROMIS anxiety T-score
  "CDRISC_SUM_Y12C"         # CD-RISC-10 resilience score (0-40)
)

# Sociodemographic and household variables
covariate_vars <- c(
  "EXT_PROETH_y12C",          # Primary ethnicity
  "TRANS_NB_CAT_Y12C",        # Gender identity
  "OECD_HH_INCOME_Y12M",      # Household income (OECD-equivalised)
  "HHST_Y12M",                # Household structure
  "HHST_BUBBLE_Y12M",         # Household bubble structure
  "AGG_FIS_CAT_Y12M",         # Food insecurity category
  "HHTENURE_Y12M",            # Housing tenure
  "CROWDINGGP_Y12M",          # Household crowding index
  "RURALITY_BIN_UR2018_Y12C", # Urban-rural classification
  "NZDEP2018_5_Y12C"          # NZ Deprivation Index (deciles 1-5)
)


# -----------------------------------------------------------------------------
# 3. DERIVE TOTAL SLEEP VARIABLE
# -----------------------------------------------------------------------------

clean_sleep <- function(df) {
  df %>%
    mutate(
      # Age 2: night + nap
      total_sleep_age2 = SLP1_Hours_Y2CM + coalesce(SLP2_Hours_Y2CM, 0),
      # Age 4: night + nap
      total_sleep_age4 = SLP1_hr_m45CM + coalesce(SLP2n_hr_m45Cm, 0),
      # Age 8: night only (naps not collected)
      total_sleep_age8 = SLP1_HR_Y8CM,
      # Age 12: night only
      total_sleep_age12 = SLP1_HR_Y12CM
    )
}


# -----------------------------------------------------------------------------
# 4. RECODE ETHNICITY
# -----------------------------------------------------------------------------

recode_ethnicity <- function(df) {
  df %>%
    mutate(
      ethnicity = case_when(
        EXT_PROETH_y12C == 1 ~ "Maori",
        EXT_PROETH_y12C == 2 ~ "Pasifika",
        EXT_PROETH_y12C == 3 ~ "Sole European",
        EXT_PROETH_y12C == 4 ~ "Asian",
        TRUE ~ "Other"
      ),
      ethnicity = factor(ethnicity,
                         levels = c("Sole European", "Maori", "Pasifika", "Asian", "Other"))
    )
}


# -----------------------------------------------------------------------------
# 5. RECODE SLEEP QUALITY VARIABLES
# -----------------------------------------------------------------------------

recode_sleep_quality <- function(df) {
  df %>%
    mutate(
      # Mother-reported: 1=No problem, 2=Small, 3=Moderate, 4=Large
      MotherSleepQ = factor(SLP10_Y12CM,
                            levels = 1:4,
                            labels = c("No problem", "Small problem",
                                       "Moderate problem", "Large problem"),
                            ordered = TRUE),
      # Child-reported: 1=Very well, 2=Fairly well, 3=Fairly badly, 4=Very badly
      ChildSleepQ = factor(SLP16_Y12C,
                           levels = 1:4,
                           labels = c("Very well", "Fairly well",
                                      "Fairly badly", "Very badly"),
                           ordered = TRUE)
    )
}


# -----------------------------------------------------------------------------
# 6. RECODE HOUSEHOLD / SOCIOECONOMIC VARIABLES
# -----------------------------------------------------------------------------

recode_household <- function(df) {
  df %>%
    mutate(
      # Food insecurity
      food_insecurity = factor(AGG_FIS_CAT_Y12M,
                               levels = 1:3,
                               labels = c("Severe", "Moderate", "Secure")),
      # Housing tenure
      housing_tenure = factor(HHTENURE_Y12M,
                              levels = 1:4,
                              labels = c("Own", "Private rental",
                                         "Public housing", "Other")),
      # Crowding
      crowding = factor(CROWDINGGP_Y12M,
                        levels = 1:3,
                        labels = c("Low", "Medium", "High")),
      # Rurality
      rurality = factor(RURALITY_BIN_UR2018_Y12C,
                        levels = c(0, 1),
                        labels = c("Urban", "Rural")),
      # NZ Deprivation Index (as factor for regression)
      NZDEP = factor(NZDEP2018_5_Y12C,
                     levels = 1:5,
                     labels = paste0("NZDEP", 1:5))
    )
}


# -----------------------------------------------------------------------------
# 7. RESHAPE TO LONG FORMAT FOR LONGITUDINAL ANALYSIS
# -----------------------------------------------------------------------------

reshape_long <- function(df) {
  df %>%
    select(ChildID, ethnicity, total_sleep_age2, total_sleep_age4,
           total_sleep_age8, total_sleep_age12,
           all_of(covariate_vars)) %>%
    pivot_longer(
      cols = starts_with("total_sleep_age"),
      names_to = "age",
      names_prefix = "total_sleep_age",
      values_to = "total_sleep"
    ) %>%
    mutate(age = as.numeric(age))
}


# -----------------------------------------------------------------------------
# 8. HANDLE MISSING DATA
# -----------------------------------------------------------------------------

flag_missing <- function(df) {
  cat("Missing value summary:\n")
  df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
    filter(n_missing > 0) %>%
    arrange(desc(n_missing)) %>%
    print()
  return(df)
}


# -----------------------------------------------------------------------------
# 9. APPLY CLEANING PIPELINE
# -----------------------------------------------------------------------------

# data_clean <- data_raw %>%
#   select(all_of(c(sleep_vars, wellbeing_vars, covariate_vars))) %>%
#   clean_names() %>%
#   clean_sleep() %>%
#   recode_ethnicity() %>%
#   recode_sleep_quality() %>%
#   recode_household() %>%
#   flag_missing()

# data_long <- reshape_long(data_clean)

# -----------------------------------------------------------------------------
# 10. SAVE CLEANED DATA
# -----------------------------------------------------------------------------

# saveRDS(data_clean, "data/processed/data_clean.rds")
# saveRDS(data_long,  "data/processed/data_long.rds")

cat("Script 01: Data cleaning complete.\n")
