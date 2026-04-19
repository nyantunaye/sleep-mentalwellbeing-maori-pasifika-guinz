# =============================================================================
# Script 04: Predictive Modelling — Random Forest
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
# =============================================================================

library(tidyverse)
library(randomForest)
library(ggplot2)
library(caret)         # for cross-validation

# Load cleaned data
# data_clean <- readRDS("data/processed/data_clean.rds")
# data_y12   <- data_clean %>% filter(!is.na(total_sleep_age12))


# =============================================================================
# HELPER: Prepare modelling dataset
# =============================================================================

prepare_rf_data <- function(data_clean, outcome_var) {
  predictors <- c(
    "ethnicity", "NZDEP2018_5_Y12C", "RURALITY_BIN_UR2018_Y12C",
    "TRANS_NB_CAT_Y12C",
    "DEPRESS_SCORE_10_Y12C", "PAS_T_SCORE2_Y12C", "CDRISC_SUM_Y12C"
  )

  data_clean %>%
    select(all_of(c(outcome_var, predictors))) %>%
    rename(outcome = all_of(outcome_var)) %>%
    mutate(
      ethnicity = factor(ethnicity),
      NZDEP     = factor(NZDEP2018_5_Y12C),
      Rurality  = factor(RURALITY_BIN_UR2018_Y12C,
                         levels = c(0, 1), labels = c("Urban", "Rural")),
      TransNB   = factor(TRANS_NB_CAT_Y12C)
    ) %>%
    select(-NZDEP2018_5_Y12C, -RURALITY_BIN_UR2018_Y12C,
           -TRANS_NB_CAT_Y12C) %>%
    drop_na()
}


# =============================================================================
# FIGURE 7: Random Forest — Predicting Sleep Duration at Age 12
# =============================================================================

run_rf_sleep_duration <- function(data_clean) {
  cat("\n--- Random Forest: Predicting Sleep Duration (Age 12) ---\n")

  rf_data <- prepare_rf_data(data_clean, "total_sleep_age12")

  set.seed(42)
  rf_sleep <- randomForest(
    outcome ~ .,
    data       = rf_data,
    ntree      = 500,
    mtry       = 3,
    importance = TRUE
  )

  print(rf_sleep)

  cat("\nVariable Importance:\n")
  imp <- importance(rf_sleep)
  print(imp)

  # Plot variable importance
  varImpPlot(rf_sleep,
             main = "Random Forest Variable Importance for Sleep Duration (Age 12)")

  # ggplot version
  imp_df <- as.data.frame(imp) %>%
    rownames_to_column("Variable") %>%
    arrange(desc(`%IncMSE`))

  p_imp <- ggplot(imp_df, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
    geom_point(size = 3, colour = "steelblue") +
    geom_segment(aes(xend = Variable, y = 0, yend = `%IncMSE`),
                 colour = "steelblue") +
    coord_flip() +
    labs(
      title = "Random Forest Variable Importance for Sleep Duration (Age 12)",
      x = NULL, y = "%IncMSE"
    ) +
    theme_minimal(base_size = 12)

  # ggsave("outputs/figures/fig07_rf_sleep_duration.png", p_imp,
  #        width = 8, height = 5)

  return(rf_sleep)
}


# =============================================================================
# FIGURE 16: Random Forest — Mother-Reported Sleep Quality
# =============================================================================

run_rf_mother_sleep <- function(data_clean) {
  cat("\n--- Random Forest: Mother-Reported Sleep Quality ---\n")

  rf_data <- data_clean %>%
    select(
      MotherSleepQ,
      ethnicity, NZDEP2018_5_Y12C, RURALITY_BIN_UR2018_Y12C,
      TRANS_NB_CAT_Y12C,
      DEPRESS_SCORE_10_Y12C, PAS_T_SCORE2_Y12C, CDRISC_SUM_Y12C
    ) %>%
    mutate(
      MotherSleepQ = factor(MotherSleepQ),
      ethnicity    = factor(ethnicity),
      NZDEP        = factor(NZDEP2018_5_Y12C),
      Rurality     = factor(RURALITY_BIN_UR2018_Y12C),
      TransNB      = factor(TRANS_NB_CAT_Y12C)
    ) %>%
    select(-NZDEP2018_5_Y12C, -RURALITY_BIN_UR2018_Y12C,
           -TRANS_NB_CAT_Y12C) %>%
    drop_na()

  set.seed(42)
  rf_mother <- randomForest(
    MotherSleepQ ~ .,
    data       = rf_data,
    ntree      = 500,
    mtry       = 3,
    importance = TRUE
  )

  print(rf_mother)
  cat(sprintf("OOB Error Rate: %.2f%%\n", rf_mother$err.rate[500, "OOB"] * 100))

  imp <- importance(rf_mother)
  cat("\nVariable Importance:\n")
  print(imp)

  varImpPlot(rf_mother,
             main = "Random Forest Variable Importance for Mother-Reported Sleep Quality (Age 12)")

  return(rf_mother)
}


# =============================================================================
# FIGURE 18: Random Forest — Child-Reported Sleep Quality
# =============================================================================

run_rf_child_sleep <- function(data_clean) {
  cat("\n--- Random Forest: Child-Reported Sleep Quality ---\n")

  rf_data <- data_clean %>%
    select(
      ChildSleepQ,
      ethnicity, NZDEP2018_5_Y12C, RURALITY_BIN_UR2018_Y12C,
      TRANS_NB_CAT_Y12C,
      DEPRESS_SCORE_10_Y12C, PAS_T_SCORE2_Y12C, CDRISC_SUM_Y12C
    ) %>%
    mutate(
      ChildSleepQ = factor(ChildSleepQ),
      ethnicity   = factor(ethnicity),
      NZDEP       = factor(NZDEP2018_5_Y12C),
      Rurality    = factor(RURALITY_BIN_UR2018_Y12C),
      TransNB     = factor(TRANS_NB_CAT_Y12C)
    ) %>%
    select(-NZDEP2018_5_Y12C, -RURALITY_BIN_UR2018_Y12C,
           -TRANS_NB_CAT_Y12C) %>%
    drop_na()

  set.seed(42)
  rf_child <- randomForest(
    ChildSleepQ ~ .,
    data       = rf_data,
    ntree      = 500,
    mtry       = 3,
    importance = TRUE
  )

  print(rf_child)
  cat(sprintf("OOB Error Rate: %.2f%%\n", rf_child$err.rate[500, "OOB"] * 100))

  imp <- importance(rf_child)
  cat("\nVariable Importance:\n")
  print(imp)

  varImpPlot(rf_child,
             main = "Random Forest Variable Importance for Child-Reported Sleep Quality (Age 12)")

  return(rf_child)
}


# =============================================================================
# CROSS-VALIDATION (10-fold) — for sleep duration regression
# =============================================================================

run_cross_validation <- function(data_clean) {
  cat("\n--- 10-fold Cross-Validation: Sleep Duration Model ---\n")

  rf_data <- prepare_rf_data(data_clean, "total_sleep_age12")

  set.seed(42)
  train_control <- trainControl(method = "cv", number = 10)

  cv_model <- train(
    outcome ~ .,
    data      = rf_data,
    method    = "rf",
    trControl = train_control,
    ntree     = 500
  )

  print(cv_model)
  cat("\nCV Results:\n")
  print(cv_model$results)

  return(cv_model)
}


# =============================================================================
# RUN ALL (uncomment as needed)
# =============================================================================

# rf_sleep  <- run_rf_sleep_duration(data_clean)
# rf_mother <- run_rf_mother_sleep(data_clean)
# rf_child  <- run_rf_child_sleep(data_clean)
# cv_model  <- run_cross_validation(data_clean)

cat("Script 04: Random forest modelling complete.\n")
