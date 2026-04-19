# =============================================================================
# Script 03: Inferential Modelling
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
# =============================================================================

library(tidyverse)
library(car)          # for Levene's test and VIF
library(lme4)         # for mixed-effects models
library(lmerTest)     # p-values for lme4
library(performance)  # model diagnostics
library(broom)        # tidy model output
library(broom.mixed)  # tidy for lme4

# Load cleaned data
# data_clean <- readRDS("data/processed/data_clean.rds")
# data_long  <- readRDS("data/processed/data_long.rds")


# =============================================================================
# SECTION 1: AGE EFFECTS ON SLEEP DURATION
# ANOVA + Tukey Post-Hoc + Levene's Test (Table 2)
# =============================================================================

run_age_anova <- function(data_long) {
  cat("\n--- ANOVA: Age differences in total sleep duration ---\n")

  anova_result <- aov(total_sleep ~ factor(age), data = data_long)
  print(summary(anova_result))

  cat("\n--- Tukey HSD Post-Hoc ---\n")
  print(TukeyHSD(anova_result))

  cat("\n--- Levene's Test for Homogeneity of Variance ---\n")
  print(leveneTest(total_sleep ~ factor(age), data = data_long))

  return(anova_result)
}


# =============================================================================
# SECTION 2: ETHNIC DIFFERENCES IN SLEEP DURATION
# ANOVA + Levene's Test (Table 5)
# =============================================================================

run_ethnicity_anova <- function(data_long) {
  cat("\n--- ANOVA: Ethnic differences in total sleep duration ---\n")

  anova_eth <- aov(total_sleep ~ ethnicity, data = data_long)
  print(summary(anova_eth))

  cat("\n--- Levene's Test ---\n")
  print(leveneTest(total_sleep ~ ethnicity, data = data_long))

  return(anova_eth)
}


# =============================================================================
# SECTION 3: AGE × ETHNICITY INTERACTION (Table 4)
# =============================================================================

run_age_ethnicity_interaction <- function(data_long) {
  cat("\n--- Linear Regression: Age × Ethnicity interaction ---\n")

  # Relevel so Sole European is reference
  data_long <- data_long %>%
    mutate(ethnicity = relevel(factor(ethnicity), ref = "Sole European"),
           age       = factor(age))

  model_interact <- lm(total_sleep ~ age * ethnicity, data = data_long)
  print(summary(model_interact))

  cat("\n--- VIF Check ---\n")
  # Note: vif on interaction models requires care
  tryCatch(print(vif(model_interact)), error = function(e) cat("VIF skipped for interaction model\n"))

  return(model_interact)
}


# =============================================================================
# SECTION 4: SLEEP VS MENTAL WELLBEING AT AGE 12
# Tables 6, 7, 8
# =============================================================================

run_sleep_wellbeing_regressions <- function(data_y12) {
  # Prepare age-12 data subset
  data_y12 <- data_y12 %>%
    rename(
      OverallSleep    = total_sleep_age12,
      depression      = DEPRESS_SCORE_10_Y12C,
      anxiety         = PAS_T_SCORE2_Y12C,
      resilience_score = CDRISC_SUM_Y12C
    )

  # Table 6: Sleep vs Depression
  cat("\n--- Table 6: Sleep vs Depression ---\n")
  m_dep <- lm(depression ~ OverallSleep, data = data_y12)
  print(summary(m_dep))

  # Table 7: Sleep vs Anxiety
  cat("\n--- Table 7: Sleep vs Anxiety ---\n")
  m_anx <- lm(anxiety ~ OverallSleep, data = data_y12)
  print(summary(m_anx))

  # Table 8: Sleep vs Resilience
  cat("\n--- Table 8: Sleep vs Resilience ---\n")
  m_res <- lm(resilience_score ~ OverallSleep, data = data_y12)
  print(summary(m_res))

  return(list(depression = m_dep, anxiety = m_anx, resilience = m_res))
}


# =============================================================================
# SECTION 5: EARLIER SLEEP PREDICTING DEPRESSION AT AGE 12
# Table 9
# =============================================================================

run_early_sleep_depression <- function(data_clean) {
  data_y12 <- data_clean %>%
    rename(depression = DEPRESS_SCORE_10_Y12C)

  ages <- c(2, 4, 8)
  sleep_cols <- c("total_sleep_age2", "total_sleep_age4", "total_sleep_age8")

  for (i in seq_along(ages)) {
    cat(sprintf("\n--- Model %d: Sleep Age %d → Depression Age 12 ---\n", i, ages[i]))
    formula_str <- paste("depression ~", sleep_cols[i])
    m <- lm(as.formula(formula_str), data = data_y12)
    print(summary(m))
  }
}


# =============================================================================
# SECTION 6: MULTIVARIATE REGRESSION — FACTORS PREDICTING SLEEP AT AGE 12
# Table 10 / Table 11
# =============================================================================

run_multivariate_sleep <- function(data_y12) {
  cat("\n--- Table 10: Multivariate regression — sleep duration predictors ---\n")

  data_y12 <- data_y12 %>%
    mutate(
      ethnicity = relevel(factor(ethnicity), ref = "Sole European"),
      NZDEP     = factor(NZDEP2018_5_Y12C)
    )

  # Model with psychological wellbeing variables
  m_multi <- lm(
    total_sleep_age12 ~ relevel(ethnicity, ref = "Sole European") +
      NZDEP + rurality + TransNB +
      DEPRESS_SCORE_10_Y12C + PAS_T_SCORE2_Y12C + resilience_score,
    data = data_y12
  )
  print(summary(m_multi))

  cat("\n--- VIF ---\n")
  print(vif(m_multi))

  # Extended model with household variables (Table 11)
  cat("\n--- Table 11: Extended model with household variables ---\n")
  m_household <- lm(
    total_sleep ~ ethnicity + OECD_HH_INCOME_Y12M +
      HHST_BUBBLE_Y12M + HHST_Y12M + AGG_FIS_CAT_Y12M +
      HHTENURE_Y12M + CROWDINGGP_Y12M +
      RURALITY_BIN_UR2018_Y12C + age,
    data = data_y12
  )
  print(summary(m_household))

  return(list(m_multi = m_multi, m_household = m_household))
}


# =============================================================================
# SECTION 7: MIXED-EFFECTS LONGITUDINAL MODEL
# Table 12
# =============================================================================

run_mixed_effects <- function(data_long) {
  cat("\n--- Table 12: Linear Mixed-Effects Model ---\n")

  data_long <- data_long %>%
    mutate(
      ethnicity = relevel(factor(ethnicity), ref = "Sole European"),
      age_c     = age - 2   # centre age at 2 years
    )

  model_mixed <- lmer(
    total_sleep ~ ethnicity + OECD_HH_INCOME_Y12M +
      HHST_BUBBLE_Y12M + HHST_Y12M + AGG_FIS_CAT_Y12M +
      HHTENURE_Y12M + CROWDINGGP_Y12M +
      RURALITY_BIN_UR2018_Y12C + age + (age | ChildID),
    data = data_long,
    REML = TRUE,
    control = lmerControl(optimizer = "nloptwrap")
  )

  print(summary(model_mixed))

  cat("\n--- VIF (Mixed Model) ---\n")
  print(check_collinearity(model_mixed))

  return(model_mixed)
}


# =============================================================================
# SECTION 8: CHI-SQUARE TEST — AGE vs SLEEP PROBLEM SEVERITY
# Table 15
# =============================================================================

run_chisq_sleep_problems <- function(data_long) {
  cat("\n--- Table 15: Chi-Square Test — Age vs Sleep Problem Severity ---\n")

  problem_table <- table(data_long$age, data_long$sleep_problem)
  print(problem_table)

  chisq_result <- chisq.test(problem_table)
  print(chisq_result)

  return(chisq_result)
}


# =============================================================================
# SECTION 9: COHEN'S KAPPA — INTER-RATER AGREEMENT
# Table 16
# =============================================================================

run_cohens_kappa <- function(data_clean) {
  library(psych)

  cat("\n--- Table 16: Cohen's Kappa (Mother vs Child sleep quality) ---\n")

  rater_data <- data_clean %>%
    filter(!is.na(MotherSleepQ), !is.na(ChildSleepQ)) %>%
    select(MotherSleepQ, ChildSleepQ) %>%
    mutate(across(everything(), as.numeric))

  kappa_result <- cohen.kappa(rater_data, w = "quadratic")
  print(kappa_result)

  return(kappa_result)
}


# =============================================================================
# SECTION 10: ORDINAL LOGISTIC REGRESSION — SLEEP QUALITY
# Tables 17 & 18
# =============================================================================

run_ordinal_regression <- function(data_y12) {
  library(MASS)

  data_y12 <- data_y12 %>%
    mutate(
      ethnicity    = relevel(factor(ethnicity), ref = "Sole European"),
      NZDEP        = factor(NZDEP2018_5_Y12C),
      MotherSleepQ = factor(MotherSleepQ, ordered = TRUE),
      ChildSleepQ  = factor(ChildSleepQ, ordered = TRUE)
    )

  # Table 17: Mother-reported sleep quality
  cat("\n--- Table 17: Ordinal Logistic — Mother-reported Sleep Quality ---\n")
  m_mother <- polr(
    MotherSleepQ ~ relevel(ethnicity, ref = "Sole European") +
      NZDEP + rurality + TransNB +
      DEPRESS_SCORE_10_Y12C + PAS_T_SCORE2_Y12C + resilience_score,
    data = data_y12, Hess = TRUE
  )
  print(summary(m_mother))

  # Odds Ratios
  or_mother <- exp(cbind(OR = coef(m_mother),
                         confint(m_mother)))
  cat("\nOdds Ratios (Mother):\n")
  print(round(or_mother, 3))

  # Table 18: Child-reported sleep quality
  cat("\n--- Table 18: Ordinal Logistic — Child-reported Sleep Quality ---\n")
  m_child <- polr(
    ChildSleepQ ~ relevel(ethnicity, ref = "Sole European") +
      NZDEP + rurality + TransNB +
      DEPRESS_SCORE_10_Y12C + PAS_T_SCORE2_Y12C + resilience_score,
    data = data_y12, Hess = TRUE
  )
  print(summary(m_child))

  or_child <- exp(cbind(OR = coef(m_child),
                        confint(m_child)))
  cat("\nOdds Ratios (Child):\n")
  print(round(or_child, 3))

  return(list(mother = m_mother, child = m_child))
}


# =============================================================================
# SECTION 11: MODEL DIAGNOSTICS
# =============================================================================

run_diagnostics <- function(model, model_name = "Model") {
  cat(sprintf("\n--- Diagnostics: %s ---\n", model_name))

  par(mfrow = c(2, 2))
  plot(model)
  par(mfrow = c(1, 1))

  cat("VIF:\n")
  tryCatch(print(vif(model)), error = function(e) cat("VIF not available\n"))
}


# =============================================================================
# RUN ANALYSIS (uncomment as needed)
# =============================================================================

# anova_age    <- run_age_anova(data_long)
# anova_eth    <- run_ethnicity_anova(data_long)
# model_inter  <- run_age_ethnicity_interaction(data_long)
# models_wb    <- run_sleep_wellbeing_regressions(data_clean)
# run_early_sleep_depression(data_clean)
# models_multi <- run_multivariate_sleep(data_clean)
# model_mixed  <- run_mixed_effects(data_long)
# chisq_res    <- run_chisq_sleep_problems(data_long)
# kappa_res    <- run_cohens_kappa(data_clean)
# models_ord   <- run_ordinal_regression(data_clean)

cat("Script 03: Inferential modelling complete.\n")
