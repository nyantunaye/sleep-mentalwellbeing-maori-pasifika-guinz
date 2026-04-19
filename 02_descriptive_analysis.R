# =============================================================================
# Script 02: Descriptive Analysis and Visualisations
# Project: Sleep Patterns and Mental Wellbeing in Māori and Pasifika Youth
# Data: Growing Up in New Zealand (GUiNZ) Study
# Author: Nyan Tun Aye | AUT Master of Analytics
# =============================================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)   # combining plots

# Load cleaned data
# data_clean <- readRDS("data/processed/data_clean.rds")
# data_long  <- readRDS("data/processed/data_long.rds")


# =============================================================================
# FIGURE 1: Density Plots of Daytime Nap Duration (Ages 2 and 4)
# =============================================================================

plot_nap_density <- function(data_clean) {
  nap_data <- data_clean %>%
    select(nap_age2 = SLP2_Hours_Y2CM,
           nap_age4 = SLP2n_hr_m45Cm) %>%
    pivot_longer(everything(), names_to = "age_group", values_to = "nap_hours") %>%
    mutate(age_group = recode(age_group,
                              "nap_age2" = "Age 2",
                              "nap_age4" = "Age 4")) %>%
    filter(!is.na(nap_hours), nap_hours > 0)

  ggplot(nap_data, aes(x = nap_hours, colour = age_group)) +
    geom_density(linewidth = 1.2) +
    scale_colour_manual(values = c("Age 2" = "steelblue", "Age 4" = "orange")) +
    labs(
      title = "Combined Density Plot of Daytime Nap Sleep Duration (Ages 2 and 4)",
      x = "Sleep Duration (Hours)",
      y = "Density",
      colour = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
}


# =============================================================================
# FIGURE 2: Total Sleep Duration Across Four Developmental Ages
# =============================================================================

plot_total_sleep_density <- function(data_long) {
  data_long %>%
    filter(!is.na(total_sleep)) %>%
    mutate(age_label = paste("Age", age)) %>%
    ggplot(aes(x = total_sleep, colour = age_label)) +
    geom_density(linewidth = 1.1) +
    scale_colour_manual(values = c(
      "Age 2"  = "steelblue",
      "Age 4"  = "orange",
      "Age 8"  = "green3",
      "Age 12" = "purple"
    )) +
    labs(
      title = "Combined Density Plot of Total Sleep Duration (Ages 2, 4, 8 and 12)",
      x = "Sleep Duration (Hours)",
      y = "Density",
      colour = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
}


# =============================================================================
# FIGURE 3: Average Sleep Duration Across Childhood (Ages 2-12)
# =============================================================================

plot_avg_sleep <- function(data_long) {
  sleep_means <- data_long %>%
    filter(!is.na(total_sleep)) %>%
    group_by(age) %>%
    summarise(
      mean_sleep = mean(total_sleep, na.rm = TRUE),
      sd_sleep   = sd(total_sleep, na.rm = TRUE),
      n          = n(),
      se         = sd_sleep / sqrt(n),
      .groups = "drop"
    )

  print(sleep_means)

  ggplot(sleep_means, aes(x = age, y = mean_sleep)) +
    geom_line(colour = "steelblue", linewidth = 1.2) +
    geom_point(colour = "steelblue", size = 4) +
    geom_errorbar(aes(ymin = mean_sleep - 1.96 * se,
                      ymax = mean_sleep + 1.96 * se),
                  width = 0.3, colour = "steelblue") +
    geom_text(aes(label = paste0(round(mean_sleep, 1), "h")),
              vjust = -1.2, size = 4) +
    scale_x_continuous(breaks = c(2, 4, 8, 12)) +
    labs(
      title = "Average Sleep Duration Across Ages",
      x = "Age (years)",
      y = "Mean Sleep Duration (hours)"
    ) +
    theme_minimal(base_size = 13)
}


# =============================================================================
# FIGURE 4: Mean Sleep Duration by Age and Ethnicity
# =============================================================================

plot_sleep_by_ethnicity <- function(data_long) {
  eth_means <- data_long %>%
    filter(!is.na(total_sleep),
           ethnicity %in% c("Sole European", "Maori", "Pasifika")) %>%
    group_by(age, ethnicity) %>%
    summarise(
      mean_sleep = mean(total_sleep, na.rm = TRUE),
      se         = sd(total_sleep, na.rm = TRUE) / sqrt(n()),
      .groups    = "drop"
    )

  ggplot(eth_means, aes(x = age, y = mean_sleep,
                        colour = ethnicity, group = ethnicity)) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_sleep - 1.96 * se,
                      ymax = mean_sleep + 1.96 * se),
                  width = 0.3) +
    geom_text(aes(label = round(mean_sleep, 2)),
              vjust = -1, size = 3) +
    scale_colour_manual(values = c(
      "Sole European" = "steelblue",
      "Maori"         = "red3",
      "Pasifika"      = "green4"
    )) +
    scale_x_continuous(breaks = c(2, 4, 8, 12)) +
    labs(
      title = "Mean Total Sleep Duration (hours) by Age and Ethnicity\nwith 95% Confidence Intervals (GUiNZ Data)",
      x = "Child Age (years)",
      y = "Mean Total Sleep Duration (hours)",
      colour = "Ethnicity"
    ) +
    theme_minimal(base_size = 13)
}


# =============================================================================
# FIGURE 5: Weekday vs Weekend Sleep at Age 12
# =============================================================================

plot_weekday_weekend <- function(data_clean) {
  # Assumes variables: SLP_WKDAY_Y12CM, SLP_WKEND_Y12CM
  data_clean %>%
    filter(!is.na(SLP_WKDAY_Y12CM), !is.na(SLP_WKEND_Y12CM)) %>%
    ggplot(aes(x = SLP_WKDAY_Y12CM, y = SLP_WKEND_Y12CM)) +
    geom_point(alpha = 0.3, colour = "steelblue", size = 1) +
    geom_smooth(method = "lm", colour = "red", se = FALSE) +
    labs(
      title = "Weekday vs Weekend Sleep (Mother-reported)",
      x = "Weekday Sleep Duration (hours)",
      y = "Weekend Sleep Duration (hours)"
    ) +
    theme_minimal(base_size = 13)
}


# =============================================================================
# FIGURE 10: Sleep Problem Severity Distribution Across Ages
# =============================================================================

plot_sleep_problems <- function(data_clean) {
  # Reshape sleep problem variables across ages
  prob_data <- data_clean %>%
    select(
      age2  = SLP_PROB_Y2CM,    # adjust variable names as needed
      age4  = SLP_PROB_Y4CM,
      age8  = SLP_PROB_Y8CM,
      age12 = SLP10_Y12CM
    ) %>%
    pivot_longer(everything(), names_to = "age_grp", values_to = "severity") %>%
    mutate(
      age_grp  = recode(age_grp,
                        age2 = "2", age4 = "4", age8 = "8", age12 = "12"),
      severity = factor(severity, levels = 1:4,
                        labels = c("No problem", "Small", "Moderate", "Large"))
    ) %>%
    filter(!is.na(severity)) %>%
    count(age_grp, severity) %>%
    group_by(age_grp) %>%
    mutate(pct = n / sum(n) * 100)

  ggplot(prob_data, aes(x = severity, y = n, fill = severity)) +
    geom_col() +
    facet_wrap(~ age_grp, nrow = 2, labeller = label_both) +
    scale_fill_manual(values = c(
      "No problem" = "#2ecc71",
      "Small"      = "#e67e22",
      "Moderate"   = "#9b59b6",
      "Large"      = "#e91e63"
    )) +
    labs(
      title = "Distribution of Sleep Problems by Age Groups (2, 4, 8, and 12 Years)",
      x = "Sleep Problem Severity",
      y = "Number of Children",
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}


# =============================================================================
# FIGURE 11: Sleep Problems by Age and Ethnicity (Stacked Bar)
# =============================================================================

plot_sleep_problems_ethnicity <- function(data_long) {
  # Requires sleep_problem variable merged into long data
  data_long %>%
    filter(!is.na(sleep_problem),
           ethnicity %in% c("Maori", "Pasifika", "Sole European")) %>%
    count(age, ethnicity, sleep_problem) %>%
    group_by(age, ethnicity) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = factor(age), y = pct, fill = sleep_problem)) +
    geom_col(position = "stack") +
    geom_text(aes(label = paste0(round(pct, 1), "%")),
              position = position_stack(vjust = 0.5), size = 2.8) +
    facet_wrap(~ ethnicity) +
    scale_fill_manual(values = c(
      "No problem" = "#2ecc71",
      "Small"      = "#e67e22",
      "Moderate"   = "#9b59b6",
      "Large"      = "#e91e63"
    )) +
    labs(
      title = "Sleep Problems by Age and Ethnicity (Mother-reported)",
      x = "Age (years)",
      y = "Percentage of Children (%)",
      fill = "Sleep Problem"
    ) +
    theme_minimal(base_size = 12)
}


# =============================================================================
# FIGURES 12 & 13: Sleep Quality Distribution (Mother vs Child)
# =============================================================================

plot_sleep_quality_dist <- function(data_clean) {
  p1 <- data_clean %>%
    filter(!is.na(MotherSleepQ)) %>%
    count(MotherSleepQ) %>%
    ggplot(aes(x = MotherSleepQ, y = n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Mother-reported Sleep Quality (Age 12)",
         x = NULL, y = "Count") +
    theme_minimal()

  p2 <- data_clean %>%
    filter(!is.na(ChildSleepQ)) %>%
    count(ChildSleepQ) %>%
    ggplot(aes(x = ChildSleepQ, y = n)) +
    geom_col(fill = "orange") +
    labs(title = "Child-reported Sleep Quality (Age 12)",
         x = NULL, y = "Count") +
    theme_minimal()

  p1 / p2
}


# =============================================================================
# FIGURE 14: Cross-tabulation Heatmap (Mother vs Child Sleep Quality)
# =============================================================================

plot_rater_heatmap <- function(data_clean) {
  data_clean %>%
    filter(!is.na(MotherSleepQ), !is.na(ChildSleepQ)) %>%
    count(MotherSleepQ, ChildSleepQ) %>%
    ggplot(aes(x = ChildSleepQ, y = MotherSleepQ, fill = n)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = n), size = 4) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = "Mother vs Child Reported Sleep Quality",
      x = "Child Sleep Quality",
      y = "Mother Sleep Quality",
      fill = "Count"
    ) +
    theme_minimal(base_size = 12)
}


# =============================================================================
# RUN ALL PLOTS AND SAVE
# =============================================================================

# Uncomment to run:
# dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
#
# ggsave("outputs/figures/fig01_nap_density.png",
#        plot_nap_density(data_clean), width = 8, height = 5)
# ggsave("outputs/figures/fig02_total_sleep_density.png",
#        plot_total_sleep_density(data_long), width = 8, height = 5)
# ggsave("outputs/figures/fig03_avg_sleep.png",
#        plot_avg_sleep(data_long), width = 7, height = 5)
# ggsave("outputs/figures/fig04_sleep_by_ethnicity.png",
#        plot_sleep_by_ethnicity(data_long), width = 9, height = 6)
# ggsave("outputs/figures/fig10_sleep_problems.png",
#        plot_sleep_problems(data_clean), width = 10, height = 6)
# ggsave("outputs/figures/fig11_sleep_problems_ethnicity.png",
#        plot_sleep_problems_ethnicity(data_long), width = 10, height = 6)
# ggsave("outputs/figures/fig14_rater_heatmap.png",
#        plot_rater_heatmap(data_clean), width = 7, height = 5)

cat("Script 02: Descriptive analysis complete.\n")
