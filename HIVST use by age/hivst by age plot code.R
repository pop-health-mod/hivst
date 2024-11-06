# Clearing all objects
rm(list = ls())
gc() 

library(dplyr)
library(survey)
library(ggplot2)

setwd("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by age")

final_hivstuse_byage <- read.csv("final_hivstuse_byage.csv", header = TRUE)

#with year
final_hivstuse_byage %>%
  ggplot(aes(y = factor(age_group), x = hivst_use_prop..percentage.)) +
  geom_point(aes(shape = survey_name), color = "purple") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, linewidth = 0.5) +  # horizontal error bars
  geom_text(aes(label = year, vjust = -0.5), size = 2.5) +  # Adjust vertical justification for years
  facet_wrap(~ country, scales = "free", nrow = 5, ncol = 7) +
  labs(
    title = "Country-level percentages of HIV self-test use stratified by age group from 2012 to 2022", 
    x = "Percentage (%)",
    y = "Age Group",
    shape = "Survey"
  ) +
  scale_shape_manual(values = c(
    "DHS" = 16,      # Filled circle
    "MICS" = 17,     # Filled triangle
    "PHIA" = 18,     # Filled diamond
    "KAIS" = 4,      # Cross
    "BAIS" = 15      # Filled square
  )) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray80", color = "black"),  # Shaded box for country names
    plot.background = element_rect(fill = "white", color = NA)           # Ensure white plot background
  )


# without year

final_hivstuse_byage %>%
  ggplot(aes(y = factor(age_group), x = hivst_use_prop..percentage.)) +
  geom_point(aes(shape = survey_name), color = "purple") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2, linewidth = 0.5) +  # horizontal error bars
  facet_wrap(~ country, scales = "free", nrow = 5, ncol = 7) +
  labs(
    title = "Country-level percentages of HIV self-test use stratified by age group from 2012 to 2022", 
    x = "Percentage (%)",
    y = "Age Group",
    shape = "Survey"
  ) +
  scale_shape_manual(values = c(
    "DHS" = 16,      # Filled circle
    "MICS" = 17,     # Filled triangle
    "PHIA" = 18,     # Filled diamond
    "KAIS" = 4,      # Cross
    "BAIS" = 15      # Filled square
  )) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray80", color = "black"),  # Shaded box for country names
    plot.background = element_rect(fill = "white", color = NA)           # Ensure white plot background
  )


##---------------------------------------------








