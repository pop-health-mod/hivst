# Clearing all objects
rm(list = ls())
gc() 

library(dplyr)
library(survey)
library(ggplot2)

setwd("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex")

final_hivstuse_bysex <- read.csv("final_hivstuse_bysex.csv", header = TRUE)

# creating plot for HIVST use by sex

# without year

final_hivstuse_bysex %>%
  ggplot(aes(x=sex, y=hivst_use_prop*100))+
  geom_point(aes(shape=survey_name), color="purple")+
  geom_errorbar(data=final_hivstuse_bysex,
                aes(ymin=ci_lower*100, ymax=ci_upper*100),
                width=.2,linewidth=.5)+
  facet_wrap(~country,  scales="free", nrow = 5, ncol = 7)+
  #theme_minimal()+
  labs(title="Country level percentages of HIV self-test use stratified by sex from 2012 to 2022", 
       y="Percentage (%)",
       x="Sex",
       shape="Survey") +
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


ggsave("hivstuse_bysex.png", plot = last_plot(),
       width = 18, height = 12, units = "in", dpi = 300)


# with year

final_hivstuse_bysex %>%
  ggplot(aes(x=sex, y=hivst_use_prop*100)) +
  geom_point(aes(shape=survey_name), color="purple") +
  geom_errorbar(aes(ymin=ci_lower*100, ymax=ci_upper*100), width=.2, linewidth=.5) +
  geom_text(aes(label=year, hjust=ifelse(sex == "Female", 1.5, -0.5)), size=2.5) +  # Adjust hjust for female/male and reduce font size
  facet_wrap(~country, scales="free", nrow = 5, ncol = 7) +
  labs(title="Country level percentages of HIV self-test use stratified by sex from 2012 to 2022", 
       y="Percentage (%)",
       x="Sex",
       shape="Survey") +
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


ggsave("hivstuse_bysex_year.png", plot = last_plot(),
       width = 30, height = 21, units = "in", dpi = 300)


ggsave("sample.png", plot = last_plot(),
       width = 26, height = 18, units = "in", dpi = 300)


