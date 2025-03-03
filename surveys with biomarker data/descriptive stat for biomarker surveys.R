
# total 17 surveys with biomarker data, 15 countries (Malawi and Zimbabwe two surveys)

rm(list = ls())
gc()

library(dplyr)
library(survey)
library(stringr)
library(survey)
library(ggplot2)
library(labelled)
library(haven)


setwd("D:\\Downloads\\MSc Thesis\\hivst\\surveys with biomarker data")

#--------proportion of HIVST use by HIV status from the biomarker data------------
#-----DHS----------
bio_list_dhs <- readRDS("cleaned biomarker surveys/bio_list_dhs.rds")

# function for calculating hivst use by hiv status
calc_prop_hivst_hivstat <- function(data) {
  # subsetting data to include only hiv_status 0 and 1
  data_subset <- subset(data, hiv_status %in% c(0, 1))
  
  # creating the survey design object
  design <- svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~ind_wt,
    data = data_subset,
    nest = TRUE
  )
  
  # calculating survey-adjusted proportions by hiv status
  result <- svyby(
    ~I(hivst_use == 1),
    ~hiv_status,
    design = design,
    FUN = svyciprop,
    method = "logit",
    vartype = "ci",
    level = 0.95
  )
  
  # renaming columns for clarity
  names(result)[names(result) == "I(hivst_use == 1)"] <- "prop_hivst"
  
  return(result)
}


# checking whether the function works properly
calc_prop_hivst_hivstat(bio_list_dhs[[1]])
calc_prop_hivst_hivstat(bio_list_dhs[[2]])

# survey adjusted proportions
options(survey.lonely.psu = "adjust")
hivstat_hivstuse_dhs <- lapply(bio_list_dhs, calc_prop_hivst_hivstat)

# final data frame, also extracting hiv_status
survey_ids_dhs <- sapply(bio_list_dhs, function(df) unique(df$survey_id))
dhs_bio_hivstat <- do.call(
  rbind,
  lapply(seq_along(hivstat_hivstuse_dhs), function(i) {
    temp_df <- hivstat_hivstuse_dhs[[i]]
    data.frame(
      survey_id  = rep(survey_ids_dhs[i], nrow(temp_df)),
      hiv_status = temp_df[["hiv_status"]],        
      proportion = temp_df[["prop_hivst"]],        
      ci_l       = temp_df[["ci_l"]],
      ci_u       = temp_df[["ci_u"]]
    )
  })
)

dhs_bio_hivstat <- dhs_bio_hivstat %>%
  mutate(
    country_id = str_sub(survey_id, 1, 3),
    year       = str_sub(survey_id, 4, 7),
    survey     = str_sub(survey_id, 8, 10)
  )

dhs_bio_hivstat


#---------BAIS-----------
# bais_prop_hiv
# hiv_status I(hivst_use == 1)        ci_l       ci_u
# 0          0        0.02350053 0.019584453 0.02817716
# 1          1        0.01005199 0.005382414 0.01869654

# data frame with the BAIS 2021 estimates
bais_bio_hivstat <- data.frame(
  survey_id  = rep("BWA2021BAIS", 2),
  hiv_status = c(0, 1),
  proportion = c(0.02350053, 0.01005199),
  ci_l       = c(0.019584453, 0.005382414),
  ci_u       = c(0.02817716, 0.01869654)
)

# country_id, year, survey columns
bais_bio_hivstat <- bais_bio_hivstat %>%
  mutate(
    country_id = str_sub(survey_id, 1, 3),
    year       = str_sub(survey_id, 4, 7),
    survey     = str_sub(survey_id, 8, 11)   
  )

bais_bio_hivstat


#-----PHIA_-------
setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/PHIA raw data")
bio_list_phia <- readRDS("cleaned biomarker surveys/bio_list_phia.rds")

# function for calculating hivst use by hiv status
calc_prop_hivst_hivstat_phia <- function(data) {
  
  # creating the survey design object
  design <- svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~ind_wt,
    data = data,
    nest = TRUE
  )
  
  # calculating survey-adjusted proportions by hiv status
  result <- svyby(
    ~I(hivst_use == 1),
    ~hiv_status,
    design = design,
    FUN = svyciprop,
    method = "logit",
    vartype = "ci",
    level = 0.95
  )
  
  # renaming columns for clarity
  names(result)[names(result) == "I(hivst_use == 1)"] <- "prop_hivst"
  
  return(result)
}


# checking whether the function works properly
calc_prop_hivst_hivstat_phia(bio_list_phia[[1]])
calc_prop_hivst_hivstat_phia(bio_list_phia[[2]])

# survey adjusted proportions
options(survey.lonely.psu = "adjust")
hivstat_hivstuse_phia <- lapply(bio_list_phia, calc_prop_hivst_hivstat_phia)


# final data frame, also extracting hiv_status
survey_ids_phia <- sapply(bio_list_phia, function(df) unique(df$survey_id))
phia_bio_hivstat <- do.call(
  rbind,
  lapply(seq_along(hivstat_hivstuse_phia), function(i) {
    temp_df <- hivstat_hivstuse_phia[[i]]
    data.frame(
      survey_id  = rep(survey_ids_phia[i], nrow(temp_df)),
      hiv_status = temp_df[["hiv_status"]],        
      proportion = temp_df[["prop_hivst"]],        
      ci_l       = temp_df[["ci_l"]],
      ci_u       = temp_df[["ci_u"]]
    )
  })
)

phia_bio_hivstat <- phia_bio_hivstat %>%
  mutate(
    country_id = str_sub(survey_id, 1, 3),
    year       = str_sub(survey_id, 4, 7),
    survey     = str_sub(survey_id, 8, 11)
  )

phia_bio_hivstat


# rbinding all 
overall_bio_hivstat <- rbind(dhs_bio_hivstat, bais_bio_hivstat, phia_bio_hivstat)

# creating country column from iso3 country codes
iso3_to_country <- c(
  BEN = "Benin",
  BFA = "Burkina Faso",
  BDI = "Burundi",
  CMR = "Cameroon",
  COM = "Comoros",
  CIV = "Cote d'Ivoire",
  GAB = "Gabon",
  GHA = "Ghana",
  GIN = "Guinea",
  KEN = "Kenya",
  LBR = "Liberia",
  MDG = "Madagascar",
  MWI = "Malawi",
  MLI = "Mali",
  MRT = "Mauritania",
  RWA = "Rwanda",
  SEN = "Senegal",
  SLE = "Sierra Leone",
  ZAF = "South Africa",
  TZA = "Tanzania",
  UGA = "Uganda",
  ZMB = "Zambia",
  ZWE = "Zimbabwe",
  CAF = "Central African Republic",
  TCD = "Chad",
  COD = "Democratic Republic of the Congo",
  GMB = "Gambia",
  GNB = "Guinea-Bissau",
  STP = "Sao Tome and Principe",
  TGO = "Togo",
  SWZ = "Eswatini",
  NAM = "Namibia",
  LSO = "Lesotho",
  MOZ = "Mozambique",
  BWA = "Botswana"
)

overall_bio_hivstat <- overall_bio_hivstat %>%
  mutate(country = iso3_to_country[country_id])

overall_bio_hivstat <- overall_bio_hivstat %>%
  arrange(country)


#------------plot-------------
overall_bio_hivstat %>%
  ggplot(aes(x=hiv_status, y=proportion*100)) +
  geom_point(aes(shape=survey), color="purple") +
  geom_errorbar(aes(ymin=ci_l*100, ymax=ci_u*100), width=.2, linewidth=.5) +
  geom_text(aes(label = year), 
            nudge_x = 0.15,    # nudges the text to the right by 0.15 units
            size = 2.5) +
  facet_wrap(~country, scales="free", nrow = 3, ncol = 5) +
  labs(title="Country level proportions of HIV self-test use stratified by HIV status", 
       y="Proportion of HIVST use (%)",
       x="HIV status",
       shape="Survey") +
  scale_shape_manual(values = c(
    "DHS" = 16,      # Filled circle
    "PHIA" = 18,     # Filled diamond
    "BAIS" = 15      # Filled square
  )) +
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("HIV Negative", "HIV Positive"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "gray80", color = "black"),  # Shaded box for country names
    plot.background = element_rect(fill = "white", color = NA)           #  white plot background
  )




