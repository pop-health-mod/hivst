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
library(lme4)
library(metafor)

setwd("D:\\Downloads\\MSc Thesis\\hivst\\surveys with biomarker data")

#--------------without excluding those on ART status--------------

#-----cleaning and subsetting for logistic regression --------

#--DHS---
bio_list_dhs <- readRDS("cleaned biomarker surveys/bio_list_dhs.rds")

# combining all dataframes in the list into one data frame
bio_list_dhs_cleaned <- lapply(bio_list_dhs, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # labelled columns to character
})

# merging the cleaned list
bio_dhs_combined <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  bio_list_dhs_cleaned
)
# keeping only observations with HIV status 0 and 1
table(bio_dhs_combined$hiv_status)
bio_dhs_combined <- subset(bio_dhs_combined, hiv_status %in% c(0, 1))


#--PHIA----
bio_list_phia <- readRDS("cleaned biomarker surveys/bio_list_phia.rds")

bio_list_phia_cleaned <- lapply(bio_list_phia, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # Convert labelled columns to character
})

# merge the cleaned list
bio_phia_combined <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  bio_list_phia_cleaned
)
# keeping only observations with HIV status 0 and 1
bio_phia_combined <- subset(bio_phia_combined, hiv_status %in% c(0, 1))
table(bio_phia_combined$hiv_status, useNA = "ifany")


#---BAIS----
bio_bais <- readRDS("cleaned biomarker surveys/bio_bais.rds")
# keeping only observations with HIV status 0 and 1
table(bio_bais$hiv_status, useNA = "ifany")
bio_bais <- subset(bio_bais, hiv_status %in% c(0, 1))


#---creating one single df for all-----
bio_surveys_list <- list(bio_dhs_combined, bio_phia_combined, bio_bais)
# Merging all surveys in surveys_list into a single data frame
bio_pooled_surveys <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  bio_surveys_list
)

saveRDS(bio_pooled_surveys, file = "cleaned biomarker surveys//bio_pooled_surveys.rds")


#----- performing logistic reg-------
bio_pooled_surveys <- readRDS("cleaned biomarker surveys/bio_pooled_surveys.rds")

# converting variables into necessary structure before regression
colnames(bio_pooled_surveys)

# hivst use
str(bio_pooled_surveys$hivst_use)
bio_pooled_surveys$hivst_use <- factor(bio_pooled_surveys$hivst_use, 
                                       levels = c(0, 1),
                                       labels = c("No", "Yes"))

# hiv status
str(bio_pooled_surveys$hiv_status)
bio_pooled_surveys$hiv_status <- factor(bio_pooled_surveys$hiv_status,
                                        levels = c(0, 1),
                                        labels = c("Negative", "Positive"))

# sex
str(bio_pooled_surveys$sex)
bio_pooled_surveys$sex <- factor(bio_pooled_surveys$sex, 
                                 levels = c(0, 1),
                                 labels = c("Female", "Male"))

# age group
str(bio_pooled_surveys$agegrp)
table(bio_pooled_surveys$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
bio_pooled_surveys$agegrp <- factor(
  bio_pooled_surveys$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(bio_pooled_surveys$region)
str(bio_pooled_surveys$region)
bio_pooled_surveys$region <- factor(bio_pooled_surveys$region,
                                    levels = c("0", "1"),
                                    labels = c("Rural", "Urban"))


# wealth index
table(bio_pooled_surveys$wealth_index)
str(bio_pooled_surveys$wealth_index)
bio_pooled_surveys$wealth_index <- as.character(bio_pooled_surveys$wealth_index)
bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index %in% c("1", "Lowest")
] <- "1"

bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index %in% c("2", "Second")
] <- "2"

bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index %in% c("3", "Middle")
] <- "3"

bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index %in% c("4", "Fourth")
] <- "4"

bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index %in% c("5", "Highest")
] <- "5"

bio_pooled_surveys$wealth_index[
  bio_pooled_surveys$wealth_index == "99"
] <- NA

bio_pooled_surveys$wealth_index <- factor(
  bio_pooled_surveys$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(bio_pooled_surveys$schl_years)
table(bio_pooled_surveys$schl_years)
bio_pooled_surveys$schl_years <- factor(bio_pooled_surveys$schl_years,
                                        levels = c("0", "1", "2", "3"),
                                        labels = c("No edu", "Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(bio_pooled_surveys$survey_id)
str(bio_pooled_surveys$survey_id)
bio_pooled_surveys$survey_id <- factor(bio_pooled_surveys$survey_id)

# country
str(bio_pooled_surveys$country)
bio_pooled_surveys$country <- factor(bio_pooled_surveys$country)


#------ logistic regression -----
basic_logistic <- glm(hivst_use ~ hiv_status, 
                      data = bio_pooled_surveys, family = "binomial")
summary(basic_logistic)

basic_logistic2 <- glm(hivst_use ~ hiv_status + sex, 
                       data = bio_pooled_surveys, family = "binomial")
summary(basic_logistic2)


basic_logistic3 <- glm(hivst_use ~ hiv_status + sex + region + agegrp, 
                       data = bio_pooled_surveys, family = "binomial")
summary(basic_logistic3)


basic_logistic4 <- glm(hivst_use ~ hiv_status + sex + region + agegrp + wealth_index + schl_years, 
                       data = bio_pooled_surveys, family = "binomial")
summary(basic_logistic4)


#---adding survey fixed effect (testing for survey fixed effect and country fixed effect------
final_logistic <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years + survey_id, 
                      data = bio_pooled_surveys, family = "binomial")
summary(final_logistic)


final_logistic2 <- glm(hivst_use ~ hiv_status + sex + region + agegrp + country, 
                       data = bio_pooled_surveys, family = "binomial")
summary(final_logistic2)



#---testing with mixed effect if it gives similar results (with survey id as random effect)---
mixed_logistic <- glmer(hivst_use ~ hiv_status + (1|survey_id), 
                        data = bio_pooled_surveys, family = "binomial")
summary(mixed_logistic)


#--------- logistic regression excluding those on ART----------------
#---------------only PHIA and BAIS-----------

#--PHIA----
bio_list_phia2 <- readRDS("cleaned biomarker surveys/bio_list_phia_art.rds")

bio_list_phia_cleaned2 <- lapply(bio_list_phia2, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # Convert labelled columns to character
})

# merge the cleaned list
bio_phia_combined2 <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  bio_list_phia_cleaned2
)


#---BAIS----
bio_bais2 <- readRDS("cleaned biomarker surveys/bio_bais_art.rds")


#---creating one single df for all-----
bio_surveys_list2 <- list(bio_phia_combined2, bio_bais2)
# Merging all surveys in surveys_list into a single data frame
bio_pooled_surveys2 <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  bio_surveys_list2
)

saveRDS(bio_pooled_surveys2, file = "cleaned biomarker surveys/bio_pooled_surveys_art.rds")

#----- performing logistic reg excluding those on ART-------
bio_pooled_svy_art <- readRDS("cleaned biomarker surveys/bio_pooled_surveys_art.rds")

# converting variables into necessary structure before regression
colnames(bio_pooled_svy_art)

# hivst use
str(bio_pooled_svy_art$hivst_use)
bio_pooled_svy_art$hivst_use <- factor(bio_pooled_svy_art$hivst_use, 
                                       levels = c(0, 1),
                                       labels = c("No", "Yes"))

# hiv status
str(bio_pooled_svy_art$hiv_status)
bio_pooled_svy_art$hiv_status <- factor(bio_pooled_svy_art$hiv_status,
                                        levels = c(0, 1),
                                        labels = c("Negative", "Positive"))

# sex
str(bio_pooled_svy_art$sex)
bio_pooled_svy_art$sex <- factor(bio_pooled_svy_art$sex, 
                                 levels = c(0, 1),
                                 labels = c("Female", "Male"))

# age group
str(bio_pooled_svy_art$agegrp)
table(bio_pooled_svy_art$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
bio_pooled_svy_art$agegrp <- factor(
  bio_pooled_svy_art$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(bio_pooled_svy_art$region)
str(bio_pooled_svy_art$region)
bio_pooled_svy_art$region <- factor(bio_pooled_svy_art$region,
                                    levels = c("0", "1"),
                                    labels = c("Rural", "Urban"))


# wealth index
table(bio_pooled_svy_art$wealth_index)
str(bio_pooled_svy_art$wealth_index)

bio_pooled_svy_art$wealth_index <- factor(
  bio_pooled_svy_art$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(bio_pooled_svy_art$schl_years)
table(bio_pooled_svy_art$schl_years)
bio_pooled_svy_art$schl_years <- factor(bio_pooled_svy_art$schl_years,
                                        levels = c("1", "2", "3"),
                                        labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(bio_pooled_svy_art$survey_id)
str(bio_pooled_svy_art$survey_id)
bio_pooled_svy_art$survey_id <- factor(bio_pooled_svy_art$survey_id)

# country
str(bio_pooled_svy_art$country)
bio_pooled_svy_art$country <- factor(bio_pooled_svy_art$country)


#------ logistic regression -----
basic_logistic <- glm(hivst_use ~ hiv_status, 
                      data = bio_pooled_svy_art, family = "binomial")
summary(basic_logistic)

basic_logistic2 <- glm(hivst_use ~ hiv_status + sex, 
                       data = bio_pooled_svy_art, family = "binomial")
summary(basic_logistic2)


basic_logistic3 <- glm(hivst_use ~ hiv_status + sex + region + agegrp, 
                       data = bio_pooled_svy_art, family = "binomial")
summary(basic_logistic3)


basic_logistic4 <- glm(hivst_use ~ hiv_status + sex + region + agegrp + wealth_index + schl_years, 
                       data = bio_pooled_svy_art, family = "binomial")
summary(basic_logistic4)


#---adding survey fixed effect (testing for survey fixed effect and country fixed effect------
final_logistic <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years + survey_id, 
                      data = bio_pooled_svy_art, family = "binomial")
summary(final_logistic)


final_logistic2 <- glm(hivst_use ~ hiv_status + sex + region + agegrp + country, 
                       data = bio_pooled_svy_art, family = "binomial")
summary(final_logistic2)


#----------------random effects meta-analysis-------------------------

# did logistic regression on each survey and saving the estimates in a df
# this RE meta analysis is on the datasets after excluding people on ART
# 8 PHIAs and 1 BAIS


df_meta <- rbind(df_survey1, df_survey2, df_survey3, df_survey4, df_survey5, df_survey6, df_survey7, df_survey8)
saveRDS(df_meta, file="D:\\Downloads\\df_meta.rds")


library(metafor)
df_meta <- readRDS("D:\\Downloads\\df_meta.rds")

# RE meta analysis
res <- rma.uni(
  yi    = df_meta$logOR,    # log-ORs
  sei   = df_meta$seLogOR,  # standard errors
  method = "REML"           
)
summary(res)

forest(res)

forest(
  res,
  slab    = df_meta$survey,
  transf  = exp,  # exponentiate the estimates and CIs
  refline = 1,    # no-effect line at OR=1
  xlab    = "Odds Ratio (95% CI)"
)



forest(res,
       slab    = df_meta$survey,
       transf  = exp,
       refline = 1,
       xlab    = "Odds Ratio (95% CI)",
       digits  = c(2, 2),
       col     = "blue",             # overall color
       col.square = "darkgreen",     # color for the squares
       bg       = "lightgreen",      # background color inside the squares
       border   = "darkgreen",       # border color around squares
       col.diamond = "purple",       # fill color of the pooled-effect diamond
       col.diamond.lines = "purple"  # outline color of the diamond
)









