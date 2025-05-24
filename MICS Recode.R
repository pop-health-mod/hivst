

# Clearing all objects
rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(labelled)
library(survey)

setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/MICS raw data")

# Note: MICS don't have biomarker data so HIV weight and HIV blood test result not available

# Central African Republic 2018-19

#-----Female------

cafmics_female_ind <- read_sav("Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/wm.sav")

# Selecting and renaming variables
cafmics2018f <- cafmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
   #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
   #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
 ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV

# Calculating the median year of interview
median(cafmics2018f$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
cafmics2018f <- cafmics2018f %>%  mutate(
    country = "Central African Republic",
    survey_id = "CAF2018MICS",
    med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
cafmics2018f <- cafmics2018f %>% select(country, survey_id, med_year, everything())

# Converting province into character
cafmics2018f$province <- as.character(cafmics2018f$province)

# Adding the 'sex' column with value 0 (women)
cafmics2018f <- cafmics2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
cafmics2018f <- cafmics2018f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
cafmics2018f <- cafmics2018f %>% mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
cafmics2018f <- cafmics2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
cafmics2018f <- cafmics2018f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))


# Recoding the wealth quintile 
cafmics2018f <- cafmics2018f %>% mutate(wealth_index = case_when(
    wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
    TRUE ~ wealth_index                         # Keep all other values unchanged
  )) %>% mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
cafmics2018f <- cafmics2018f %>% mutate(ever_heard = case_when(
    ever_heard == 1 ~ 1,           # Keep '1' unchanged
    ever_heard == 2 ~ 0,           # Recode '2' as '0'
    ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
    ))


# recoding hivst knowledge
cafmics2018f <- cafmics2018f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
    hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
    is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
    ))

# recoding hivst use
cafmics2018f <- cafmics2018f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
    ))

# recoding ever tested
cafmics2018f <- cafmics2018f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
cafmics2018f <- cafmics2018f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
cafmics2018f <- cafmics2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#check hivst proportion (0.039, matches report, knwledge=0.15, also matches)
#caf_design_f <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = cafmics2018f, nest = TRUE)
#prop_caff <- svyciprop(~I(hivst_use == 1), design = caf_design_f, method = "logit", level = 0.95)

#prop_caff_kn <- svyciprop(~I(hivst_knwldge == 1), design = caf_design_f, method = "logit", level = 0.95)

#------------Male-------------

cafmics_male_ind <- read_sav("Central African Republic MICS6 Datasets/Central African Republic MICS6 SPSS Datasets/mn.sav")

# Selecting and renaming variables
cafmics2018m <- cafmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV

# Calculating the median year of interview
median(cafmics2018m$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
cafmics2018m <- cafmics2018m %>%  mutate(
  country = "Central African Republic",
  survey_id = "CAF2018MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
cafmics2018m <- cafmics2018m %>% select(country, survey_id, med_year, everything())

# Converting province into character
cafmics2018m$province <- as.character(cafmics2018m$province)

# Adding the 'sex' column with value 1 (men)
cafmics2018m <- cafmics2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
cafmics2018m <- cafmics2018m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
cafmics2018m <- cafmics2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
cafmics2018m <- cafmics2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
cafmics2018m <- cafmics2018m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))

# Recoding the wealth quintile 
cafmics2018m <- cafmics2018m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
cafmics2018m <- cafmics2018m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
cafmics2018m <- cafmics2018m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
cafmics2018m <- cafmics2018m %>% mutate(hivst_use = case_when(
  ever_heard == 1 &  hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# recoding ever tested
cafmics2018m <- cafmics2018m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
cafmics2018m <- cafmics2018m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
cafmics2018m <- cafmics2018m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# Combining male and female for Central African Republic 2018-19
combined_cafmics <- bind_rows(cafmics2018f, cafmics2018m)


#-----------------------------------------------------------------------

# Chad 2019

#-----Female------

tcdmics_female_ind <- read_sav("Chad MICS6 Datasets\\Chad MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
tcdmics2019f <- tcdmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV

# Adding columns for country, survey ID, med_year
tcdmics2019f <- tcdmics2019f %>%  mutate(
  country = "Chad",
  survey_id = "TCD2019MICS")

# Reordering columns to make country, survey_id, median year the leftmost columns
tcdmics2019f <- tcdmics2019f %>% select(country, survey_id, everything())

# Converting province into character
tcdmics2019f$province <- as.character(tcdmics2019f$province)

# Adding the 'sex' column with value 0 (women)
tcdmics2019f <- tcdmics2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
tcdmics2019f <- tcdmics2019f %>% mutate(ind_wt = ind_wt * 1000000)


# Recoding regions
tcdmics2019f <- tcdmics2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding curr_marital
tcdmics2019f <- tcdmics2019f %>%
  mutate(curr_marital = case_when(
    curr_marital %in% c(1, 2) ~ 5,   # Married/In a Union
    curr_marital == 3 ~ 1,           # Single
    curr_marital == 9 ~ NA_real_,    # Recode 9 as NA
  ))

# recoding schooling
tcdmics2019f <- tcdmics2019f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))


# Recoding the wealth quintile 
tcdmics2019f <- tcdmics2019f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
tcdmics2019f <- tcdmics2019f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
tcdmics2019f <- tcdmics2019f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
tcdmics2019f <- tcdmics2019f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 &  hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# recoding ever tested
tcdmics2019f <- tcdmics2019f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# Recoding received_hivresult
tcdmics2019f <- tcdmics2019f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult %in% c(8, 9) ~ NA_real_,   # Recode '8' and '9' as NA
  ))

# Recoding the months ago last hivtest
tcdmics2019f <- tcdmics2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
tcdmics2019f <- tcdmics2019f %>%
  filter(!is.na(strata))

#checking proporiton of hivst use   #0.0258
#surveydesign_tcdmicsf <- svydesign(ids = ~psu, weights = ~ind_wt, data = tcdmics2019f, nest = TRUE)
#prop_tcdmicsf <- svyciprop(~I(hivst_use == 1), design = surveydesign_tcdmicsf , method = "logit", level = 0.95)

#------------Male-------------

tcdmics_male_ind <- read_sav("Chad MICS6 Datasets\\Chad MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
tcdmics2019m <- tcdmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Adding columns for country, survey ID
tcdmics2019m <- tcdmics2019m %>%  mutate(
  country = "Chad",
  survey_id = "TCD2019MICS")

# Reordering columns to make country, survey_id the leftmost columns
tcdmics2019m <- tcdmics2019m %>% select(country, survey_id, everything())

# Recoding province
tcdmics2019m$province <- as.character(tcdmics2019m$province)

# Adding the 'sex' column with value 1 (men)
tcdmics2019m <- tcdmics2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
tcdmics2019m <- tcdmics2019m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
tcdmics2019m <- tcdmics2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding curr_marital
tcdmics2019m <- tcdmics2019m %>%
  mutate(curr_marital = case_when(
    curr_marital %in% c(1, 2) ~ 5,   # Married/In a Union
    curr_marital == 3 ~ 1,           # Single
    curr_marital == 9 ~ NA_real_,    # Recode 9 as NA
  ))


# recoding schooling
tcdmics2019m <- tcdmics2019m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))

# Recoding the wealth quintile 
tcdmics2019m <- tcdmics2019m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
tcdmics2019m <- tcdmics2019m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
tcdmics2019m <- tcdmics2019m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))


# recoding hivst use
tcdmics2019m <- tcdmics2019m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))


# Recoding received_hivresult
tcdmics2019m <- tcdmics2019m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult %in% c(8, 9) ~ NA_real_,   # Recode '8' and '9' as NA
  ))

# Recoding the months ago last hivtest
tcdmics2019m <- tcdmics2019m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
tcdmics2019m <- tcdmics2019m %>%
  filter(!is.na(strata))

# Combining male and female for Chad 2019
combined_tcdmics <- bind_rows(tcdmics2019f, tcdmics2019m)


#-------------------------------------------------------------------------------

# Democratic Republic of Congo 2017-18


#-----Female------

codmics_female_ind <- read_sav("Democratic Republic of Congo MICS6 SPSS Datafiles\\DRCongo MICS6 SPSS Datafiles\\wm.sav")
  
codmics2017f <- codmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(codmics2017f$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
codmics2017f <- codmics2017f %>%  mutate(
  country = "Democratic Republic of Congo",
  survey_id = "COD2017MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
codmics2017f <- codmics2017f %>% select(country, survey_id, med_year, everything())

# Converting province into character
codmics2017f$province <- as.character(codmics2017f$province)

# Adding the 'sex' column with value 0 (women)
codmics2017f <- codmics2017f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
codmics2017f <- codmics2017f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
codmics2017f <- codmics2017f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
codmics2017f <- codmics2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
codmics2017f <- codmics2017f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))


# Recoding the wealth quintile 
codmics2017f <- codmics2017f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
codmics2017f <- codmics2017f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
codmics2017f <- codmics2017f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
codmics2017f <- codmics2017f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# recoding ever tested
codmics2017f <- codmics2017f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
codmics2017f <- codmics2017f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
codmics2017f <- codmics2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # ---- age grp den and num--------
# # Recode the existing agegrp into new age groups
# codmics2017f <- codmics2017f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# codmics_design_f <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = codmics2017f,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# cod_prop_age_f <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = codmics_design_f,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# cod_prop_age_f <- cod_prop_age_f %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# cod_prop_age_f <- cod_prop_age_f %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# cod_prop_age_f_selected <- cod_prop_age_f %>%
#   select(agegroup_new, deno, num)

#------------Male-------------

codmics_male_ind <- read_sav("Democratic Republic of Congo MICS6 SPSS Datafiles\\DRCongo MICS6 SPSS Datafiles\\mn.sav")

# Selecting and renaming variables
codmics2017m <- codmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(codmics2017m$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
codmics2017m <- codmics2017m %>%  mutate(
  country = "Democratic Republic of Congo",
  survey_id = "COD2017MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
codmics2017m <- codmics2017m %>% select(country, survey_id, med_year, everything())

# Converting province into character
codmics2017m$province <- as.character(codmics2017m$province)

# Adding the 'sex' column with value 1 (men)
codmics2017m <- codmics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
codmics2017m <- codmics2017m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
codmics2017m <- codmics2017m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
codmics2017m <- codmics2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
codmics2017m <- codmics2017m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))

# Recoding the wealth quintile 
codmics2017m <- codmics2017m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
codmics2017m <- codmics2017m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
codmics2017m <- codmics2017m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
codmics2017m <- codmics2017m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# recoding ever tested
codmics2017m <- codmics2017m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
codmics2017m <- codmics2017m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
codmics2017m <- codmics2017m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# ---- age grp den and num--------
# Recode the existing agegrp into new age groups
codmics2017m <- codmics2017m %>%
  mutate(
    agegroup_new = case_when(
      agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
      agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
      agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
      TRUE ~ NA_character_  # Handle unexpected values
    )
  )

codmics_design_m <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~ind_wt,
  data = codmics2017m,
  nest = TRUE
)


# Calculate the proportion om HIV testing usage by the new age groups
cod_prop_age_m <- svyby(
  ~I(hivst_use == 1),
  ~agegroup_new,
  design = codmics_design_m,
  FUN = svyciprop,
  method = "logit",
  vartype = "se",
  level = 0.95
)

# Rename the resulting columns mor clarity
cod_prop_age_m <- cod_prop_age_m %>%
  rename(
    prop = `I(hivst_use == 1)`,
    se_prop = `se.as.numeric(I(hivst_use == 1))`
  )


# Calculate denominator (deno) and numerator (num)
cod_prop_age_m <- cod_prop_age_m %>%
  mutate(
    deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
    num = prop * deno                          # Calculate numerator
  )

cod_prop_age_m_selected <- cod_prop_age_m %>%
  select(agegroup_new, deno, num)



# Combining male and female for DR Congo 2017-18
combined_codmics <- bind_rows(codmics2017f, codmics2017m)

#------------------------------------------------------------------------------

# Gambia 2017-18

# Female

gmbmics_female_ind <- read_sav("Gambia MICS6 Datasets\\The Gambia MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
gmbmics2018f <- gmbmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(gmbmics2018f$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
gmbmics2018f <- gmbmics2018f %>%  mutate(
  country = "Gambia",
  survey_id = "GMB2018MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
gmbmics2018f <- gmbmics2018f %>% select(country, survey_id, med_year, everything())

# Converting province into character
gmbmics2018f$province <- as.character(gmbmics2018f$province)

# Adding the 'sex' column with value 0 (women)
gmbmics2018f <- gmbmics2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
gmbmics2018f <- gmbmics2018f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
gmbmics2018f <- gmbmics2018f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gmbmics2018f <- gmbmics2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
gmbmics2018f <- gmbmics2018f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))


# Recoding the wealth quintile 
gmbmics2018f <- gmbmics2018f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
gmbmics2018f <- gmbmics2018f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
gmbmics2018f <- gmbmics2018f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
gmbmics2018f <- gmbmics2018f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# recoding ever tested
gmbmics2018f <- gmbmics2018f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
gmbmics2018f <- gmbmics2018f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
gmbmics2018f <- gmbmics2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


#------------Male-------------

gmbmics_male_ind <- read_sav("Gambia MICS6 Datasets\\The Gambia MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
gmbmics2018m <- gmbmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(gmbmics2018m$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
gmbmics2018m <- gmbmics2018m %>%  mutate(
  country = "Gambia",
  survey_id = "GMB2018MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
gmbmics2018m <- gmbmics2018m %>% select(country, survey_id, med_year, everything())

# Converting province into character
gmbmics2018m$province <- as.character(gmbmics2018m$province)

# Adding the 'sex' column with value 1 (men)
gmbmics2018m <- gmbmics2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
gmbmics2018m <- gmbmics2018m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
gmbmics2018m <- gmbmics2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gmbmics2018m <- gmbmics2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling
gmbmics2018m <- gmbmics2018m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode '9' as NA
    TRUE ~ schl_years            # Keep all other values unchanged
  ))

# Recoding the wealth quintile 
gmbmics2018m <- gmbmics2018m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
gmbmics2018m <- gmbmics2018m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
gmbmics2018m <- gmbmics2018m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
gmbmics2018m <- gmbmics2018m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0 (consider non-respondents as non-users)
))

# recoding ever tested
gmbmics2018m <- gmbmics2018m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
gmbmics2018m <- gmbmics2018m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
gmbmics2018m <- gmbmics2018m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# Combining male and female for Gambia 2018
combined_gmbmics <- bind_rows(gmbmics2018f, gmbmics2018m)

#------------------------------------------------------------------------------

# Ghana 2017-18

ghamics_female_ind <- read_sav("Ghana MICS6 SPSS Datasets\\Ghana MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
ghamics2017f <- ghamics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(ghamics2017f$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
ghamics2017f <- ghamics2017f %>%  mutate(
  country = "Ghana",
  survey_id = "GHA2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
ghamics2017f <- ghamics2017f %>% select(country, survey_id, med_year, everything())

# Converting province into character
ghamics2017f$province <- as.character(ghamics2017f$province)

# Adding the 'sex' column with value 0 (women)
ghamics2017f <- ghamics2017f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
ghamics2017f <- ghamics2017f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
ghamics2017f <- ghamics2017f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
ghamics2017f <- ghamics2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling (labels are slightly different)
ghamics2017f <- ghamics2017f %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3) ~ 2,  # Secondary (recode 2 and 3 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))


# Recoding the wealth quintile 
ghamics2017f <- ghamics2017f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
ghamics2017f <- ghamics2017f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
ghamics2017f <- ghamics2017f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
ghamics2017f <- ghamics2017f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
ghamics2017f <- ghamics2017f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
ghamics2017f <- ghamics2017f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
ghamics2017f <- ghamics2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


# ---- age grp den and num--------
# Recode the existing agegrp into new age groups
ghamics2017f <- ghamics2017f %>%
  mutate(
    agegroup_new = case_when(
      agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
      agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
      agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
      TRUE ~ NA_character_  # Handle unexpected values
    )
  )

ghamics_design_f <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~ind_wt,
  data = ghamics2017f,
  nest = TRUE
)


# Calculate the proportion of HIV testing usage by the new age groups
gha_prop_age_f <- svyby(
  ~I(hivst_use == 1),
  ~agegroup_new,
  design = ghamics_design_f,
  FUN = svyciprop,
  method = "logit",
  vartype = "se",
  level = 0.95
)

# Rename the resulting columns for clarity
gha_prop_age_f <- gha_prop_age_f %>%
  rename(
    prop = `I(hivst_use == 1)`,
    se_prop = `se.as.numeric(I(hivst_use == 1))`
  )


# Calculate denominator (deno) and numerator (num)
gha_prop_age_f <- gha_prop_age_f %>%
  mutate(
    deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
    num = prop * deno                          # Calculate numerator
  )

gha_prop_age_f_selected <- gha_prop_age_f %>%
  select(agegroup_new, deno, num)


#extract design adjusted SE
#ghamics_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = ghamics2017f, nest = TRUE)
#gha_prop <- svyciprop(~I(hivst_use == 1), design = ghamics_design, method = "logit", level = 0.95)
#se_hivst_use <- SE(gha_prop)

#ghaprop_hivst_use_f <- 0.0236
#gha_se_hivst_use_f <- 0.002033154

#eff_ss_f <- (ghaprop_hivst_use_f * (1 - ghaprop_hivst_use_f)) / (gha_se_hivst_use_f^2)
#ghamicsf_num <- ghaprop_hivst_use_f * eff_ss_f

#ghamicsf denominatory and numerator ( 5574.414, 131.5562)




#------------Male-------------

ghamics_male_ind <- read_sav("Ghana MICS6 SPSS Datasets\\Ghana MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
ghamics2017m <- ghamics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(ghamics2017m$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
ghamics2017m <- ghamics2017m %>%  mutate(
  country = "Ghana",
  survey_id = "GHA2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
ghamics2017m <- ghamics2017m %>% select(country, survey_id, med_year, everything())

# Converting province into character
ghamics2017m$province <- as.character(ghamics2017m$province)

# Adding the 'sex' column with value 1 (men)
ghamics2017m <- ghamics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
ghamics2017m <- ghamics2017m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
ghamics2017m <- ghamics2017m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
ghamics2017m <- ghamics2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))


# recoding schooling (labels are slightly different)
ghamics2017m <- ghamics2017m %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3) ~ 2,  # Secondary (recode 2 and 3 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))

# Recoding the wealth quintile 
ghamics2017m <- ghamics2017m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
ghamics2017m <- ghamics2017m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
ghamics2017m <- ghamics2017m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
ghamics2017m <- ghamics2017m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
ghamics2017m <- ghamics2017m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
ghamics2017m <- ghamics2017m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
ghamics2017m <- ghamics2017m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


# Recode the existing agegrp into new age groups
ghamics2017m <- ghamics2017m %>%
  mutate(
    agegroup_new = case_when(
      agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
      agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
      agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
      TRUE ~ NA_character_  # Handle unexpected values
    )
  )

ghamics_design_m <- svydesign(
  ids = ~psu,
  strata = ~strata,
  weights = ~ind_wt,
  data = ghamics2017m,
  nest = TRUE
)


# Calculate the proportion of HIV testing usage by the new age groups
gha_prop_age_m <- svyby(
  ~I(hivst_use == 1),
  ~agegroup_new,
  design = ghamics_design_m,
  FUN = svyciprop,
  method = "logit",
  vartype = "se",
  level = 0.95
)

# Rename the resulting columns for clarity
gha_prop_age_m <- gha_prop_age_m %>%
  rename(
    prop = `I(hivst_use == 1)`,
    se_prop = `se.as.numeric(I(hivst_use == 1))`
  )


# Calculate denominator (deno) and numerator (num)
gha_prop_age_m <- gha_prop_age_m%>%
  mutate(
    deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
    num = prop * deno                          # Calculate numerator
  )

gha_prop_age_m_selected <- gha_prop_age_m %>%
  select(agegroup_new, deno, num)


# Combining male and female for Ghana 2017-18
combined_ghamics <- bind_rows(ghamics2017f, ghamics2017m)


#extract design adjusted SE
ghamics_design_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = ghamics2017m, nest = TRUE)
gha_prop_m <- svyciprop(~I(hivst_use == 1), design = ghamics_design_m, method = "logit", level = 0.95)
se_hivst_use_m <- SE(gha_prop_m)

ghaprop_hivst_use_m <- 0.0143 
gha_se_hivst_use_m <- 0.002349851 

eff_ss_m <- (ghaprop_hivst_use_m * (1 - ghaprop_hivst_use_m)) / (gha_se_hivst_use_m^2)
ghamicsm_num <- ghaprop_hivst_use_m * eff_ss_m

#2017 ghamicsm denominator and numerator (2552.702, 36.50364)

#-----------------------------------------------------------------------------

# Guinea Bissau 2018-19

#-------- Female ----------

gnbmics_female_ind <- read_sav("Guinea Bissau MICS6 Datasets\\Guinea Bissau MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
gnbmics2018f <- gnbmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(gnbmics2018f$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
gnbmics2018f <- gnbmics2018f %>%  mutate(
  country = "Guinea-Bissau",
  survey_id = "GNB2018MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
gnbmics2018f <- gnbmics2018f %>% select(country, survey_id, med_year, everything())

# Converting province into character
gnbmics2018f$province <- as.character(gnbmics2018f$province)

# Adding the 'sex' column with value 0 (women)
gnbmics2018f <- gnbmics2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
gnbmics2018f <- gnbmics2018f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
gnbmics2018f <- gnbmics2018f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gnbmics2018f <- gnbmics2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling (labels are slightly different)
gnbmics2018f <- gnbmics2018f %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3) ~ 2,  # Secondary (recode 2 and 3 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))

# Recoding the wealth quintile 
gnbmics2018f <- gnbmics2018f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
gnbmics2018f <- gnbmics2018f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
gnbmics2018f <- gnbmics2018f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
gnbmics2018f <- gnbmics2018f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
gnbmics2018f <- gnbmics2018f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
gnbmics2018f <- gnbmics2018f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
gnbmics2018f <- gnbmics2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


# #----age grp wise den num---------
# # Recode the existing agegrp into new age groups
# gnbmics2018f <- gnbmics2018f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# gnbmics_design_f <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = gnbmics2018f,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# gnb_prop_age_f <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = gnbmics_design_f,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# gnb_prop_age_f <- gnb_prop_age_f %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# gnb_prop_age_f <- gnb_prop_age_f %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# gnb_prop_age_f_selected <- gnb_prop_age_f %>%
#   select(agegroup_new, deno, num)



#------------Male-------------

gnbmics_male_ind <- read_sav("Guinea Bissau MICS6 Datasets\\Guinea Bissau MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
gnbmics2018m <- gnbmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(gnbmics2018m$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
gnbmics2018m <- gnbmics2018m %>%  mutate(
  country = "Guinea-Bissau",
  survey_id = "GNB2018MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
gnbmics2018m <- gnbmics2018m %>% select(country, survey_id, med_year, everything())

# Converting province into character
gnbmics2018m$province <- as.character(gnbmics2018m$province)

# Adding the 'sex' column with value 1 (men)
gnbmics2018m <- gnbmics2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
gnbmics2018m <- gnbmics2018m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
gnbmics2018m <- gnbmics2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gnbmics2018m <- gnbmics2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))


# recoding schooling (labels are slightly different)
gnbmics2018m <- gnbmics2018m %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3) ~ 2,  # Secondary (recode 2 and 3 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))

# Recoding the wealth quintile 
gnbmics2018m <- gnbmics2018m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
gnbmics2018m <- gnbmics2018m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
gnbmics2018m <- gnbmics2018m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
gnbmics2018m <- gnbmics2018m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
gnbmics2018m <- gnbmics2018m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
gnbmics2018m <- gnbmics2018m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
gnbmics2018m <- gnbmics2018m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#----age grp wise den num---------
# Recode the existing agegrp into new age groups
# gnbmics2018m <- gnbmics2018m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# gnbmics_design_m <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = gnbmics2018m,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion om HIV testing usage by the new age groups
# gnb_prop_age_m <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = gnbmics_design_m,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns mor clarity
# gnb_prop_age_m <- gnb_prop_age_m %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# gnb_prop_age_m <- gnb_prop_age_m %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# gnb_prop_age_m_selected <- gnb_prop_age_m %>%
#   select(agegroup_new, deno, num)
# 


# Combining male and female for Guinea-Bissau 2018-19
combined_gnbmics <- bind_rows(gnbmics2018f, gnbmics2018m)

#-------------------------------------------------------------------------------

# Madagascar 2018

# ---------Female-----
mdgmics_female_ind <- read_sav("Madagascar MICS6 datasets\\Madagascar MICS6 datasets\\Madagascar MICS6 SPSS datasets\\wm.sav")

# Selecting and renaming variables
mdgmics2018f <- mdgmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(mdgmics2018f$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
mdgmics2018f <- mdgmics2018f %>%  mutate(
  country = "Madagascar",
  survey_id = "MDG2018MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
mdgmics2018f <- mdgmics2018f %>% select(country, survey_id, med_year, everything())

# Converting province into character
mdgmics2018f$province <- as.character(mdgmics2018f$province)

# Adding the 'sex' column with value 0 (women)
mdgmics2018f <- mdgmics2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mdgmics2018f <- mdgmics2018f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
mdgmics2018f <- mdgmics2018f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mdgmics2018f <- mdgmics2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile 
mdgmics2018f <- mdgmics2018f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
mdgmics2018f <- mdgmics2018f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
mdgmics2018f <- mdgmics2018f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
mdgmics2018f <- mdgmics2018f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
mdgmics2018f <- mdgmics2018f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
mdgmics2018f <- mdgmics2018f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
mdgmics2018f <- mdgmics2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # Recode the existing agegrp into new age groups
# mdgmics2018f <- mdgmics2018f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# mdgmics_design_f <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = mdgmics2018f,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# mdg_prop_age_f <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = mdgmics_design_f,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# mdg_prop_age_f <- mdg_prop_age_f %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# mdg_prop_age_f <- mdg_prop_age_f %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# mdg_prop_age_f_selected <- mdg_prop_age_f %>%
#   select(agegroup_new, deno, num)
# 



#extract design adjusted SE
#mdgmics_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mdgmics2018f, nest = TRUE)
#mdg_prop <- svyciprop(~I(hivst_use == 1), design = mdgmics_design, method = "logit", level = 0.95)
#se_hivst_use <- SE(mdg_prop)

#mdgprop_hivst_use_f <- 0.0166
#mdg_se_hivst_use_f <- 0.001799979

#eff_ss_f <- (mdgprop_hivst_use_f * (1 - mdgprop_hivst_use_f)) / (mdg_se_hivst_use_f^2)
#mdgmicsf_num <- mdgprop_hivst_use_f * eff_ss_f

#mdgmicsf Den: 5038.525 Num: 83.63951


#------------Male-------------

mdgmics_male_ind <- read_sav("Madagascar MICS6 datasets\\Madagascar MICS6 datasets\\Madagascar MICS6 SPSS datasets\\mn.sav")

# Selecting and renaming variables
mdgmics2018m <- mdgmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(mdgmics2018m$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country, survey ID, med_year
mdgmics2018m <- mdgmics2018m %>%  mutate(
  country = "Madagascar",
  survey_id = "MDG2018MICS",
  med_year = 2018)

# Reordering columns to make country, survey_id, median year the leftmost columns
mdgmics2018m <- mdgmics2018m %>% select(country, survey_id, med_year, everything())

# Converting province into character
mdgmics2018m$province <- as.character(mdgmics2018m$province)

# Adding the 'sex' column with value 1 (men)
mdgmics2018m <- mdgmics2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
mdgmics2018m <- mdgmics2018m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
mdgmics2018m <- mdgmics2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mdgmics2018m <- mdgmics2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile 
mdgmics2018m <- mdgmics2018m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
mdgmics2018m <- mdgmics2018m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
mdgmics2018m <- mdgmics2018m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
mdgmics2018m <- mdgmics2018m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
mdgmics2018m <- mdgmics2018m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
mdgmics2018m <- mdgmics2018m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
mdgmics2018m <- mdgmics2018m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # Recode the existing agegrp into new age groups
# mdgmics2018m <- mdgmics2018m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# mdgmics_design_m <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = mdgmics2018m,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# mdg_prop_age_m <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = mdgmics_design_m,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# mdg_prop_age_m <- mdg_prop_age_m %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# mdg_prop_age_m <- mdg_prop_age_m %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# mdg_prop_age_m_selected <- mdg_prop_age_m %>%
#   select(agegroup_new, deno, num)
# 


#mdgmics_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mdgmics2018m, nest = TRUE)
#mdg_prop <- svyciprop(~I(hivst_use == 1), design = mdgmics_design, method = "logit", level = 0.95)
#se_hivst_use <- SE(mdg_prop)

#mdgprop_hivst_use_m <- 0.01155
#mdg_se_hivst_use_m <- 0.001933078

#eff_ss_m <- (mdgprop_hivst_use_m * (1 - mdgprop_hivst_use_m)) / (mdg_se_hivst_use_m^2)
#mdgmicsm_num <- mdgprop_hivst_use_m * eff_ss_m

#mdgmicsm Den: 3055.188 Num: 35.28742


# Combining male and female for Madagascar 2018
combined_mdgmics <- bind_rows(mdgmics2018f, mdgmics2018m)

#------------------------------------------------------------------------------

# Malawi 2019-20

#-------- Female ----------

mwimics_female_ind <- read_sav("Malawi MICS6 SPSS\\Malawi MICS6 SPSS\\Malawi MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
mwimics2019f <- mwimics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(mwimics2019f$year, na.rm = TRUE) # median interview year 2020

# Adding columns for country, survey ID, med_year
mwimics2019f <- mwimics2019f %>%  mutate(
  country = "Malawi",
  survey_id = "MWI2019MICS",
  med_year = 2020)

# Reordering columns to make country, survey_id, median year the leftmost columns
mwimics2019f <- mwimics2019f %>% select(country, survey_id, med_year, everything())

# Converting province into character
mwimics2019f$province <- as.character(mwimics2019f$province)

# Adding the 'sex' column with value 0 (women)
mwimics2019f <- mwimics2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mwimics2019f <- mwimics2019f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
mwimics2019f <- mwimics2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mwimics2019f <- mwimics2019f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# recoding schooling (labels are slightly different)
mwimics2019f <- mwimics2019f %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3, 5) ~ 2,  # Secondary (recode 2 and 3 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))

# Recoding the wealth quintile 
mwimics2019f <- mwimics2019f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
mwimics2019f <- mwimics2019f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
mwimics2019f <- mwimics2019f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
mwimics2019f <- mwimics2019f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
mwimics2019f <- mwimics2019f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
mwimics2019f <- mwimics2019f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
mwimics2019f <- mwimics2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # # age grp wise den num
# # Recode the existing agegrp into new age groups
# mwimics2019f <- mwimics2019f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# mwi_prop_age_f<- with(mwimics2019f, table(agegroup_new, hivst_use, useNA = "ifany"))
# mwi_prop_age_f<- mwi_prop_age_f  * 0.8



#extract design adjusted SE
#mwimics_design_f <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mwimics2019f, nest = TRUE)
#mwi_prop_f <- svyciprop(~I(hivst_use == 1), design = mwimics_design_f, method = "logit", level = 0.95)
#ci <- confint(mwi_prop_f)  
#se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

#mwiprop_hivst_use_f <- 0.0664
#mwi_se_hivst_use_f <- 0.003048752

#eff_ss_f <- (mwiprop_hivst_use_f * (1 - mwiprop_hivst_use_f)) / (mwi_se_hivst_use_f^2)
#ghamicsf_num <- mwiprop_hivst_use_f * eff_ss_f

#mwimicsf denominator and numerator ( 6669.369, 442.8461)


#------------Male-------------

mwimics_male_ind <- read_sav("Malawi MICS6 SPSS\\Malawi MICS6 SPSS\\Malawi MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
mwimics2019m <- mwimics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(mwimics2019m$year, na.rm = TRUE) # median interview year 2020

# Adding columns for country, survey ID, med_year
mwimics2019m <- mwimics2019m %>%  mutate(
  country = "Malawi",
  survey_id = "MWI2019MICS",
  med_year = 2020)

# Reordering columns to make country, survey_id, median year the leftmost columns
mwimics2019m <- mwimics2019m %>% select(country, survey_id, med_year, everything())

# Converting province into character
mwimics2019m$province <- as.character(mwimics2019m$province)

# Adding the 'sex' column with value 1 (men)
mwimics2019m <- mwimics2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
mwimics2019m <- mwimics2019m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
mwimics2019m <- mwimics2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mwimics2019m <- mwimics2019m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))


# recoding schooling (labels are slightly different)
mwimics2019m <- mwimics2019m %>%
  mutate(schl_years = case_when(
    schl_years == 0 ~ 0,  # Pre-primary or none (unchanged)
    schl_years == 1 ~ 1,  # Primary (unchanged)
    schl_years %in% c(2, 3, 5) ~ 2,  # Secondary (recode 2, 3 , 5 to 2)
    schl_years == 4 ~ 3,  # Tertiary (recode 4 to 3)
    schl_years == 9 ~ NA_real_  # Missing
  ))

# Recoding the wealth quintile 
mwimics2019m <- mwimics2019m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
mwimics2019m <- mwimics2019m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
mwimics2019m <- mwimics2019m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
mwimics2019m <- mwimics2019m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
mwimics2019m <- mwimics2019m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
mwimics2019m <- mwimics2019m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
mwimics2019m <- mwimics2019m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # age grp wise den num
# # Recode the existing agegrp into new age groups
# mwimics2019m <- mwimics2019m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# mwi_prop_age_m<- with(mwimics2019m, table(agegroup_new, hivst_use, useNA = "ifany"))
# mwi_prop_age_m<- mwi_prop_age_m  * 0.8



#extract design adjusted SE
#mwimics_design_m <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mwimics2019m, nest = TRUE)
#mwi_prop_m <- svyciprop(~I(hivst_use == 1), design = mwimics_design_m, method = "logit", level = 0.95)
#ci <- confint(mwi_prop_m)  
#se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

#mwiprop_hivst_use_m <- 0.0996
#mwi_se_hivst_use_m <- 0.00645901

#eff_ss_m <- (mwiprop_hivst_use_m * (1 - mwiprop_hivst_use_m)) / (mwi_se_hivst_use_m^2)
#mwimicsm_num <- mwiprop_hivst_use_m * eff_ss_m

#mwimicsm denominator and numerator ( 2149.626, 214.1028)


# Combining male and female for Malawi 2019-20
combined_mwimics <- bind_rows(mwimics2019f, mwimics2019m)

#checking proportions (F: 0.066, M: 0.099 ) matches report
#weighted.mean(mwimics2019f$hivst_use, mwimics2019f$ind_wt, na.rm = T)
#weighted.mean(mwimics2019m$hivst_use, mwimics2019m$ind_wt, na.rm = T)

#------------------------------------------------------------------------------

# Sao Tome and Principe 2019

#-------- Female ----------

stpmics_female_ind <- read_sav("Sao Tome and Principe MICS6 Datasets\\Sao Tome and Principe MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
stpmics2019f <- stpmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Identifying strata with only one PSU
psu_count_by_strata_f <- stpmics2019f %>%
  group_by(strata) %>%
  summarise(n_psu = n_distinct(psu)) %>%
  filter(n_psu == 1)
single_psu_strata_f <- psu_count_by_strata_f$strata  

# Removing strata with only one PSU 
stpmics2019f <- stpmics2019f %>%
  filter(!strata %in% single_psu_strata_f)

# Calculating the median year of interview
median(stpmics2019f$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
stpmics2019f <- stpmics2019f %>%  mutate(
  country = "São Tomé and Príncipe",
  survey_id = "STP2019MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
stpmics2019f <- stpmics2019f %>% select(country, survey_id, med_year, everything())

# Converting province into character
stpmics2019f$province <- as.character(stpmics2019f$province)

# Adding the 'sex' column with value 0 (women)
stpmics2019f <- stpmics2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
stpmics2019f <- stpmics2019f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
stpmics2019f <- stpmics2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
stpmics2019f <- stpmics2019f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
stpmics2019f <- stpmics2019f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
stpmics2019f <- stpmics2019f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
stpmics2019f <- stpmics2019f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
stpmics2019f <- stpmics2019f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
stpmics2019f <- stpmics2019f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
stpmics2019f <- stpmics2019f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
stpmics2019f <- stpmics2019f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
stpmics2019f <- stpmics2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))





#------------Male-------------

stpmics_male_ind <- read_sav("Sao Tome and Principe MICS6 Datasets\\Sao Tome and Principe MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
stpmics2019m <- stpmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV

# Identifying strata with only one PSU
psu_count_by_strata_m <- stpmics2019m %>%
  group_by(strata) %>%
  summarise(n_psu = n_distinct(psu)) %>%
  filter(n_psu == 1)
single_psu_strata_m <- psu_count_by_strata_m$strata  

# Removing strata with only one PSU 
stpmics2019m <- stpmics2019m %>%
  filter(!strata %in% single_psu_strata_m)

# Calculating the median year of interview
median(stpmics2019m$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
stpmics2019m <- stpmics2019m %>%  mutate(
  country = "São Tomé and Príncipe",
  survey_id = "STP2019MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
stpmics2019m <- stpmics2019m %>% select(country, survey_id, med_year, everything())

# Converting province into character
stpmics2019m$province <- as.character(stpmics2019m$province)

# Adding the 'sex' column with value 1 (men)
stpmics2019m <- stpmics2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
stpmics2019m <- stpmics2019m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
stpmics2019m <- stpmics2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
stpmics2019m <- stpmics2019m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
stpmics2019m <- stpmics2019m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))


# Recoding the wealth quintile 
stpmics2019m <- stpmics2019m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
stpmics2019m <- stpmics2019m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
stpmics2019m <- stpmics2019m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
stpmics2019m <- stpmics2019m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
stpmics2019m <- stpmics2019m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
stpmics2019m <- stpmics2019m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
stpmics2019m <- stpmics2019m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))



# Combining male and female for Sao Tome and Principe 2019
combined_stpmics <- bind_rows(stpmics2019f, stpmics2019m)



#female (27.9, matches report)
#stp_design_f <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = stpmics2019f, nest = TRUE)
#prop_stp_f <- svyciprop(~I(hivst_use == 1), design = stp_design_f, method = "logit", level = 0.95)


#male (17.9, matches report)
#stp_design_m <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = stpmics2019m, nest = TRUE, lonely.psu = "adjust")
#prop_stp_m <- svyciprop(~I(hivst_use == 1), design = stp_design_m, method = "logit", level = 0.95)



#-----------------------------------------------------------------------------

# Sierra Leone 2017 (strata variable not available directly)

#-------- Female ----------

slemics_female_ind <- read_sav("Sierra Leone MICS6 Datasets\\Sierra Leone MICS6 Datasets\\wm.sav")

# Create the proxy strata by combining district (HH7) and urban/rural (HH6)
slemics_female_ind <- slemics_female_ind %>%
  mutate(
    HH7 = as.numeric(HH7),
    HH6 = as.numeric(HH6)
  )

# Create the new numeric strata variable
slemics_female_ind <- slemics_female_ind %>%
  mutate(
    strata = HH7 * 10 + HH6
  )

# Selecting and renaming variables
slemics2017f <- slemics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata,                         # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(slemics2017f$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
slemics2017f <- slemics2017f %>%  mutate(
  country = "Sierra Leone",
  survey_id = "SLE2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
slemics2017f <- slemics2017f %>% select(country, survey_id, med_year, everything())

# Converting province into character
slemics2017f$province <- as.character(slemics2017f$province)

# Adding the 'sex' column with value 0 (women)
slemics2017f <- slemics2017f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
slemics2017f <- slemics2017f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
slemics2017f <- slemics2017f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
slemics2017f <- slemics2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
slemics2017f <- slemics2017f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
slemics2017f <- slemics2017f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
slemics2017f <- slemics2017f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
slemics2017f <- slemics2017f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
slemics2017f <- slemics2017f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
slemics2017f <- slemics2017f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
slemics2017f <- slemics2017f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
slemics2017f <- slemics2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
slemics2017f <- slemics2017f %>%
  filter(!is.na(strata))

# # Recode the existing agegrp into new age groups
# slemics2017f <- slemics2017f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# table(slemics2017f$agegroup_new, slemics2017f$hivst_use)
# table_data <- matrix(c(7109, 209, 5314, 212, 4868, 156), nrow = 3, byrow = TRUE,
#                      dimnames = list(c("Group 1 (Age 15-24)", "Group 2 (Age 25-34)", "Group 3 (Age 35+)"),
#                                      c("0", "1")))
# table_data <- cbind(table_data, 'Total' = rowSums(table_data))
# print(table_data * 0.8)





#extract design adjusted SE
slemics_design <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = slemics2017f, nest = TRUE)
sle_prop <- svyciprop(~I(hivst_use == 1), design = slemics_design, method = "logit", level = 0.95)
ci <- confint(sle_prop)  
se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

sleprop_hivst_use_f <- 0.0323
sle_se_hivst_use_f <- 0.002476462

eff_ss_f <- (sleprop_hivst_use_f * (1 - sleprop_hivst_use_f)) / (sle_se_hivst_use_f^2)
slemicsf_num <- sleprop_hivst_use_f * eff_ss_f

#sle_f (denominator: 5096.593, numerator: 164.6199)


#------------Male-------------

slemics_male_ind <- read_sav("Sierra Leone MICS6 Datasets\\Sierra Leone MICS6 Datasets\\mn.sav")


# Create the proxy strata by combining district (HH7) and urban/rural (HH6)
slemics_male_ind <- slemics_male_ind %>%
  mutate(
    HH7 = as.numeric(HH7),
    HH6 = as.numeric(HH6)
  )

# Create the new numeric strata variable
slemics_male_ind <- slemics_male_ind %>%
  mutate(
    strata = HH7 * 10 + HH6
  )


# Selecting and renaming variables
slemics2017m <- slemics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata,                          # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(slemics2017m$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
slemics2017m <- slemics2017m %>%  mutate(
  country = "Sierra Leone",
  survey_id = "SLE2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
slemics2017m <- slemics2017m %>% select(country, survey_id, med_year, everything())

# Converting province into character
slemics2017m$province <- as.character(slemics2017m$province)

# Adding the 'sex' column with value 1 (men)
slemics2017m <- slemics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
slemics2017m <- slemics2017m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
slemics2017m <- slemics2017m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
slemics2017m <- slemics2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
slemics2017m <- slemics2017m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))


# Recoding the wealth quintile 
slemics2017m <- slemics2017m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
slemics2017m <- slemics2017m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
slemics2017m <- slemics2017m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
slemics2017m <- slemics2017m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
 is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
slemics2017m <- slemics2017m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
slemics2017m <- slemics2017m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
slemics2017m <- slemics2017m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


#removing NA in strata
slemics2017m <- slemics2017m %>%
  filter(!is.na(strata))

# # Recode the existing agegrp into new age groups
# slemics2017m <- slemics2017m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# table(slemics2017m$agegroup_new, slemics2017m$hivst_use)
# table_data_m <- matrix(c(7109, 209, 5314, 212, 4868, 156), nrow = 3, byrow = TRUE,
#                      dimnames = list(c("Group 1 (Age 15-24)", "Group 2 (Age 25-34)", "Group 3 (Age 35+)"),
#                                      c("0", "1")))
# table_data_m <- cbind(table_data_m, 'Total' = rowSums(table_data_m))
# print(table_data_m * 0.8)


#extract design adjusted SE
slemics_design_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = slemics2017m, nest = TRUE)
sle_prop_m <- svyciprop(~I(hivst_use == 1), design = slemics_design_m, method = "logit", level = 0.95)
ci <- confint(sle_prop_m)  
se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

sleprop_hivst_use_m <- 0.0204
sle_se_hivst_use_m <- 0.002847059

eff_ss_m <- (sleprop_hivst_use_m * (1 - sleprop_hivst_use_m)) / (sle_se_hivst_use_m^2)
slemicsm_num <- sleprop_hivst_use_m * eff_ss_m

#sle_m (denominator: 2465.392, numerator: 0.0204)


# Combining male and female for Sierra Leone 2017
combined_slemics <- bind_rows(slemics2017f, slemics2017m)

#checking hivst proportion (female: 0.032 , male:  0.02) matches report
#weighted.mean(slemics2017f$hivst_use, slemics2017f$ind_wt, na.rm = T)
#weighted.mean(slemics2017m$hivst_use, slemics2017m$ind_wt, na.rm = T)

# HIVST use by sex (female: 0,032, male: 0.02: matches report)
#sle_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = combined_slemics, nest = TRUE)

#hivst_by_sex <- svyby(~I(hivst_use == 1),~sex, sle_design,svyciprop,method = "logit", vartype = "ci")

#-------------------------------------------------------------------------------

# Togo 2017

#-------- Female ----------

tgomics_female_ind <- read_sav("Togo MICS6 SPSS Datasets\\Togo MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
tgomics2017f <- tgomics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(tgomics2017f$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
tgomics2017f <- tgomics2017f %>%  mutate(
  country = "Togo",
  survey_id = "TGO2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
tgomics2017f <- tgomics2017f %>% select(country, survey_id, med_year, everything())

# Converting province into character
tgomics2017f$province <- as.character(tgomics2017f$province)

# Adding the 'sex' column with value 0 (women)
tgomics2017f <- tgomics2017f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
tgomics2017f <- tgomics2017f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
tgomics2017f <- tgomics2017f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
tgomics2017f <- tgomics2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
tgomics2017f <- tgomics2017f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
tgomics2017f <- tgomics2017f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
tgomics2017f <- tgomics2017f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
tgomics2017f <- tgomics2017f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
tgomics2017f <- tgomics2017f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
tgomics2017f <- tgomics2017f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
tgomics2017f <- tgomics2017f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
tgomics2017f <- tgomics2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
tgomics2017f <- tgomics2017f %>%
  filter(!is.na(strata))

#------------Male-------------

tgomics_male_ind <- read_sav("Togo MICS6 SPSS Datasets\\Togo MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
tgomics2017m <- tgomics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(tgomics2017m$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country, survey ID, med_year
tgomics2017m <- tgomics2017m %>%  mutate(
  country = "Togo",
  survey_id = "TGO2017MICS",
  med_year = 2017)

# Reordering columns to make country, survey_id, median year the leftmost columns
tgomics2017m <- tgomics2017m %>% select(country, survey_id, med_year, everything())

# Converting province into character
tgomics2017m$province <- as.character(tgomics2017m$province)

# Adding the 'sex' column with value 1 (men)
tgomics2017m <- tgomics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
tgomics2017m <- tgomics2017m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
tgomics2017m <- tgomics2017m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
tgomics2017m <- tgomics2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
tgomics2017m <- tgomics2017m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))


# Recoding the wealth quintile 
tgomics2017m <- tgomics2017m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
tgomics2017m <- tgomics2017m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
tgomics2017m <- tgomics2017m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
tgomics2017m <- tgomics2017m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
tgomics2017m <- tgomics2017m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
tgomics2017m <- tgomics2017m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
tgomics2017m <- tgomics2017m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
tgomics2017m <- tgomics2017m %>%
  filter(!is.na(strata))

# Combining male and female for Togo 2017
combined_tgomics <- bind_rows(tgomics2017f, tgomics2017m)



#------------------------------------------------------------------------------


# Comoros 2022

#-------- Female ----------

commics_female_ind <- read_sav("Comoros MICS6 Datasets\\Comoros MICS6 Datasets\\Comoros MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
commics2022f <- commics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(commics2022f$year, na.rm = TRUE) # median interview year 2022

# Adding columns for country, survey ID, med_year
commics2022f <- commics2022f %>%  mutate(
  country = "Comoros",
  survey_id = "COM2022MICS",
  med_year = 2022)

# Reordering columns to make country, survey_id, median year the leftmost columns
commics2022f <- commics2022f %>% select(country, survey_id, med_year, everything())

# Converting province into character
commics2022f$province <- as.character(commics2022f$province)

# Adding the 'sex' column with value 0 (women)
commics2022f <- commics2022f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
commics2022f <- commics2022f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
commics2022f <- commics2022f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
commics2022f <- commics2022f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
commics2022f <- commics2022f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
commics2022f <- commics2022f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
commics2022f <- commics2022f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
commics2022f <- commics2022f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
commics2022f <- commics2022f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
commics2022f <- commics2022f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
commics2022f <- commics2022f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
commics2022f <- commics2022f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))


#------------Male-------------

commics_male_ind <- read_sav("Comoros MICS6 Datasets\\Comoros MICS6 Datasets\\Comoros MICS6 SPSS Datasets\\mn.sav")


# Selecting and renaming variables
commics2022m <- commics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(commics2022m$year, na.rm = TRUE) # median interview year 2022

# Adding columns for country, survey ID, med_year
commics2022m <- commics2022m %>%  mutate(
  country = "Comoros",
  survey_id = "COM2022MICS",
  med_year = 2022)

# Reordering columns to make country, survey_id, median year the leftmost columns
commics2022m <- commics2022m %>% select(country, survey_id, med_year, everything())

# Converting province into character
commics2022m$province <- as.character(commics2022m$province)

# Adding the 'sex' column with value 1 (men)
commics2022m <- commics2022m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
commics2022m <- commics2022m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
commics2022m <- commics2022m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
commics2022m <- commics2022m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
commics2022m <- commics2022m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,  # Recode 9 as NA
    TRUE ~ schl_years  # Keep everything else unchanged
  ))


# Recoding the wealth quintile 
commics2022m <- commics2022m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
commics2022m <- commics2022m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
commics2022m <- commics2022m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
commics2022m <- commics2022m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
commics2022m <- commics2022m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
commics2022m <- commics2022m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
commics2022m <- commics2022m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# Combining male and female for Comoros 2022
combined_commics <- bind_rows(commics2022f, commics2022m)

#-------------------------------------------------------------------------------

# Eswatini 2021-22

#-------- Female ----------

swzmics_female_ind <- read_sav("Eswatini MICS6 Datasets\\Eswatini MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
swzmics2021f <- swzmics_female_ind %>%
  select(
    psu = WM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(swzmics2021f$year, na.rm = TRUE) # median interview year 2022

# Adding columns for country, survey ID, med_year
swzmics2021f <- swzmics2021f %>%  mutate(
  country = "Eswatini",
  survey_id = "SWZ2021MICS",
  med_year = 2022)

# Reordering columns to make country, survey_id, median year the leftmost columns
swzmics2021f <- swzmics2021f %>% select(country, survey_id, med_year, everything())

# Converting province into character
swzmics2021f$province <- as.character(swzmics2021f$province)

# Adding the 'sex' column with value 0 (women)
swzmics2021f <- swzmics2021f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
swzmics2021f <- swzmics2021f %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
swzmics2021f <- swzmics2021f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
swzmics2021f <- swzmics2021f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
swzmics2021f <- swzmics2021f %>%
  mutate(schl_years = case_when(
    schl_years %in% c(2, 4) ~ 2,  # Recode 2 and 4 as secondary (2)
    schl_years == 9 ~ NA_real_,   # Recode 9 as NA
    TRUE ~ schl_years             # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
swzmics2021f <- swzmics2021f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
swzmics2021f <- swzmics2021f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
swzmics2021f <- swzmics2021f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
swzmics2021f <- swzmics2021f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
  ))

# recoding ever tested
swzmics2021f <- swzmics2021f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
swzmics2021f <- swzmics2021f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
swzmics2021f <- swzmics2021f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
swzmics2021f <- swzmics2021f %>%
  filter(!is.na(strata))


# # ---- age grp den and num--------
# # Recode the existing agegrp into new age groups
# swzmics2021f <- swzmics2021f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 


# swz_f <- as.data.frame.matrix(table(swzmics2021f$agegroup_new, swzmics2021f$hivst_use))
# swz_f$Total <- rowSums(swz_f)
# swz_f[, sapply(swz_f, is.numeric)] <- swz_f[, sapply(swz_f, is.numeric)] * 0.8


#------------Male-------------

swzmics_male_ind <- read_sav("Eswatini MICS6 Datasets\\Eswatini MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
swzmics2021m <- swzmics_male_ind %>%
  select(
    psu = MWM1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(swzmics2021m$year, na.rm = TRUE) # median interview year 2022

# Adding columns for country, survey ID, med_year
swzmics2021m <- swzmics2021m %>%  mutate(
  country = "Eswatini",
  survey_id = "SWZ2021MICS",
  med_year = 2022)

# Reordering columns to make country, survey_id, median year the leftmost columns
swzmics2021m <- swzmics2021m %>% select(country, survey_id, med_year, everything())

# Converting province into character
swzmics2021m$province <- as.character(swzmics2021m$province)

# Adding the 'sex' column with value 1 (men)
swzmics2021m <- swzmics2021m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
swzmics2021m <- swzmics2021m %>% mutate(ind_wt = ind_wt * 1000000)

# Recoding regions
swzmics2021m <- swzmics2021m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
swzmics2021m <- swzmics2021m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
swzmics2021m <- swzmics2021m %>%
  mutate(schl_years = case_when(
    schl_years %in% c(2, 4) ~ 2,  # Recode 2 and 4 as secondary (2)
    schl_years == 9 ~ NA_real_,   # Recode 9 as NA
    TRUE ~ schl_years             # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
swzmics2021m <- swzmics2021m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
swzmics2021m <- swzmics2021m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
swzmics2021m <- swzmics2021m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# recoding hivst use
swzmics2021m <- swzmics2021m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,          # Keep '1' unchanged
  ever_heard == 1 & hivst_use == 2 ~ 0,          # Recode '2' as '0'
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

# recoding ever tested
swzmics2021m <- swzmics2021m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
swzmics2021m <- swzmics2021m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
swzmics2021m <- swzmics2021m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

#removing NA in strata
swzmics2021m <- swzmics2021m %>%
  filter(!is.na(strata))


#---age grp wise-----
# Recode the existing agegrp into new age groups
# swzmics2021m <- swzmics2021m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# swz_m <- as.data.frame.matrix(table(swzmics2021m$agegroup_new, swzmics2021m$hivst_use))
# swz_m$Total <- rowSums(swz_m)
# swz_m[, sapply(swz_m, is.numeric)] <- swz_m[, sapply(swz_m, is.numeric)] * 0.8



# Combining male and female for Eswatini 2021-22
combined_swzmics <- bind_rows(swzmics2021f, swzmics2021m)


#-------------------------------------------------------------------------

# Zimbabwe 2019

#-------- Female ----------

zwemics_female_ind <- read_sav("Zimbabwe MICS6 SPSS Datasets\\Zimbabwe MICS6 SPSS Datasets\\wm.sav")

# Selecting and renaming variables
zwemics2019f <- zwemics_female_ind %>%
  select(
    psu = HH1,                      # PSU/Cluster
    strata = stratum,               # Strata
    province = HH7,                 # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = wmweight,              # Individual weight
    year = WM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = WB4,                     # Age of the respondent
    agegrp = WAGE,                  # Age in 5y age groups
    curr_marital = MSTATUS,         # Current marital status
    schl_years = welevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = HA1,               # Ever heard of HIV
    hivst_knwldge = HA28,           # Knowledge of HIV self-test
    hivst_use = HA29,               # Use of HIV self-test
    ever_tested = HA24,             # Ever tested for HIV
    received_hivresult =  HA26,     # Whether received result from last HIV test
    last_hivtest = HA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(zwemics2019f$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
zwemics2019f <- zwemics2019f %>%  mutate(
  country = "Zimbabwe",
  survey_id = "ZWE2019MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
zwemics2019f <- zwemics2019f %>% select(country, survey_id, med_year, everything())

# Converting province into character
zwemics2019f$province <- as.character(zwemics2019f$province)

# Adding the 'sex' column with value 0 (women)
zwemics2019f <- zwemics2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
zwemics2019f <- zwemics2019f %>% mutate(ind_wt = ind_wt) #* 1000000)

# Recoding regions
zwemics2019f <- zwemics2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
zwemics2019f <- zwemics2019f %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
zwemics2019f <- zwemics2019f %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,   # Recode 9 as NA
    TRUE ~ schl_years             # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
zwemics2019f <- zwemics2019f %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
zwemics2019f <- zwemics2019f %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))

# recoding hivst knowledge
zwemics2019f <- zwemics2019f %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))



# Recoding hivst use for nested variable
zwemics2019f <- zwemics2019f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,           # Keep '1' unchanged (Yes, used HIVST)
  ever_heard == 1 & hivst_use == 2 ~ 0,           # Recode '2' as '0' (No, didn't use HIVST)
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


# recoding ever tested
zwemics2019f <- zwemics2019f %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
zwemics2019f <- zwemics2019f %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))


# Recoding the months ago last hivtest
zwemics2019f <- zwemics2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # Recode the existing agegrp into new age groups
# zwemics2019f <- zwemics2019f %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# zwemics_design_f <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = zwemics2019f,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# zwe_prop_age_f <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = zwemics_design_f,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# zwe_prop_age_f <- zwe_prop_age_f %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# zwe_prop_age_f <- zwe_prop_age_f %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# zwe_prop_age_f_selected <- zwe_prop_age_f %>%
#   select(agegroup_new, deno, num)


#------------Male-------------

zwemics_male_ind <- read_sav("Zimbabwe MICS6 SPSS Datasets\\Zimbabwe MICS6 SPSS Datasets\\mn.sav")

# Selecting and renaming variables
zwemics2019m <- zwemics_male_ind %>%
  select(
    psu = HH1,                      # PSU/Cluster
    strata = stratum,                # Strata
    province = HH7,                  # Area/Province
    region = HH6,                   # Urban/ Rural Area
    ind_wt = mnweight,              # Individual weight
    year = MWM6Y,                    # Survey year
    hhid = HH2,                     # Household ID
    age = MWB4,                     # Age of the respondent
    agegrp = MWAGE,                  # Age in 5y age groups
    curr_marital = MMSTATUS,         # Current marital status
    schl_years = mwelevel,           # Highest level of schooling
    wealth_index = windex5,         # Wealth quintile
    #exchange_sex : NA,              # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners : NA,            # Number of sex partners in the last 12 months
    ever_heard = MHA1,               # Ever heard of HIV
    hivst_knwldge = MHA28,           # Knowledge of HIV self-test
    hivst_use = MHA29,               # Use of HIV self-test
    ever_tested = MHA24,             # Ever tested for HIV
    received_hivresult = MHA26,     # Whether received result from last HIV test
    last_hivtest = MHA25,            # How many months ago last tested for HIV 
    #last_hivresult : NA            # result of last hiv test
  ) %>%
  filter(!is.na(ever_heard))    #removing NA in ever heard of HIV


# Calculating the median year of interview
median(zwemics2019m$year, na.rm = TRUE) # median interview year 2019

# Adding columns for country, survey ID, med_year
zwemics2019m <- zwemics2019m %>%  mutate(
  country = "Zimbabwe",
  survey_id = "ZWE2019MICS",
  med_year = 2019)

# Reordering columns to make country, survey_id, median year the leftmost columns
zwemics2019m <- zwemics2019m %>% select(country, survey_id, med_year, everything())

# Converting province into character
zwemics2019m$province <- as.character(zwemics2019m$province)

# Adding the 'sex' column with value 1 (men)
zwemics2019m <- zwemics2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
zwemics2019m <- zwemics2019m %>% mutate(ind_wt = ind_wt) #* 1000000)

# Recoding regions
zwemics2019m <- zwemics2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
zwemics2019m <- zwemics2019m %>%
  mutate(curr_marital = case_when(
    curr_marital == 3 ~ 1,   # Single
    curr_marital == 2 ~ 3,   # Divorced/Separated
    curr_marital == 1 ~ 5,   # Married/In a Union
    curr_marital == 9 ~ NA_real_, # Recode 9 as NA
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding schooling
zwemics2019m <- zwemics2019m %>%
  mutate(schl_years = case_when(
    schl_years == 9 ~ NA_real_,   # Recode 9 as NA
    TRUE ~ schl_years             # Keep everything else unchanged
  ))

# Recoding the wealth quintile 
zwemics2019m <- zwemics2019m %>% mutate(wealth_index = case_when(
  wealth_index == 0 | wealth_index == 1 ~ 1,   # Recode '0' or '1' as '1'
  TRUE ~ wealth_index                         # Keep all other values unchanged
)) %>% mutate(wealth_index = factor(wealth_index, 
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# recoding ever heard
zwemics2019m <- zwemics2019m %>% mutate(ever_heard = case_when(
  ever_heard == 1 ~ 1,           # Keep '1' unchanged
  ever_heard == 2 ~ 0,           # Recode '2' as '0'
  ever_heard == 9 ~ NA_real_,    # Recode '9' as NA
))


# recoding hivst knowledge
zwemics2019m <- zwemics2019m %>% mutate(hivst_knwldge = case_when(
  ever_heard == 1 & hivst_knwldge == 1 ~ 1,         # Keep '1' unchanged
  ever_heard == 1 & hivst_knwldge == 2 ~ 0,         # Recode '2' as '0'
  hivst_knwldge == 9 ~ NA_real_,  # Recode '9' as NA
  is.na(hivst_knwldge) ~ 0          # Recode all NAs as 0
))

# Recode hivst_use for nested variable
zwemics2019m <- zwemics2019m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,           # Keep '1' unchanged (Yes, used HIVST)
  ever_heard == 1 & hivst_use == 2 ~ 0,           # Recode '2' as '0' (No, didn't use HIVST)
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


# recoding ever tested
zwemics2019m <- zwemics2019m %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,          # Keep '1' unchanged
    ever_tested == 2 ~ 0,          # Recode '2' as '0'
    ever_tested == 9 ~ NA_real_,   # Recode '9' as NA
  ))

# recoding received hiv result
zwemics2019m <- zwemics2019m %>%
  mutate(received_hivresult = case_when(
    received_hivresult == 1 ~ 1,          # Keep '1' unchanged
    received_hivresult == 2 ~ 0,          # Recode '2' as '0'
    received_hivresult == 8 ~ NA_real_,   # Recode '8' as NA
  ))

# Recoding the months ago last hivtest
zwemics2019m <- zwemics2019m %>%
  mutate(last_hivtest = case_when(
    last_hivtest == 9 ~ NA_real_,    # Recode '9' as NA
    TRUE ~ last_hivtest              # Keep all other values unchanged
  ))

# # Recode the existing agegrp into new age groups
# zwemics2019m <- zwemics2019m %>%
#   mutate(
#     agegroup_new = case_when(
#       agegrp %in% c(1, 2) ~ "Group 1 (Age 15-24)",
#       agegrp %in% c(3, 4) ~ "Group 2 (Age 25-34)",
#       agegrp %in% c(5, 6, 7) ~ "Group 3 (Age 35+)",
#       TRUE ~ NA_character_  # Handle unexpected values
#     )
#   )
# 
# zwemics_design_m <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = zwemics2019m,
#   nest = TRUE
# )
# 
# 
# # Calculate the proportion of HIV testing usage by the new age groups
# zwe_prop_age_m <- svyby(
#   ~I(hivst_use == 1),
#   ~agegroup_new,
#   design = zwemics_design_m,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )
# 
# # Rename the resulting columns for clarity
# zwe_prop_age_m <- zwe_prop_age_m %>%
#   rename(
#     prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator (deno) and numerator (num)
# zwe_prop_age_m <- zwe_prop_age_m %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  # Calculate denominator
#     num = prop * deno                          # Calculate numerator
#   )
# 
# zwe_prop_age_m_selected <- zwe_prop_age_m %>%
#   select(agegroup_new, deno, num)
# 



# Combining male and female for Zimbabwe 2019
combined_zwemics <- bind_rows(zwemics2019f, zwemics2019m)



# hivst prop (female: 0.055, male: 0.053) matches report
#weighted.mean(zwemics2019f$hivst_use, zwemics2019f$ind_wt, na.rm = T)
#weighted.mean(zwemics2019m$hivst_use, zwemics2019m$ind_wt, na.rm = T)

#combined_zwemics$sex <- factor(combined_zwemics$sex, levels = c(0, 1), labels = c("Female", "Male"))



#Female:  Num: 559.25, Deno:  10129, Male: Num:221.48 , Deno: 4178





#-------------------------------------------------------------------------------

# Combine all mics together

list_mics <- list(
 combined_cafmics,
  combined_tcdmics,
 combined_codmics,
  combined_gmbmics,
  combined_ghamics,
  combined_gnbmics,
  combined_mdgmics,
  combined_mwimics,
  combined_stpmics,
  combined_slemics,
  combined_tgomics,
  combined_commics,
  combined_swzmics,
  combined_zwemics
)

# Combine all mics female

list_mics_f <- list(
  cafmics2018f,
  tcdmics2019f,
  codmics2017f,
  gmbmics2018f,
  ghamics2017f,
  gnbmics2018f,
  mdgmics2018f,
  mwimics2019f,
  stpmics2019f,
  slemics2017f,
  tgomics2017f,
  commics2022f,
  swzmics2021f,
  zwemics2019f
)

# Combine all mics male

list_mics_m <- list(
  cafmics2018m,
  tcdmics2019m,
  codmics2017m,
  gmbmics2018m,
  ghamics2017m,
  gnbmics2018m,
  mdgmics2018m,
  mwimics2019m,
  stpmics2019m,
  slemics2017m,
  tgomics2017m,
  commics2022m,
  swzmics2021m,
  zwemics2019m
)


# Saving
#saveRDS(list_mics, file = "list_mics.rds")






