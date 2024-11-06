# check NA mics and fix codes accordingly

# Clearing all objects
rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(labelled)
library(survey)

setwd("D:/Downloads/MSc Thesis/hiv-selftesting/1. thesis rawdata/MICS raw data")

# TCD MICS6

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

# recoding hivst use
tcdmics2019f <- tcdmics2019f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))

# Remove rows where strata is NA 
tcdmics2019f_cleaned <- tcdmics2019f %>%
  filter(!is.na(strata))


#checking with survey package  #0.0258
survey_designs_f <- svydesign(
  ids = ~psu, weights = ~ind_wt, data = tcdmics2019f_cleaned, nest = TRUE)
prop_f <- svyciprop(~I(hivst_use == 1), design = survey_designs_f, method = "logit", level = 0.95)

prop_f

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
  )

#removing NA in ever heard of HIV
tcdmics2019m <- tcdmics2019m %>%
  filter(!is.na(ever_heard))

# recoding hivst use
tcdmics2019m <- tcdmics2019m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as 0
))


# Remove rows where strata is NA 
tcdmics2019m_cleaned <- tcdmics2019m %>%
  filter(!is.na(strata))

# checking proportion of hivst use: TCD male # 0.025
weighted.mean(tcdmics2019m_cleaned$hivst_use, tcdmics2019m_cleaned$ind_wt, na.rm = TRUE) #0.02579

#checking with survey package  # 0.0202 (0.0147 0.0276)
survey_designs <- svydesign(
  ids = ~psu, weights = ~ind_wt, data = tcdmics2019m_cleaned, nest = TRUE)
prop_m <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)

prop_m

#combine male and female (0.0245)
#combinedtcd <- bind_rows(tcdmics2019f_cleaned, tcdmics2019m_cleaned)

#s_com <- svydesign(
 # ids = ~psu, weights = ~ind_wt, data = combinedtcd, nest = TRUE)
#prop_com <- svyciprop(~I(hivst_use == 1), design = s_com, method = "logit", level = 0.95)



# Eswatini MICS6
# Female

swzmics_female_ind <- read_sav("Eswatini MICS6 Datasets\\Eswatini MICS6 SPSS Datasets\\wm.sav")

# checking unweighted people who used HIVST manually
#numerator <- sum(table(swzmics_female_ind$HA29, swzmics_female_ind$WAGE)[1, 1:7])
#denominator <- sum(table(swzmics_female_ind$WAGE)[1:7])
#proportion <- numerator / denominator


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
  ) 


#removing NA in ever heard of HIV
swzmics2021f <- swzmics2021f %>%
  filter(!is.na(ever_heard))

# recoding hivst use
swzmics2021f <- swzmics2021f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,   #have heard of HIV but didn't use hivst
  hivst_use == 9 ~ NA_real_,    # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


#checking if NA in strata is non-interview (yes they are)
na_strata_swzf <- swzmics2021f %>%
  filter(is.na(strata))

# Removing rows where strata is NA 
swzf_cleaned <- swzmics2021f %>%
  filter(!is.na(strata))


#checking proportion : 0.31 (almost matches)
weighted.mean(swzf_cleaned$hivst_use, swzf_cleaned$ind_wt, na.rm = T)


#checking with survey design (also 0.31 almost matches with report)
swz_design_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = swzf_cleaned, nest = TRUE)
prop_swzf <- svyciprop(~I(hivst_use == 1), design = swz_design_f, method = "logit", level = 0.95)

prop_swzf

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
  ) 

#removing NA in ever heard of HIV
swzmics2021m <- swzmics2021m %>%
  filter(!is.na(ever_heard))

# recoding hivst use
swzmics2021m <- swzmics2021m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


#checking if NA in strata is non-interview (yes they are)
na_strata_swzm <- swzmics2021m %>%
  filter(is.na(strata))

# Remove rows where strata is NA 
swzm_cleaned <- swzmics2021m %>%
  filter(!is.na(strata)  & !is.na(ever_heard))


#checking proportion (0.256) matches report
weighted.mean(swzm_cleaned$hivst_use, swzm_cleaned$ind_wt, na.rm = T)


swz_design_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = swzm_cleaned, nest = TRUE)
prop_swzm <- svyciprop(~I(hivst_use == 1), design = swz_design_m, method = "logit", level = 0.95)

prop_swzm


# sierra leone

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
  )

#removing NA in ever heard of HIV
slemics2017f <- slemics2017f %>%
  filter(!is.na(ever_heard))

# recoding hivst use
slemics2017f <- slemics2017f %>% mutate(hivst_use = case_when(
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,   
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


#checking if NA in strata is non-interview (yes they are)
na_strata_slef <- slemics2017f %>%
  filter(is.na(strata))

# Remove rows where strata is NA 
slef_cleaned <- slemics2017f %>%
  filter(!is.na(strata))

# Adding the 'sex' column with value 1 (men)
slemics2017m <- slemics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

#checking proportion, yes matches report (0.0323) 
weighted.mean(slef_cleaned$hivst_use, slef_cleaned$ind_wt, na.rm = T)

#also matches the report : 0.0323 (0.0278, 0.0375)
sle_design_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = slef_cleaned, nest = TRUE)
prop_slef <- svyciprop(~I(hivst_use == 1), design = sle_design_f, method = "logit", level = 0.95)

prop_slef

#--------Male----------

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
  )

#removing NA in ever heard of HIV
slemics2017m <- slemics2017m %>%
  filter(!is.na(ever_heard))

# Adding the 'sex' column with value 1 (men)
slemics2017m <- slemics2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# recoding hivst use
slemics2017m <- slemics2017m %>% mutate(hivst_use = case_when(
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

#checking if NA in strata is non-interview (yes they are)
na_strata_slem <- slemics2017m %>%
  filter(is.na(strata))

# Remove rows where strata is NA 
slem_cleaned <- slemics2017m %>%
  filter(!is.na(strata))


#checking proportion (0,02) matches report
weighted.mean(slem_cleaned$hivst_use, slem_cleaned$ind_wt, na.rm = T)

#also matches from survey package : 0.02 (0.0155, 0.0267)
sle_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = slem_cleaned, nest = TRUE)

prop_slem <- svyciprop(~I(hivst_use == 1), design = sle_m, method = "logit", level = 0.95)

prop_slem



# HIVST use by sex (female: 0,032, male: 0.02: matches report)
sle_design <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = combined_slemics, nest = TRUE)

hivst_by_sex <- svyby(
  ~I(hivst_use == 1),    # The condition for HIVST use
  ~sex,                  # Group by sex
  sle_design,         # Your survey design object
  svyciprop,             # Use svyciprop for proportions
  method = "logit",      # Method for calculating CI (logit or linearized)
  vartype = "ci"         # Include confidence intervals
)

# Togo MICS6


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
  )

#removing NA in ever heard of HIV
tgomics2017f <- tgomics2017f %>%
  filter(!is.na(ever_heard))

# recoding hivst use
tgomics2017f <- tgomics2017f %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))

#checking if NA in strata is non-interview (yes they are)
na_strata_tgof <- tgomics2017f %>%
  filter(is.na(strata))

# Remove rows where strata is NA 
tgof_cleaned <- tgomics2017f %>%
  filter(!is.na(strata))


#checking proportion matches report (0.017)
weighted.mean(tgof_cleaned$hivst_use, tgof_cleaned$ind_wt, na.rm = T)

#also matches the report
tgo_design_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = tgof_cleaned, nest = TRUE)
prop_tgof <- svyciprop(~I(hivst_use == 1), design = tgo_design_f, method = "logit", level = 0.95)

prop_tgof

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
  )

#removing NA in ever heard of HIV
tgomics2017m <- tgomics2017m %>%
  filter(!is.na(ever_heard))

# recoding hivst use
tgomics2017m <- tgomics2017m %>% mutate(hivst_use = case_when(
  ever_heard == 1 & hivst_use == 1 ~ 1,   # They have heard of HIV and used self-test
  ever_heard == 1 & hivst_use == 2 ~ 0,
  hivst_use == 9 ~ NA_real_,   # Recode '9' as NA
  is.na(hivst_use) ~ 0          # Recode all NAs as '0' (consider non-respondents as non-users)
))


#checking if NA in strata is non-interview (yes they are)
na_strata_tgom <- tgomics2017m %>%
  filter(is.na(strata))

# Remove rows where strata is NA 
tgom_cleaned <- tgomics2017m %>%
  filter(!is.na(strata))


#checking proportion matches report (0.007)
weighted.mean(tgom_cleaned$hivst_use, tgom_cleaned$ind_wt, na.rm = T)

#also matches the report (0.007)
tgo_design_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = tgom_cleaned, nest = TRUE)
prop_tgom <- svyciprop(~I(hivst_use == 1), design = tgo_design_m, method = "logit", level = 0.95)

prop_tgom


# Summary: need to remove NAs for ever_heard
#matches for chad (m,f), 
#togo(m,f), 
#sierra leone (f,m),
#eswatini (f,m)







