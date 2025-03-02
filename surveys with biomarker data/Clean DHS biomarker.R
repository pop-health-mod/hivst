# Separate script for DHS surveys with biomarker data

# Gabon has biomarker data but no program data so didn't include

rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(labelled)
library(survey)

setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/DHS raw data")

#---------------------------------------------------------------------------------------

# Burundi 2016-17 (Has HIV Biomarket dataset)

#-----Female------
bdidhs_female_ind <- read_dta("Burundi 2016-17_DHS/BUIR71DT/BUIR71FL.DTA")
bdidhs_female_bio <- read_dta("Burundi 2016-17_DHS/BUAR71DT/BUAR71FL.DTA")

# merging by cluster number, household number and line number
merged_burundi_female <- left_join(bdidhs_female_ind, bdidhs_female_bio, 
                                   by = c("v001" = "hivclust", 
                                          "v002" = "hivnumb", 
                                          "v003" = "hivline"))

# Selecting and renaming variables
bdidhs2016f <- merged_burundi_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 


# Calculating the median year of interview
median(bdidhs2016f$year, na.rm = TRUE) #median year 2016

# Adding columns for country, survey ID
bdidhs2016f <- bdidhs2016f %>%
  mutate(
    country = "Burundi",
    survey_id = "BDI2016DHS",
    med_year = 2016
  )

# Reordering columns to make country and survey_id the leftmost columns
bdidhs2016f <- bdidhs2016f %>% select(country, survey_id, med_year, everything())

# Recoding province
bdidhs2016f$province <- as.character(bdidhs2016f$province)

# Adding the 'sex' column with value 0 (women)
bdidhs2016f <- bdidhs2016f %>%
  mutate(sex = 0) %>%             # Assign value 0 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Correcting individual and HIV weights 
bdidhs2016f <- bdidhs2016f %>%
  mutate(
    ind_wt = ind_wt / 1e6,
    hiv_wt = hiv_wt / 1e6
  )

# Creating the hivst_knwldge and hivst_use variables
bdidhs2016f <- bdidhs2016f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
bdidhs2016f <- bdidhs2016f %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bdidhs2016f <- bdidhs2016f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bdidhs2016f <- bdidhs2016f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile the specified labels
bdidhs2016f <- bdidhs2016f %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the hiv_status variable
bdidhs2016f <- bdidhs2016f %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))


# Recoding the last_hivtest
bdidhs2016f <- bdidhs2016f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#----raw (survey unadjusted) denominator and numerator for hivst use and hiv blood test result---

# table(bdidhs2016f$hiv_status) # 3 means test inconclusive
# table(bdidhs2016f$hivst_use) 
# 
# table(bdidhs2016f$hiv_status, bdidhs2016f$hivst_use) # rows hiv status col hivst use

# 0.0026 among hiv status=0, 0.0084 among hiv status=1

#-----Male------

bdidhs_male_ind <- read_dta("Burundi 2016-17_DHS/BUMR71DT/BUMR71FL.DTA")
bdidhs_male_bio <- read_dta("Burundi 2016-17_DHS/BUAR71DT/BUAR71FL.DTA")

# merging by cluster number, household number and line number
merged_burundi_male <- left_join(bdidhs_male_ind, bdidhs_male_bio, 
                                 by = c("mv001" = "hivclust", 
                                        "mv002" = "hivnumb", 
                                        "mv003" = "hivline"))

# Selecting and renaming variables
bdidhs2016m <- merged_burundi_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(bdidhs2016m$year, na.rm = TRUE) #median year 2016

# Adding columns for country, survey ID
bdidhs2016m <- bdidhs2016m %>%
  mutate(
    country = "Burundi",
    survey_id = "BDI2016DHS",
    med_year = 2016
  )

# Reordering columns to make country and survey_id the leftmost columns
bdidhs2016m <- bdidhs2016m %>% select(country, survey_id, med_year, everything())

# Recoding province
bdidhs2016m$province <- as.character(bdidhs2016m$province)


# Adding the 'sex' column with value 1 (men)
bdidhs2016m <- bdidhs2016m %>%
  mutate(sex = 1) %>%             # Assign value 1 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Correcting individual and HIV weights 
bdidhs2016m <- bdidhs2016m %>%
  mutate(
    ind_wt = ind_wt / 1e6,
    hiv_wt = hiv_wt / 1e6
  )

# Creating the hivst_knwldge and hivst_use variables
bdidhs2016m <- bdidhs2016m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
bdidhs2016m <- bdidhs2016m %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bdidhs2016m <- bdidhs2016m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bdidhs2016m <- bdidhs2016m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile for specified labels
bdidhs2016m <- bdidhs2016m %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the hiv_status variable
bdidhs2016m <- bdidhs2016m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))

# Recoding the last_hivtest
bdidhs2016m <- bdidhs2016m %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


# checking raw proportions for my own clarity
# table(bdidhs2016m$hiv_status) # 3 means test inconclusive
# table(bdidhs2016m$hivst_use) 
# 
# table(bdidhs2016m$hiv_status, bdidhs2016m$hivst_use) # rows hiv status col hivst use

#among hiv negative, proportion 32/(32+7268)= 0.00438
#among hiv positive, proportion 2/(2+51)= 0.0377

# Combining male and female for Burundi
combined_bdidhs <- bind_rows(bdidhs2016f, bdidhs2016m)


#---function for hiv status and hivst use proportion survey unadjusted and adjusted ---
# table(combined_bdidhs$hiv_status, combined_bdidhs$hivst_use) # rows hiv status col hivst use

#among hiv negative, proportion 54/(54+15619)= 0.0034
#among hiv positive, proportion 2/(3+3+169)= 0.0174

# survey adjusted proportion of HIVST use by hiv blood test status
# note we don't want hivstatus=3(inconclusive) in our numerator and denominator

# bdidhs_design <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = subset(combined_bdidhs, hiv_status %in% c(0, 1)),
#   nest = TRUE
# )
# 
# bdi_prop_hiv <- svyby(
#   ~I(hivst_use == 1),
#   ~hiv_status,
#   design = bdidhs_design,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )

# matches closely with survey unadjusted (0.003 for hiv(-)ve, 0.02 for hiv(+)ve)

# the section below is only if we need to use survey adjusted numerator and denominator for our mathematical model in future
# bdi_prop_hiv <- bdi_prop_hiv %>%
#    rename(
#    prop = `I(hivst_use == 1)`,
#     se_prop = `se.as.numeric(I(hivst_use == 1))`
#   )
# 
# 
# # Calculate denominator and numerator 
# bdi_prop_hiv <- bdi_prop_hiv %>%
#   mutate(
#     deno = (prop * (1 - prop)) / (se_prop^2),  
#     num = prop * deno                          
#   )
# 
# bdi_prop_hiv <- bdi_prop_hiv %>%
#   select(hiv_status, deno, num)
# 
# bdi_prop_hiv 

#----------------------------------------------------------------------------------------

# Cameroon 2018 (Has HIV Biomarket dataset)

#-----Female------
cmrdhs_female_ind <- read_dta("Cameroon_2018_DHS\\CMIR71DT\\CMIR71FL.DTA")
cmrdhs_female_bio <- read_dta("Cameroon_2018_DHS\\CMAR71DT\\CMAR71FL.DTA")

# merging by cluster number, household number and line number
merged_cameroon_female <- left_join(cmrdhs_female_ind, cmrdhs_female_bio, 
                                    by = c("v001" = "hivclust", 
                                           "v002" = "hivnumb", 
                                           "v003" = "hivline"))

# Selecting and renaming variables
cmrdhs2018f <- merged_cameroon_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    last_hivresult = s1018a         # result of last hiv test
  ) 

# Calculating the median year of interview
median(cmrdhs2018f$year, na.rm = TRUE) #median year 2018

# Adding columns for country, survey ID
cmrdhs2018f <- cmrdhs2018f %>% mutate(
  country = "Cameroon",survey_id = "CMR2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
cmrdhs2018f <- cmrdhs2018f %>%
  select(country, survey_id, med_year, everything())


# Adding the 'sex' column with value 0 (women)
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(sex = 0) %>%             # Assign value 0 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Recoding province
cmrdhs2018f$province <- as.character(cmrdhs2018f$province)

# Correcting individual and HIV weights 
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
cmrdhs2018f <- cmrdhs2018f %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))


# Recoding the hiv_status variable
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))


# Recoding the last_hivtest
cmrdhs2018f <- cmrdhs2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))

# Recoding the last hiv result
cmrdhs2018f <- cmrdhs2018f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 8 ~ 88,  # Dont know
  TRUE ~ NA_real_            # Missing or other values
))


#-----Male------

cmrdhs_male_ind <- read_dta("Cameroon_2018_DHS\\CMMR71DT\\CMMR71FL.DTA")
cmrdhs_male_bio <- read_dta("Cameroon_2018_DHS\\CMAR71DT\\CMAR71FL.DTA")

# merging by cluster number, household number and line number
merged_cameroon_male <- left_join(cmrdhs_male_ind, cmrdhs_male_bio, 
                                  by = c("mv001" = "hivclust", 
                                         "mv002" = "hivnumb", 
                                         "mv003" = "hivline"))
# Selecting and renaming variables
cmrdhs2018m <- merged_cameroon_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV
    last_hivresult = sm714a          # result of last hiv test
  ) 

# Calculating the median year of interview
median(cmrdhs2018m$year, na.rm = TRUE) 

# Adding columns for country, survey ID
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(
    country = "Cameroon",
    survey_id = "CMR2018DHS",
    med_year = 2018
  )

# Reordering columns to make country and survey_id the leftmost columns
cmrdhs2018m <- cmrdhs2018m %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(sex = 1) %>%             # Assign value 1 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column


# Recoding province
cmrdhs2018m$province <- as.character(cmrdhs2018m$province)


# Correcting individual and HIV weights 
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
cmrdhs2018m <- cmrdhs2018m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile for specified labels
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))

# Recoding the last_hivtest
cmrdhs2018m <- cmrdhs2018m %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))

# Recoding the last hiv result
cmrdhs2018m <- cmrdhs2018m %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 8 ~ 88,  # Dont know
  TRUE ~ NA_real_            # Missing or other values
))


# Combining male and female for cameroon
combined_cmrdhs <- bind_rows(cmrdhs2018f, cmrdhs2018m)

#---function for hiv status and hivst use proportion survey unadjusted and adjusted ---
# table(combined_cmrdhs$hiv_status, combined_cmrdhs$hivst_use) # rows hiv status col hivst use

#among hiv negative, proportion 331/(331+13268) = 0.0243
#among hiv positive, proportion 6/(16+444) = 0.0347

# survey adjusted proportion of HIVST use by hiv blood test status
# note we don't want hivstatus=3(inconclusive) in our numerator and denominator

# cmrdhs_design <- svydesign(
#   ids = ~psu,
#   strata = ~strata,
#   weights = ~ind_wt,
#   data = subset(combined_cmrdhs, hiv_status %in% c(0, 1)),
#   nest = TRUE
# )
# 
# cmr_prop_hiv <- svyby(
#   ~I(hivst_use == 1),
#   ~hiv_status,
#   design = cmrdhs_design,
#   FUN = svyciprop,
#   method = "logit",
#   vartype = "se",
#   level = 0.95
# )




#-------------------------------------------------------------------------------

# Guinea 2018 (has HIV biomarker data)

#-----Female------

gindhs_female_ind <- read_dta("Guinea_2018_DHS\\GNIR71DT\\GNIR71FL.DTA")
gindhs_female_bio <- read_dta("Guinea_2018_DHS\\GNAR71DT\\GNAR71FL.DTA")

# merging by cluster number, household number and line number
merged_guinea_female <- left_join(gindhs_female_ind, gindhs_female_bio, 
                                  by = c("v001" = "hivclust", 
                                         "v002" = "hivnumb", 
                                         "v003" = "hivline"))

# Selecting and renaming variables
gindhs2018f <- merged_guinea_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a           # How many months ago last tested for HIV
  ) 

# Calculating the median year of interview
median(gindhs2018f$year, na.rm = TRUE) 

# Adding columns for country, survey ID
gindhs2018f <- gindhs2018f %>% mutate(
  country = "Guinea",survey_id = "GIN2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
gindhs2018f <- gindhs2018f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
gindhs2018f <- gindhs2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
gindhs2018f <- gindhs2018f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
gindhs2018f$province <- as.character(gindhs2018f$province)

# Creating the hivst_knwldge and hivst_use variables
gindhs2018f <- gindhs2018f %>% mutate(
  hivst_knwldge = case_when(
    selftest_dhs == 0 ~ 0,
    selftest_dhs %in% c(1, 2) ~ 1,
    TRUE ~ 0
  ),
  hivst_use = case_when(
    selftest_dhs == 1 ~ 1,
    TRUE ~ 0
  )
)

# moving to the right of the selftest_dhs column
gindhs2018f <- gindhs2018f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
gindhs2018f <- gindhs2018f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gindhs2018f <- gindhs2018f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile the specified labels
gindhs2018f <- gindhs2018f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
gindhs2018f <- gindhs2018f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the hiv_status variable
gindhs2018f <- gindhs2018f %>% mutate(hiv_status = case_when(
  hiv_status == 7 ~ 3,  # Recode 7 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))

# Recoding the last_hivtest
gindhs2018f <- gindhs2018f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))

#-----Male------

gindhs_male_ind <- read_dta("Guinea_2018_DHS\\GNMR71DT\\GNMR71FL.DTA")
gindhs_male_bio <- read_dta("Guinea_2018_DHS\\GNAR71DT\\GNAR71FL.DTA")

# merging by cluster number, household number and line number
merged_guinea_male <- left_join(gindhs_male_ind, gindhs_male_bio, 
                                by = c("mv001" = "hivclust", 
                                       "mv002" = "hivnumb", 
                                       "mv003" = "hivline"))
# Selecting and renaming variables
gindhs2018m <- merged_guinea_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a           # How many months ago last tested for HIV
  ) 

# Calculating the median year of interview
median(gindhs2018m$year, na.rm = TRUE) 

# Adding columns for country, survey ID
gindhs2018m <- gindhs2018m %>% mutate(
  country = "Guinea", survey_id = "GIN2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
gindhs2018m <- gindhs2018m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
gindhs2018m <- gindhs2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
gindhs2018m <- gindhs2018m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
gindhs2018m$province <- as.character(gindhs2018m$province)

# Creating the hivst_knwldge and hivst_use variables
gindhs2018m <- gindhs2018m %>% mutate(
  hivst_knwldge = case_when(
    selftest_dhs == 0 ~ 0,
    selftest_dhs %in% c(1, 2) ~ 1,
    TRUE ~ 0
  ),
  hivst_use = case_when(
    selftest_dhs == 1 ~ 1,
    TRUE ~ 0
  )
)

# moving to the right of the selftest_dhs column
gindhs2018m <- gindhs2018m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
gindhs2018m <- gindhs2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
gindhs2018m <- gindhs2018m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile for specified labels
gindhs2018m <- gindhs2018m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
gindhs2018m <- gindhs2018m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the hiv_status variable
gindhs2018m <- gindhs2018m %>% mutate(hiv_status = case_when(
  hiv_status == 7 ~ 3,  # Recode 7 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))

# Recoding the last_hivtest
gindhs2018m <- gindhs2018m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))

# Combining male and female for Guinea
combined_gindhs <- bind_rows(gindhs2018f, gindhs2018m)



#------------------------------------------------------------------------------

# Malawi 2015-16 (has biomarker data)

#-----Female------

mwidhs_female_ind <- read_dta("Malawi_2015-16_DHS\\MWIR7ADT\\MWIR7AFL.DTA")
mwidhs_female_bio <- read_dta("Malawi_2015-16_DHS\\MWAR7ADT\\MWAR7AFL.DTA")

# merging by cluster number, household number and line number
merged_malawi_female <- left_join(mwidhs_female_ind, mwidhs_female_bio, 
                                  by = c("v001" = "hivclust", 
                                         "v002" = "hivnumb", 
                                         "v003" = "hivline"))

# Selecting and renaming variables
mwidhs2015f <- merged_malawi_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(mwidhs2015f$year, na.rm = TRUE) #median year 2015


# Adding columns for country, survey ID
mwidhs2015f <- mwidhs2015f %>% mutate(country = "Malawi",
                                      survey_id = "MWI2015DHS",
                                      med_year = 2015)

# Reordering columns to make country and survey_id the leftmost columns
mwidhs2015f <- mwidhs2015f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
mwidhs2015f <- mwidhs2015f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
mwidhs2015f <- mwidhs2015f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
mwidhs2015f$province <- as.character(mwidhs2015f$province)


# Creating the hivst_knwldge and hivst_use variables
mwidhs2015f <- mwidhs2015f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
mwidhs2015f <- mwidhs2015f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mwidhs2015f <- mwidhs2015f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
mwidhs2015f <- mwidhs2015f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
mwidhs2015f <- mwidhs2015f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
mwidhs2015f <- mwidhs2015f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
mwidhs2015f <- mwidhs2015f %>% mutate(hiv_status = case_when(
  hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))


# Recoding the last_hivtest
mwidhs2015f <- mwidhs2015f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

mwidhs_male_ind <- read_dta("Malawi_2015-16_DHS\\MWMR7ADT\\MWMR7AFL.DTA")
mwidhs_male_bio <- read_dta("Malawi_2015-16_DHS\\MWAR7ADT\\MWAR7AFL.DTA")

# merging by cluster number, household number and line number
merged_malawi_male <- left_join(mwidhs_male_ind, mwidhs_male_bio, 
                                by = c("mv001" = "hivclust", 
                                       "mv002" = "hivnumb", 
                                       "mv003" = "hivline"))
# Selecting and renaming variables
mwidhs2015m <- merged_malawi_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(mwidhs2015m$year, na.rm = TRUE) #median year 2015

# Adding columns for country, survey ID
mwidhs2015m <- mwidhs2015m %>% mutate(country = "Malawi", 
                                      survey_id = "MWI2015DHS",
                                      med_year = 2015)

# Reordering columns to make country and survey_id the leftmost columns
mwidhs2015m <- mwidhs2015m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
mwidhs2015m <- mwidhs2015m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
mwidhs2015m <- mwidhs2015m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
mwidhs2015m$province <- as.character(mwidhs2015m$province)


# Creating the hivst_knwldge and hivst_use variables
mwidhs2015m <- mwidhs2015m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
mwidhs2015m <- mwidhs2015m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mwidhs2015m <- mwidhs2015m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
mwidhs2015m <- mwidhs2015m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
mwidhs2015m <- mwidhs2015m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
mwidhs2015m <- mwidhs2015m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
mwidhs2015m <- mwidhs2015m %>%
  mutate(hiv_status = case_when(
    hiv_status %in% c(9, 7) ~ 3,  # Recode 9 or 7 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
mwidhs2015m <- mwidhs2015m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))


# Combining male and female for Malawi
combined_mwidhs <- bind_rows(mwidhs2015f, mwidhs2015m)

#-------------------------------------------------------------------------------------

# Senegal 2017

#-----Female------

sendhs_female_ind <- read_dta("Senegal_2017_CONTINUOUSDHS\\SNIR7ZDT\\SNIR7ZFL.DTA")
sendhs_female_bio <- read_dta("Senegal_2017_CONTINUOUSDHS\\SNAR7RDT\\SNAR7RFL.DTA")

# merging by cluster number, household number and line number
merged_senegal_female <- left_join(sendhs_female_ind, sendhs_female_bio, 
                                   by = c("v001" = "hivclust", 
                                          "v002" = "hivnumb", 
                                          "v003" = "hivline"))

# Selecting and renaming variables
sendhs2017f <- merged_senegal_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(sendhs2017f$year, na.rm = TRUE) #2017

# Adding columns for country, survey ID
sendhs2017f <- sendhs2017f %>% mutate(
  country = "Senegal",survey_id = "SEN2017DHS", med_year = 2017)

# Reordering columns to make country and survey_id the leftmost columns
sendhs2017f <- sendhs2017f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
sendhs2017f <- sendhs2017f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
sendhs2017f <- sendhs2017f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
sendhs2017f$province <- as.character(sendhs2017f$province)


# Creating the hivst_knwldge and hivst_use variables
sendhs2017f <- sendhs2017f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
sendhs2017f <- sendhs2017f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
sendhs2017f <- sendhs2017f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
sendhs2017f <- sendhs2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
sendhs2017f <- sendhs2017f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the schl_years variable
sendhs2017f <- sendhs2017f %>%
  mutate(schl_years = case_when(
    schl_years == 6 ~ 88,  # Recode 6 as 88 (don't know)
    is.na(schl_years) ~ NA_real_,  # Ensure missing values remain NA
    TRUE ~ schl_years  # Keep other values the same
  ))

# Recoding the total_partners
sendhs2017f <- sendhs2017f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
sendhs2017f <- sendhs2017f %>% mutate(hiv_status = case_when(
  hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))

# Recoding the last_hivtest
sendhs2017f <- sendhs2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

sendhs_male_ind <- read_dta("Senegal_2017_CONTINUOUSDHS\\SNMR7ZDT\\SNMR7ZFL.DTA")
sendhs_male_bio <- read_dta("Senegal_2017_CONTINUOUSDHS\\SNAR7RDT\\SNAR7RFL.DTA")

# merging by cluster number, household number and line number
merged_senegal_male <- left_join(sendhs_male_ind, sendhs_male_bio, 
                                 by = c("mv001" = "hivclust", 
                                        "mv002" = "hivnumb", 
                                        "mv003" = "hivline"))
# Selecting and renaming variables
sendhs2017m <- merged_senegal_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(sendhs2017m$year, na.rm = TRUE) #2017

# Adding columns for country, survey ID
sendhs2017m <- sendhs2017m %>% mutate(
  country = "Senegal", survey_id = "SEN2017DHS", med_year = 2017)

# Reordering columns to make country and survey_id the leftmost columns
sendhs2017m <- sendhs2017m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
sendhs2017m <- sendhs2017m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
sendhs2017m <- sendhs2017m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
sendhs2017m$province <- as.character(sendhs2017m$province)


# Creating the hivst_knwldge and hivst_use variables
sendhs2017m <- sendhs2017m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
sendhs2017m <- sendhs2017m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
sendhs2017m <- sendhs2017m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
sendhs2017m <- sendhs2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
sendhs2017m <- sendhs2017m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
sendhs2017m <- sendhs2017m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
sendhs2017m <- sendhs2017m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 or 7 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
sendhs2017m <- sendhs2017m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:24) ~ 2, # Tested 1-2 years ago
  last_hivtest %in% c(95:96) ~ 3, # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))


# Combining male and female for Senegal
combined_sendhs <- bind_rows(sendhs2017f, sendhs2017m)

#------------------------------------------------------------------------------

# Sierra Leone 2019

#-----Female------

sledhs_female_ind <- read_dta("Sierra Leone_2019_DHS\\SLIR7ADT\\SLIR7AFL.DTA")
sledhs_female_bio <- read_dta("Sierra Leone_2019_DHS\\SLAR7ADT\\SLAR7AFL.DTA")

# merging by cluster number, household number and line number
merged_sierraleone_female <- left_join(sledhs_female_ind, sledhs_female_bio, 
                                       by = c("v001" = "hivclust", 
                                              "v002" = "hivnumb", 
                                              "v003" = "hivline"))

# Selecting and renaming variables
sledhs2019f <- merged_sierraleone_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(sledhs2019f$year, na.rm = TRUE) #2019

# Adding columns for country, survey ID
sledhs2019f <- sledhs2019f %>% mutate(
  country = "Sierra Leone",survey_id = "SLE2019DHS", med_year = 2019)

# Reordering columns to make country and survey_id the leftmost columns
sledhs2019f <- sledhs2019f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
sledhs2019f <- sledhs2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
sledhs2019f <- sledhs2019f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
sledhs2019f$province <- as.character(sledhs2019f$province)


# Creating the hivst_knwldge and hivst_use variables
sledhs2019f <- sledhs2019f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
sledhs2019f <- sledhs2019f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
sledhs2019f <- sledhs2019f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
sledhs2019f <- sledhs2019f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
sledhs2019f <- sledhs2019f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
sledhs2019f <- sledhs2019f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
sledhs2019f <- sledhs2019f %>% mutate(hiv_status = case_when(
  hiv_status == 7 ~ 3,  # Recode 9 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))

# Recoding the last_hivtest
sledhs2019f <- sledhs2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))

#-----Male------

sledhs_male_ind <- read_dta("Sierra Leone_2019_DHS\\SLMR7ADT\\SLMR7AFL.DTA")
sledhs_male_bio <- read_dta("Sierra Leone_2019_DHS\\SLAR7ADT\\SLAR7AFL.DTA")


# merging by cluster number, household number and line number
merged_sierraleone_male <- left_join(sledhs_male_ind, sledhs_male_bio, 
                                     by = c("mv001" = "hivclust", 
                                            "mv002" = "hivnumb", 
                                            "mv003" = "hivline"))
# Selecting and renaming variables
sledhs2019m <- merged_sierraleone_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(sledhs2019m$year, na.rm = TRUE) #2019

# Adding columns for country, survey ID
sledhs2019m <- sledhs2019m %>% mutate(
  country = "Sierra Leone", survey_id = "SLE2019DHS", med_year = 2019)

# Reordering columns to make country and survey_id the leftmost columns
sledhs2019m <- sledhs2019m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
sledhs2019m <- sledhs2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
sledhs2019m <- sledhs2019m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
sledhs2019m$province <- as.character(sledhs2019m$province)


# Creating the hivst_knwldge and hivst_use variables
sledhs2019m <- sledhs2019m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
sledhs2019m <- sledhs2019m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
sledhs2019m <- sledhs2019m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
sledhs2019m <- sledhs2019m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
sledhs2019m <- sledhs2019m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
sledhs2019m <- sledhs2019m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
sledhs2019m <- sledhs2019m %>%
  mutate(hiv_status = case_when(
    hiv_status == 7 ~ 3,  # Recode 7 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
sledhs2019m <- sledhs2019m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))


# Combining male and female for Sierra Leone
combined_sledhs <- bind_rows(sledhs2019f, sledhs2019m)


#------------------------------------------------------------------------------

# South Africa 2016

#-----Female------

zafdhs_female_ind <- read_dta("South Africa_2016_DHS\\ZAIR71DT\\ZAIR71FL.DTA")
zafdhs_female_bio <- read_dta("South Africa_2016_DHS\\ZAAR71DT\\ZAAR71FL.DTA")

# merging by cluster number, household number and line number
merged_southafrica_female <- left_join(zafdhs_female_ind, zafdhs_female_bio, 
                                       by = c("v001" = "hivclust", 
                                              "v002" = "hivnumb", 
                                              "v003" = "hivline"))

# Selecting and renaming variables
zafdhs2016f <- merged_southafrica_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zafdhs2016f$year, na.rm = TRUE) #2016

# Adding columns for country, survey ID
zafdhs2016f <- zafdhs2016f %>% mutate(
  country = "South Africa",survey_id = "ZAF2016DHS", med_year = 2016)

# Reordering columns to make country and survey_id the leftmost columns
zafdhs2016f <- zafdhs2016f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
zafdhs2016f <- zafdhs2016f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
zafdhs2016f <- zafdhs2016f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zafdhs2016f$province <- as.character(zafdhs2016f$province)

# Creating the hivst_knwldge and hivst_use variables
zafdhs2016f <- zafdhs2016f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zafdhs2016f <- zafdhs2016f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zafdhs2016f <- zafdhs2016f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zafdhs2016f <- zafdhs2016f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
zafdhs2016f <- zafdhs2016f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
zafdhs2016f <- zafdhs2016f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
zafdhs2016f <- zafdhs2016f %>% mutate(hiv_status = case_when(
  hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
  TRUE ~ hiv_status     # Keep other values the same
))

# Recoding the last_hivtest
zafdhs2016f <- zafdhs2016f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

zafdhs_male_ind <- read_dta("South Africa_2016_DHS\\ZAMR71DT\\ZAMR71FL.DTA")
zafdhs_male_bio <- read_dta("South Africa_2016_DHS\\ZAAR71DT\\ZAAR71FL.DTA")

# merging by cluster number, household number and line number
merged_southafrica_male <- left_join(zafdhs_male_ind, zafdhs_male_bio, 
                                     by = c("mv001" = "hivclust", 
                                            "mv002" = "hivnumb", 
                                            "mv003" = "hivline"))
# Selecting and renaming variables
zafdhs2016m <- merged_southafrica_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zafdhs2016m$year, na.rm = TRUE) #2016

# Adding columns for country, survey ID
zafdhs2016m <- zafdhs2016m %>% mutate(
  country = "South Africa", survey_id = "ZAF2016DHS", med_year = 2016)

# Reordering columns to make country and survey_id the leftmost columns
zafdhs2016m <- zafdhs2016m %>% select(country, survey_id, everything())

# Adding the 'sex' column with value 1 (men)
zafdhs2016m <- zafdhs2016m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
zafdhs2016m <- zafdhs2016m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zafdhs2016m$province <- as.character(zafdhs2016m$province)


# Creating the hivst_knwldge and hivst_use variables
zafdhs2016m <- zafdhs2016m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zafdhs2016m <- zafdhs2016m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zafdhs2016m <- zafdhs2016m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zafdhs2016m <- zafdhs2016m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
zafdhs2016m <- zafdhs2016m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
zafdhs2016m <- zafdhs2016m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
zafdhs2016m <- zafdhs2016m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
zafdhs2016m <- zafdhs2016m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))


# Combining male and female for South Africa
combined_zafdhs <- bind_rows(zafdhs2016f, zafdhs2016m)


#------------------------------------------------------------------------------

# Zambia 2018

#-----Female------

zmbdhs_female_ind <- read_dta("Zambia_2018_DHS\\ZMIR71DT\\ZMIR71FL.DTA")
zmbdhs_female_bio <- read_dta("Zambia_2018_DHS\\ZMAR71DT\\ZMAR71FL.DTA")

# merging by cluster number, household number and line number
merged_zambia_female <- left_join(zmbdhs_female_ind, zmbdhs_female_bio, 
                                  by = c("v001" = "hivclust", 
                                         "v002" = "hivnumb", 
                                         "v003" = "hivline"))

# Selecting and renaming variables
zmbdhs2018f <- merged_zambia_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zmbdhs2018f$year, na.rm = TRUE) #2018

# Adding columns for country, survey ID
zmbdhs2018f <- zmbdhs2018f %>% mutate(
  country = "Zambia",survey_id = "ZMB2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
zmbdhs2018f <- zmbdhs2018f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
zmbdhs2018f <- zmbdhs2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
zmbdhs2018f <- zmbdhs2018f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zmbdhs2018f$province <- as.character(zmbdhs2018f$province)

# Creating the hivst_knwldge and hivst_use variables
zmbdhs2018f <- zmbdhs2018f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zmbdhs2018f <- zmbdhs2018f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zmbdhs2018f <- zmbdhs2018f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zmbdhs2018f <- zmbdhs2018f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
zmbdhs2018f <- zmbdhs2018f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
zmbdhs2018f <- zmbdhs2018f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
zmbdhs2018f <- zmbdhs2018f %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    hiv_status == 2 ~ 1,  # Recode 2 as 1 (HIV positive)
    TRUE ~ hiv_status     # Keep other values the same
  ))


# Recoding the last_hivtest
zmbdhs2018f <- zmbdhs2018f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

zmbdhs_male_ind <- read_dta("Zambia_2018_DHS\\ZMMR71DT\\ZMMR71FL.DTA")
zmbdhs_male_bio <- read_dta("Zambia_2018_DHS\\ZMAR71DT\\ZMAR71FL.DTA")


# merging by cluster number, household number and line number
merged_zambia_male <- left_join(zmbdhs_male_ind, zmbdhs_male_bio, 
                                by = c("mv001" = "hivclust", 
                                       "mv002" = "hivnumb", 
                                       "mv003" = "hivline"))

# Selecting and renaming variables
zmbdhs2018m <- merged_zambia_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zmbdhs2018m$year, na.rm = TRUE) #2018

# Adding columns for country, survey ID
zmbdhs2018m <- zmbdhs2018m %>% mutate(
  country = "Zambia", survey_id = "ZMB2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
zmbdhs2018m <- zmbdhs2018m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
zmbdhs2018m <- zmbdhs2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
zmbdhs2018m <- zmbdhs2018m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zmbdhs2018m$province <- as.character(zmbdhs2018m$province)


# Creating the hivst_knwldge and hivst_use variables
zmbdhs2018m <- zmbdhs2018m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zmbdhs2018m <- zmbdhs2018m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zmbdhs2018m <- zmbdhs2018m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zmbdhs2018m <- zmbdhs2018m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
zmbdhs2018m <- zmbdhs2018m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
zmbdhs2018m <- zmbdhs2018m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners %in% c(95, 82) ~ NA_real_,  # Recode 95 and 82 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))


# Recoding the hiv_status variable
zmbdhs2018m <- zmbdhs2018m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
zmbdhs2018m <- zmbdhs2018m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))


# Combining male and female for Zambia 2018
combined_zmbdhs <- bind_rows(zmbdhs2018f, zmbdhs2018m)


#----------------------------------------------------------------------------

# Zimbabwe 2015

#-----Female------

zwedhs_female_ind <- read_dta("Zimbabwe_2015_DHS\\ZWIR72DT\\ZWIR72FL.DTA")
zwedhs_female_bio <- read_dta("Zimbabwe_2015_DHS\\ZWAR71DT\\ZWAR71FL.DTA")

# merging by cluster number, household number and line number
merged_zimbabwe_female <- left_join(zwedhs_female_ind, zwedhs_female_bio, 
                                    by = c("v001" = "hivclust", 
                                           "v002" = "hivnumb", 
                                           "v003" = "hivline"))

# Selecting and renaming variables
zwedhs2015f <- merged_zimbabwe_female %>%
  select(
    psu = v001,                     # PSU/Cluster
    strata = v022,                  # Strata
    province = v101,                # Area/Province
    region = v102,                  # Urban/ Rural Area
    ind_wt = v005,                  # Individual weight
    year = v007,                    # Survey year
    hhid = v002,                    # Household ID
    age = v012,                     # Age of the respondent
    agegrp = v013,                  # Age in 5y age groups
    curr_marital = v501,            # Current marital status
    schl_years = v106,              # Highest level of schooling
    wealth_index = v190,            # Wealth quintile
    exchange_sex = v791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    hiv_wt = hiv05,                 # HIV weight
    hiv_status = hiv03,             # Final HIV status
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zwedhs2015f$year, na.rm = TRUE) #2015

# Adding columns for country, survey ID
zwedhs2015f <- zwedhs2015f %>% mutate(
  country = "Zimbabwe",survey_id = "ZWE2015DHS", med_year = 2015)

# Reordering columns to make country and survey_id the leftmost columns
zwedhs2015f <- zwedhs2015f %>%
  select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
zwedhs2015f <- zwedhs2015f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
zwedhs2015f <- zwedhs2015f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zwedhs2015f$province <- as.character(zwedhs2015f$province)


# Creating the hivst_knwldge and hivst_use variables
zwedhs2015f <- zwedhs2015f %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zwedhs2015f <- zwedhs2015f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zwedhs2015f <- zwedhs2015f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zwedhs2015f <- zwedhs2015f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
zwedhs2015f <- zwedhs2015f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
zwedhs2015f <- zwedhs2015f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ NA_real_,  # Recode 95 as NA
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the hiv_status variable
zwedhs2015f <- zwedhs2015f %>%
  mutate(hiv_status = case_when(
    hiv_status == 7 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))


# Recoding the last_hivtest
zwedhs2015f <- zwedhs2015f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

zwedhs_male_ind <- read_dta("Zimbabwe_2015_DHS\\ZWMR72DT\\ZWMR72FL.DTA")
zwedhs_male_bio <- read_dta("Zimbabwe_2015_DHS\\ZWAR71DT\\ZWAR71FL.DTA")

# merging by cluster number, household number and line number
merged_zimbabwe_male <- left_join(zwedhs_male_ind, zwedhs_male_bio, 
                                  by = c("mv001" = "hivclust", 
                                         "mv002" = "hivnumb", 
                                         "mv003" = "hivline"))

# Selecting and renaming variables
zwedhs2015m <- merged_zimbabwe_male %>%
  select(
    psu = mv001,                     # PSU/Cluster
    strata = mv022,                  # Strata
    province = mv101,                # Area/Province
    region = mv102,                  # Urban/ Rural Area
    ind_wt = mv005,                  # Individual weight
    year = mv007,                    # Survey year
    hhid = mv002,                    # Household ID
    age = mv012,                     # Age of the respondent
    agegrp = mv013,                  # Age in 5y age groups
    curr_marital = mv501,            # Current marital status
    schl_years = mv106,              # Highest level of schooling
    wealth_index = mv190,            # Wealth quintile
    exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    hiv_wt = hiv05,                  # HIV weight
    hiv_status = hiv03,              # Final HIV status
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(zwedhs2015m$year, na.rm = TRUE) #2015

# Adding columns for country, survey ID
zwedhs2015m <- zwedhs2015m %>% mutate(
  country = "Zimbabwe", survey_id = "ZWE2015DHS", med_year = 2015)

# Reordering columns to make country and survey_id the leftmost columns
zwedhs2015m <- zwedhs2015m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
zwedhs2015m <- zwedhs2015m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
zwedhs2015m <- zwedhs2015m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
zwedhs2015m$province <- as.character(zwedhs2015m$province)

# Creating the hivst_knwldge and hivst_use variables
zwedhs2015m <- zwedhs2015m %>%
  mutate(
    hivst_knwldge = case_when(
      selftest_dhs == 0 ~ 0,
      selftest_dhs %in% c(1, 2) ~ 1,
      TRUE ~ 0
    ),
    hivst_use = case_when(
      selftest_dhs == 1 ~ 1,
      TRUE ~ 0
    )
  )

# moving to the right of the selftest_dhs column
zwedhs2015m <- zwedhs2015m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
zwedhs2015m <- zwedhs2015m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
zwedhs2015m <- zwedhs2015m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
zwedhs2015m <- zwedhs2015m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
zwedhs2015m <- zwedhs2015m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners %in% c(95, 82) ~ NA_real_,  # Recode 95 and 82 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))


# Recoding the hiv_status variable
zwedhs2015m <- zwedhs2015m %>%
  mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status             # Keep other values the same
  ))

# Recoding the last_hivtest
zwedhs2015m <- zwedhs2015m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  TRUE ~ NA_real_              # Missing or other values
))

# Combining male and female for Zimbabwe 2015
combined_zwedhs <- bind_rows(zwedhs2015f, zwedhs2015m)



# Combining and saving all the DHS with biomarker dta
bio_list_dhs <- list(
  combined_bdidhs,
  combined_cmrdhs,
  combined_gindhs,
  combined_mwidhs,
  combined_sendhs,
  combined_sledhs,
  combined_zafdhs,
  combined_zmbdhs,
  combined_zwedhs
)

saveRDS(bio_list_dhs, file = "cleaned biomarker surveys/bio_list_dhs.rds")






