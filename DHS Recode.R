# Gambia (only ever heard of hivst) code
# NA real gulake 0


# Clearing all objects
rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(labelled)
library(survey)

setwd("D:/Downloads/MSc Thesis/hiv-selftesting/1. thesis rawdata/DHS raw data")


# Benin 2017-18

#-----Female------
bendhs_female_ind <- read_dta("Benin 2017-18_DHS\\BJIR71DT\\BJIR71FL.DTA")

# Selecting and renaming variables
bendhs2017f <- bendhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a            # How many months ago last tested for HIV 
  ) 

# Calculating the median year of interview
median(bendhs2017f$year, na.rm = TRUE) #median year 2018

# Adding columns for country, survey ID
bendhs2017f <- bendhs2017f %>%
  mutate(
    country = "Benin",
    survey_id = "BEN2017DHS",
    med_year = 2018 
    )

# Reordering columns to make country and survey_id the leftmost columns
bendhs2017f <- bendhs2017f %>% select(country, survey_id, med_year, everything())

# Recoding province
bendhs2017f$province <- as.character(bendhs2017f$province)

# Adding the 'sex' column with value 0 (women)
bendhs2017f <- bendhs2017f %>%
  mutate(sex = 0) %>%             # Assign value 0 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Correcting individual weights 
bendhs2017f <- bendhs2017f %>%
  mutate(ind_wt = ind_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
bendhs2017f <- bendhs2017f %>%
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
bendhs2017f <- bendhs2017f %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bendhs2017f <- bendhs2017f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bendhs2017f <- bendhs2017f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
bendhs2017f <- bendhs2017f %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the last_hivtest
bendhs2017f <- bendhs2017f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


#-----Male------

bendhs_male_ind <- read_dta("Benin 2017-18_DHS\\BJMR71DT\\BJMR71FL.DTA")

# Selecting and renaming variables
bendhs2017m <- bendhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a            # How many months ago last tested for HIV 
  ) 


# Calculating the median year of interview
median(bendhs2017m$year, na.rm = TRUE) #median year 2018


# Adding columns for country, survey ID
bendhs2017m <- bendhs2017m %>%
  mutate(
    country = "Benin",
    survey_id = "BEN2017DHS",
    med_year = 2018
  )

# Reordering columns to make country and survey_id the leftmost columns
bendhs2017m <- bendhs2017m %>%
  select(country, survey_id, med_year, everything())

# Recoding province 
bendhs2017m$province <- as.character(bendhs2017m$province)


# Adding the 'sex' column with value 1 (men)
bendhs2017m <- bendhs2017m %>%
  mutate(sex = 1) %>%             # Assign value 1 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Correcting individual weights 
bendhs2017m <- bendhs2017m %>%
  mutate(ind_wt = ind_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
bendhs2017m <- bendhs2017m %>%
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
bendhs2017m <- bendhs2017m %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bendhs2017m <- bendhs2017m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bendhs2017m <- bendhs2017m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
bendhs2017m <- bendhs2017m %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the last_hivtest
bendhs2017m <- bendhs2017m %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))


# Combining male and female for Benin
combined_bendhs <- bind_rows(bendhs2017f, bendhs2017m)


# checking weighted proportion of hivst_use (unweighted = 0.006)
# bendhs_design <- svydesign(
# ids = ~psu, strata = ~strata, weights = ~ind_wt, data = combined_bendhs, nest = TRUE)

# Calculate the proportion of people who used HIV self-testing
# prop_hivst_k <- svyciprop(~I(hivst_knwldge == 1), design = bendhs_design, method = "logit", level = 0.95)
# prop_hivst_k #0.005 (0.004, 0.007) 


#---------------------------------------------------------------------------------------

# Burkina Faso 2021

#-----Female------
bfadhs_female_ind <- read_dta("Burkina Faso 2021_DHS\\BFIR81DT\\BFIR81FL.DTA")

# Selecting and renaming variables (no observation for exchange_sex, ever_heard)
bfadhs2021f <- bfadhs_female_ind %>%
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
    total_partners =  v766b,        # Number of sex partners in the last 12 months(some don't know)
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    last_hivresult = v861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(bfadhs2021f$year, na.rm = TRUE) #median year 2021

# Adding columns for country, survey ID
bfadhs2021f <- bfadhs2021f %>%
  mutate(
    country = "Burkina Faso",
    survey_id = "BFA2021DHS",
    med_year = 2021
  )

# Reordering columns to make country and survey_id the leftmost columns
bfadhs2021f <- bfadhs2021f %>% select(country, survey_id, med_year, everything())

# Recoding province
bfadhs2021f$province <- as.character(bfadhs2021f$province)

# Adding the 'sex' column with value 0 (women)
bfadhs2021f <- bfadhs2021f %>%
  mutate(sex = 0) %>% relocate(sex, .after = agegrp)  

# Correcting individual weights 
bfadhs2021f <- bfadhs2021f %>% mutate(ind_wt = ind_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
bfadhs2021f <- bfadhs2021f %>%
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
bfadhs2021f <- bfadhs2021f %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bfadhs2021f <- bfadhs2021f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bfadhs2021f <- bfadhs2021f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile the specified labels
bfadhs2021f <- bfadhs2021f %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
bfadhs2021f <- bfadhs2021f %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
bfadhs2021f <- bfadhs2021f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))

# Recoding the last hiv result
bfadhs2021f <- bfadhs2021f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

#-----Male------

bfadhs_male_ind <- read_dta("Burkina Faso 2021_DHS\\BFMR81DT\\BFMR81FL.DTA")

# Selecting and renaming variables(no observation for exchange_sex,ever_heard)
bfadhs2021m <- bfadhs_male_ind %>%
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
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    last_hivresult = mv861           # result of last hiv test
    ) 

# Calculating the median year of interview
median(bfadhs2021m$year, na.rm = TRUE) #median year 2021

# Adding columns for country, survey ID
bfadhs2021m <- bfadhs2021m %>%
  mutate(country = "Burkina Faso",
    survey_id = "BFA2021DHS",
    med_year = 2021)

# Reordering columns to make country and survey_id the leftmost columns
bfadhs2021m <- bfadhs2021m %>%
  select(country, survey_id, med_year, everything())

# Recoding province
bfadhs2021m$province <- as.character(bfadhs2021m$province)


# Adding the 'sex' column with value 1 (men)
bfadhs2021m <- bfadhs2021m %>%
  mutate(sex = 1) %>%             # Assign value 1 to the 'sex' column for all rows
  relocate(sex, .after = agegrp)  # Move the 'sex' column after the 'agegrp' column

# Correcting individual weights 
bfadhs2021m <- bfadhs2021m %>%
  mutate(ind_wt = ind_wt / 1e6)

# Creating the hivst_knwldge and hivst_use variables
bfadhs2021m <- bfadhs2021m %>%
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
bfadhs2021m <- bfadhs2021m %>%
  relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
bfadhs2021m <- bfadhs2021m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
bfadhs2021m <- bfadhs2021m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile for specified labels
bfadhs2021m <- bfadhs2021m %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the total_partners variable
bfadhs2021m <- bfadhs2021m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
bfadhs2021m <- bfadhs2021m %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
    last_hivtest %in% 12:24 ~ 2,     # Tested 1-2 years ago
    last_hivtest %in% 25:96 ~ 3,     # Tested more than 2 years ago
    last_hivtest == 98 ~ 88,         # Don't know
    TRUE ~ NA_real_                  # Missing or other values
  ))

# Recoding the last hiv result
bfadhs2021m <- bfadhs2021m %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

# Combining male and female for Burkina Faso
combined_bfadhs <- bind_rows(bfadhs2021f, bfadhs2021m)

# checking weighted proportion of hivst (manual check = 0.003)
#bfadhs_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = combined_bfadhs, nest = TRUE)

# Calculate the proportion of people who used HIV self-testing
#prop_hivst_use_2 <- svyciprop(~I(hivst_use == 1), design = bfadhs_design, method = "logit", level = 0.95)
#prop_hivst_use_2 #0.003 (0.002, 0.004)



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


# Combining male and female for Burundi
combined_bdidhs <- bind_rows(bdidhs2016f, bdidhs2016m)

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

#-------------------------------------------------------------------------------

# Cote d'Ivoire 2021

#-----Female------
civdhs_female_ind <- read_dta("Cote d'Ivoire_2021_DHS\\CIIR81DT\\CIIR81FL.DTA")

# Selecting and renaming variables (no obs for exchange_sex)
civdhs2021f <- civdhs_female_ind %>%
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
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    last_hivresult = v861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(civdhs2021f$year, na.rm = TRUE) # med interview year 2021

# Adding columns for country, survey ID
civdhs2021f <- civdhs2021f %>%
  mutate(country = "Cote d'Ivoire", survey_id = "CIV2021DHS", med_year = 2021)

# Reordering columns to make country and survey_id the leftmost columns
civdhs2021f <- civdhs2021f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
civdhs2021f <- civdhs2021f %>%
  mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
civdhs2021f <- civdhs2021f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
civdhs2021f$province <- as.character(civdhs2021f$province)

# Creating the hivst_knwldge and hivst_use variables
civdhs2021f <- civdhs2021f %>%
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
civdhs2021f <- civdhs2021f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
civdhs2021f <- civdhs2021f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
civdhs2021f <- civdhs2021f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile the specified labels
civdhs2021f <- civdhs2021f %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
civdhs2021f <- civdhs2021f %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
civdhs2021f <- civdhs2021f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:24) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    last_hivtest == 98 ~ 88,     # Don't know
    TRUE ~ NA_real_              # Missing or other values
  ))

# Recoding the last hiv result
civdhs2021f <- civdhs2021f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

#-----Male------

civdhs_male_ind <- read_dta("Cote d'Ivoire_2021_DHS\\CIMR81DT\\CIMR81FL.DTA")

# Selecting and renaming variables(no observation for exchange_sex)
civdhs2021m <- civdhs_male_ind %>%
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
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    last_hivresult = mv861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(civdhs2021m$year, na.rm = TRUE) 

# Adding columns for country, survey ID
civdhs2021m <- civdhs2021m %>% mutate(
  country = "Cote d'Ivoire", survey_id = "CIV2021DHS", med_year = 2021)

# Reordering columns to make country and survey_id the leftmost columns
civdhs2021m <- civdhs2021m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
civdhs2021m <- civdhs2021m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
civdhs2021m <- civdhs2021m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
civdhs2021m$province <- as.character(civdhs2021m$province)

# Creating the hivst_knwldge and hivst_use variables
civdhs2021m <- civdhs2021m %>%
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
civdhs2021m <- civdhs2021m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
civdhs2021m <- civdhs2021m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
civdhs2021m <- civdhs2021m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile for specified labels
civdhs2021m <- civdhs2021m %>%
  mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the total_partners variable
civdhs2021m <- civdhs2021m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
civdhs2021m <- civdhs2021m %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
    last_hivtest %in% 12:24 ~ 2,     # Tested 1-2 years ago
    last_hivtest %in% 25:96 ~ 3,     # Tested more than 2 years ago
    last_hivtest == 98 ~ 88,         # Don't know
    TRUE ~ NA_real_                  # Missing or other values
  ))

# Recoding the last hiv result
civdhs2021m <- civdhs2021m %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

# Combining male and female for Cote d'Ivoire
combined_civdhs <- bind_rows(civdhs2021f, civdhs2021m)


#------------------------------------------------------------------------------


# Gabon 2019-21 (has biomarker dataset)

#-----Female------

gabdhs_female_ind <- read_dta("Gabon_2019-21_DHS\\GAIR71DT\\GAIR71FL.DTA")
gabdhs_female_bio <- read_dta("Gabon_2019-21_DHS\\GAAR71DT\\GAAR71FL.DTA")

# merging by cluster number, household number and line number
merged_gabon_female <- left_join(gabdhs_female_ind, gabdhs_female_bio, 
                                    by = c("v001" = "hivclust", 
                                           "v002" = "hivnumb", 
                                           "v003" = "hivline"))

# Selecting and renaming variables
gabdhs2019f <- merged_gabon_female %>%
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
median(gabdhs2019f$year, na.rm = TRUE) #median year 2020

# Adding columns for country, survey ID
gabdhs2019f <- gabdhs2019f %>% mutate(country = "Gabon",
                                      survey_id = "GAB2019DHS", 
                                      med_year = 2020)

# Reordering columns to make country and survey_id the leftmost columns
gabdhs2019f <- gabdhs2019f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
gabdhs2019f <- gabdhs2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual and HIV weights 
gabdhs2019f <- gabdhs2019f %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
gabdhs2019f$province <- as.character(gabdhs2019f$province)

# Creating the hivst_knwldge and hivst_use variables
gabdhs2019f <- gabdhs2019f %>%
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
gabdhs2019f <- gabdhs2019f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
gabdhs2019f <- gabdhs2019f %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
gabdhs2019f <- gabdhs2019f %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile the specified labels
gabdhs2019f <- gabdhs2019f %>% mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
gabdhs2019f <- gabdhs2019f %>% mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))


# Recoding the hiv_status variable
gabdhs2019f <- gabdhs2019f %>% mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))


# Recoding the last_hivtest
gabdhs2019f <- gabdhs2019f %>%
  mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))



#-----Male------

gabdhs_male_ind <- read_dta("Gabon_2019-21_DHS\\GAMR71DT\\GAMR71FL.DTA")
gabdhs_male_bio <- read_dta("Gabon_2019-21_DHS\\GAAR71DT\\GAAR71FL.DTA")

# merging by cluster number, household number and line number
merged_gabon_male <- left_join(gabdhs_male_ind, gabdhs_male_bio, 
                                  by = c("mv001" = "hivclust", 
                                         "mv002" = "hivnumb", 
                                         "mv003" = "hivline"))
# Selecting and renaming variables
gabdhs2019m <- merged_gabon_male %>%
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
median(gabdhs2019m$year, na.rm = TRUE) #median year 2020

# Adding columns for country, survey ID
gabdhs2019m <- gabdhs2019m %>% mutate(country = "Gabon", 
                                      survey_id = "GAB2019DHS",
                                      med_year = 2020)

# Reordering columns to make country and survey_id the leftmost columns
gabdhs2019m <- gabdhs2019m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
gabdhs2019m <- gabdhs2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)  

# Correcting individual and HIV weights 
gabdhs2019m <- gabdhs2019m %>% mutate(ind_wt = ind_wt / 1e6, hiv_wt = hiv_wt / 1e6)

# Recoding province
gabdhs2019m$province <- as.character(gabdhs2019m$province)

# Creating the hivst_knwldge and hivst_use variables
gabdhs2019m <- gabdhs2019m %>%
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
gabdhs2019m <- gabdhs2019m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
gabdhs2019m <- gabdhs2019m %>%
  mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
gabdhs2019m <- gabdhs2019m %>%
  mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile for specified labels
gabdhs2019m <- gabdhs2019m %>% mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners
gabdhs2019m <- gabdhs2019m %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ NA_real_,  # Recode 95 as NA
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the hiv_status variable
gabdhs2019m <- gabdhs2019m %>% mutate(hiv_status = case_when(
    hiv_status == 9 ~ 3,  # Recode 9 as 3 (inconclusive)
    TRUE ~ hiv_status     # Keep other values the same
  ))

# Recoding the last_hivtest
gabdhs2019m <- gabdhs2019m %>% mutate(last_hivtest = case_when(
    last_hivtest %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23) ~ 2, # Tested 1-2 years ago
    last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
    TRUE ~ NA_real_              # Missing or other values
  ))

# Combining male and female for Gabon
combined_gabdhs <- bind_rows(gabdhs2019f, gabdhs2019m)

#------------------------------------------------------------------------------

# Ghana 2022

#-----Female------
ghadhs_female_ind <- read_dta("Ghana_2022_DHS\\GHIR8ADT\\GHIR8AFL.DTA")

# Selecting and renaming variables (no obs for exchange_sex)
ghadhs2022f <- ghadhs_female_ind %>%
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
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,            # How many months ago last tested for HIV
    last_hivresult = v861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(ghadhs2022f$year, na.rm = TRUE) 

# Adding columns for country, survey ID
ghadhs2022f <- ghadhs2022f %>% mutate(
  country = "Ghana", survey_id = "GHA2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
ghadhs2022f <- ghadhs2022f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
ghadhs2022f <- ghadhs2022f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
ghadhs2022f <- ghadhs2022f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
ghadhs2022f$province <- as.character(ghadhs2022f$province)

# Creating the hivst_knwldge and hivst_use variables
ghadhs2022f <- ghadhs2022f %>%
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
ghadhs2022f <- ghadhs2022f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
ghadhs2022f <- ghadhs2022f %>% mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
ghadhs2022f <- ghadhs2022f %>% mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))


# Recoding the wealth quintile the specified labels
ghadhs2022f <- ghadhs2022f %>% mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
ghadhs2022f <- ghadhs2022f %>%
  mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
ghadhs2022f <- ghadhs2022f %>% mutate(last_hivtest = case_when(
    last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
    last_hivtest %in% c(12:24) ~ 2, # Tested 1-2 years ago
    last_hivtest %in% c(25:96) ~ 3,      # Tested more than 2 years ago
    last_hivtest == 98 ~ 88,     # Don't know
    TRUE ~ NA_real_              # Missing or other values
  ))

# Recoding the last hiv result
ghadhs2022f <- ghadhs2022f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

#ghana female dhs 2022 deno and num (6250.364, 150.6338)

ghadhs_design_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = ghadhs2022f, nest = TRUE)
gha_prop_f <- svyciprop(~I(hivst_use == 1), design = ghadhs_design_f, method = "logit", level = 0.95)
se_hivst_use_f <- SE(gha_prop_f)

ghaprop_hivst_use_f <-  0.0241 
gha_se_hivst_use_f <- 0.001939807 

eff_ss_f <- (ghaprop_hivst_use_f * (1 - ghaprop_hivst_use_f)) / (gha_se_hivst_use_f^2)
ghamicsf_num <- ghaprop_hivst_use_f * eff_ss_f



#-----Male------

ghadhs_male_ind <- read_dta("Ghana_2022_DHS\\GHMR8ADT\\GHMR8AFL.DTA")

# Selecting and renaming variables(no observation for exchange_sex)
ghadhs2022m <- ghadhs_male_ind %>%
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
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    last_hivresult = mv861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(ghadhs2022m$year, na.rm = TRUE) 

# Adding columns for country, survey ID
ghadhs2022m <- ghadhs2022m %>% mutate(
  country = "Ghana", survey_id = "GHA2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
ghadhs2022m <- ghadhs2022m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
ghadhs2022m <- ghadhs2022m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
ghadhs2022m <- ghadhs2022m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
ghadhs2022m$province <- as.character(ghadhs2022m$province)


# Creating the hivst_knwldge and hivst_use variables
ghadhs2022m <- ghadhs2022m %>%
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
ghadhs2022m <- ghadhs2022m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
ghadhs2022m <- ghadhs2022m %>% mutate(region = case_when(
    region == 1 ~ 1,   # Urban
    region == 2 ~ 0,   # Rural
    TRUE ~ NA_real_    # Missing
  ))

# Recoding the curr_marital 
ghadhs2022m <- ghadhs2022m %>% mutate(curr_marital = case_when(
    curr_marital == 0 ~ 1,   # Single
    curr_marital == 3 ~ 2,   # Widowed
    curr_marital == 4 ~ 3,   # Divorced
    curr_marital == 5 ~ 4,   # Separated
    curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
    TRUE ~ NA_real_          # Missing or any other values
  ))

# Recoding the wealth quintile for specified labels
ghadhs2022m <- ghadhs2022m %>% mutate(wealth_index = factor(wealth_index, 
                               levels = c(1, 2, 3, 4, 5), 
                               labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the total_partners variable
ghadhs2022m <- ghadhs2022m %>% mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
ghadhs2022m <- ghadhs2022m %>% mutate(last_hivtest = case_when(
    last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
    last_hivtest %in% 12:24 ~ 2,     # Tested 1-2 years ago
    last_hivtest %in% 25:96 ~ 3,     # Tested more than 2 years ago
    last_hivtest == 98 ~ 88,         # Don't know
    TRUE ~ NA_real_                  # Missing or other values
  ))

# Recoding the last hiv result
ghadhs2022m <- ghadhs2022m %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  TRUE ~ NA_real_            # Missing or other values
))


#2022 ghana dhs male deno and num (4558.453, 82.508)
#ghadhs_design_m <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = ghadhs2022m, nest = TRUE)
#gha_prop_m <- svyciprop(~I(hivst_use == 1), design = ghadhs_design_m, method = "logit", level = 0.95)
#se_hivst_use_m <- SE(gha_prop_m)

#ghaprop_hivst_use_m <-  0.0181 
#gha_se_hivst_use_m <- 0.001974532 

#eff_ss_m <- (ghaprop_hivst_use_m * (1 - ghaprop_hivst_use_m)) / (gha_se_hivst_use_m^2)
#ghamicsm_num <- ghaprop_hivst_use_m * eff_ss_m



# Combining male and female for Ghana
combined_ghadhs <- bind_rows(ghadhs2022f, ghadhs2022m)

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

# Kenya 2022

#-----Female------
kendhs_female_ind <- read_dta("Kenya_2022_DHS\\KEIR8CDT\\KEIR8CFL.DTA")

# Selecting and renaming variables (no obs for exchange_sex)
kendhs2022f <- kendhs_female_ind %>%
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
    total_partners =  v766b,        # Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    last_hivresult = v861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(kendhs2022f$year, na.rm = TRUE) #2022

# Adding columns for country, survey ID
kendhs2022f <- kendhs2022f %>% mutate(
  country = "Kenya", survey_id = "KEN2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
kendhs2022f <- kendhs2022f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
kendhs2022f <- kendhs2022f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
kendhs2022f <- kendhs2022f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
kendhs2022f$province <- as.character(kendhs2022f$province)

# Creating the hivst_knwldge and hivst_use variables
kendhs2022f <- kendhs2022f %>%
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
kendhs2022f <- kendhs2022f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
kendhs2022f <- kendhs2022f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
kendhs2022f <- kendhs2022f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile the specified labels
kendhs2022f <- kendhs2022f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
kendhs2022f <- kendhs2022f %>% mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    TRUE ~ total_partners        # Keep other values the same
  ))

# Recoding the last_hivtest
kendhs2022f <- kendhs2022f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:24) ~ 2, # Tested 1-2 years ago
  last_hivtest %in% c(25:96) ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing or other values
))


# Recoding the last hiv result
kendhs2022f <- kendhs2022f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))


#-----Male------

kendhs_male_ind <- read_dta("Kenya_2022_DHS\\KEMR8CDT\\KEMR8CFL.DTA")

# Selecting and renaming variables(no observation for exchange_sex)
kendhs2022m <- kendhs_male_ind %>%
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
    total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    last_hivresult = mv861           # result of last hiv test
  ) 

# Calculating the median year of interview
median(kendhs2022m$year, na.rm = TRUE) #2022

# Adding columns for country, survey ID
kendhs2022m <- kendhs2022m %>% mutate(
  country = "Kenya", survey_id = "KEN2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
kendhs2022m <- kendhs2022m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
kendhs2022m <- kendhs2022m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
kendhs2022m <- kendhs2022m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
kendhs2022m$province <- as.character(kendhs2022m$province)

# Creating the hivst_knwldge and hivst_use variables
kendhs2022m <- kendhs2022m %>% mutate(
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
kendhs2022m <- kendhs2022m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
kendhs2022m <- kendhs2022m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
kendhs2022m <- kendhs2022m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile for specified labels
kendhs2022m <- kendhs2022m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the total_partners variable
kendhs2022m <- kendhs2022m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
kendhs2022m <- kendhs2022m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:24 ~ 2,     # Tested 1-2 years ago
  last_hivtest %in% 25:96 ~ 3,     # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))

# Recoding the last hiv result
kendhs2022m <- kendhs2022m %>% mutate(last_hivresult = case_when(
    last_hivresult == 1 ~ 1,   # Positive
    last_hivresult == 2 ~ 0,   # Negative
    last_hivresult == 3 ~ 3,   # Indeterminate
    last_hivresult == 4 ~ 98,  # Refused
    TRUE ~ NA_real_            # Missing or other values
  ))

# Combining male and female for Kenya
combined_kendhs <- bind_rows(kendhs2022f, kendhs2022m)



#-------------------------------------------------------------------------

# Liberia 2019-20

#-----Female------
lbrdhs_female_ind <- read_dta("Liberia_2019-20_DHS\\LBIR7ADT\\LBIR7AFL.DTA")

# Selecting and renaming variables (no obs for exchange_sex)
lbrdhs2019f <- lbrdhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v861           # result of last hiv test (not available)
  ) 


# Calculating the median year of interview
median(lbrdhs2019f$year, na.rm = TRUE) #median year 2019


# Adding columns for country, survey ID
lbrdhs2019f <- lbrdhs2019f %>% mutate(country = "Liberia", 
                                      survey_id = "LBR2019DHS",
                                      med_year = 2019)

# Reordering columns to make country and survey_id the leftmost columns
lbrdhs2019f <- lbrdhs2019f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
lbrdhs2019f <- lbrdhs2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
lbrdhs2019f <- lbrdhs2019f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
lbrdhs2019f$province <- as.character(lbrdhs2019f$province)

# Creating the hivst_knwldge and hivst_use variables
lbrdhs2019f <- lbrdhs2019f %>% mutate(
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
lbrdhs2019f <- lbrdhs2019f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
lbrdhs2019f <- lbrdhs2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
lbrdhs2019f <- lbrdhs2019f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile the specified labels
lbrdhs2019f <- lbrdhs2019f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
lbrdhs2019f <- lbrdhs2019f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
lbrdhs2019f <- lbrdhs2019f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:24) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing or other values
))


#-----Male------

lbrdhs_male_ind <- read_dta("Liberia_2019-20_DHS\\LBMR7ADT\\LBMR7AFL.DTA")

# Selecting and renaming variables
lbrdhs2019m <- lbrdhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    #last_hivresult = mv861           # result of last hiv test (NA)
  ) 

# Calculating the median year of interview
median(lbrdhs2019m$year, na.rm = TRUE) #median year 2019

# Adding columns for country, survey ID
lbrdhs2019m <- lbrdhs2019m %>% mutate(country = "Liberia", 
                                      survey_id = "LBR2019DHS",
                                      med_year = 2019)

# Reordering columns to make country and survey_id the leftmost columns
lbrdhs2019m <- lbrdhs2019m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
lbrdhs2019m <- lbrdhs2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
lbrdhs2019m <- lbrdhs2019m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
lbrdhs2019m$province <- as.character(lbrdhs2019m$province)

# Creating the hivst_knwldge and hivst_use variables
lbrdhs2019m <- lbrdhs2019m %>% mutate(
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
lbrdhs2019m <- lbrdhs2019m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
lbrdhs2019m <- lbrdhs2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
lbrdhs2019m <- lbrdhs2019m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile for specified labels
lbrdhs2019m <- lbrdhs2019m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
lbrdhs2019m <- lbrdhs2019m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
lbrdhs2019m <- lbrdhs2019m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:23 ~ 2,     # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,          # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))


# Combining male and female for Liberia
combined_lbrdhs <- bind_rows(lbrdhs2019f, lbrdhs2019m)

#------------------------------------------------------------------------------

# Madagascar 2021

#-----Female------
mdgdhs_female_ind <- read_dta("Madagascar_2021_DHS\\MDIR81DT\\MDIR81FL.DTA")

# Selecting and renaming variables 
mdgdhs2021f <- mdgdhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v473b         # result of last hiv test (not available)
  ) 

# Calculating the median year of interview
median(mdgdhs2021f$year, na.rm = TRUE) #2021

# Adding columns for country, survey ID
mdgdhs2021f <- mdgdhs2021f %>% mutate(
  country = "Madagascar", survey_id = "MDG2021DHS", med_year = 2021)

# Reordering columns to make country and survey_id the leftmost columns
mdgdhs2021f <- mdgdhs2021f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
mdgdhs2021f <- mdgdhs2021f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mdgdhs2021f <- mdgdhs2021f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mdgdhs2021f$province <- as.character(mdgdhs2021f$province)

# Creating the hivst_knwldge and hivst_use variables
mdgdhs2021f <- mdgdhs2021f %>% mutate(
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
mdgdhs2021f <- mdgdhs2021f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mdgdhs2021f <- mdgdhs2021f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mdgdhs2021f <- mdgdhs2021f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile the specified labels
mdgdhs2021f <- mdgdhs2021f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
mdgdhs2021f <- mdgdhs2021f %>% mutate(total_partners = case_when(
    total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
    total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
    TRUE ~ total_partners        # Keep other values the same
  ))


# Recoding the last_hivtest
mdgdhs2021f <- mdgdhs2021f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))

#madagascar female Den: 6825.568 Num: 19.8624


#mdgdhs_design_f <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mdgdhs2021f, nest = TRUE)
#mdg_prop_f <- svyciprop(~I(hivst_use == 1), design = mdgdhs_design_f, method = "logit", level = 0.95)
#se_hivst_use_f <- SE(mdg_prop_f)

#mdgprop_hivst_use_f <-  0.00291 
#mdg_se_hivst_use_f <- 0.000651995 

#eff_ss_f <- (mdgprop_hivst_use_f * (1 - mdgprop_hivst_use_f)) / (mdg_se_hivst_use_f^2)
#mdgdhsf_num <- mdgprop_hivst_use_f * eff_ss_f


#-----Male------

mdgdhs_male_ind <- read_dta("Madagascar_2021_DHS\\MDMR81DT\\MDMR81FL.DTA")

# Selecting and renaming variables
mdgdhs2021m <- mdgdhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    #last_hivresult = mv861           # result of last hiv test (NA)
  ) 

# Calculating the median year of interview
median(mdgdhs2021m$year, na.rm = TRUE) #2021

# Adding columns for country, survey ID
mdgdhs2021m <- mdgdhs2021m %>% mutate(
  country = "Madagascar", survey_id = "MDG2021DHS", med_year = 2021)

# Reordering columns to make country and survey_id the leftmost columns
mdgdhs2021m <- mdgdhs2021m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
mdgdhs2021m <- mdgdhs2021m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mdgdhs2021m <- mdgdhs2021m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mdgdhs2021m$province <- as.character(mdgdhs2021m$province)


# Creating the hivst_knwldge and hivst_use variables
mdgdhs2021m <- mdgdhs2021m %>% mutate(
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
mdgdhs2021m <- mdgdhs2021m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mdgdhs2021m <- mdgdhs2021m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mdgdhs2021m <- mdgdhs2021m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile for specified labels
mdgdhs2021m <- mdgdhs2021m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
mdgdhs2021m <- mdgdhs2021m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
mdgdhs2021m <- mdgdhs2021m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:23 ~ 2,     # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,          # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))


#mdgdhs_design_m <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mdgdhs2021m, nest = TRUE)
#mdg_prop_m <- svyciprop(~I(hivst_use == 1), design = mdgdhs_design_m, method = "logit", level = 0.95)
#se_hivst_use_m <- SE(mdg_prop_m)

#mdgprop_hivst_use_m <-  0.00709 
#mdg_se_hivst_use_m <- 0.001067444 

#eff_ss_m <- (mdgprop_hivst_use_m * (1 - mdgprop_hivst_use_m)) / (mdg_se_hivst_use_m^2)
#mdgdhsm_num <- mdgprop_hivst_use_m * eff_ss_m

#Male:Den: 6178.256 Num: 43.80384

# Combining male and female for Madagascar
combined_mdgdhs <- bind_rows(mdgdhs2021f, mdgdhs2021m)


#------------------------------------------------------------------------------

# Malawi 2015-16 (has biomarket data)

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


#2015 malawi dhs female deno and num (14792.46, 135.6469)
#mwidhs_design_f <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mwidhs2015f, nest = TRUE)
#mwi_prop_f <- svyciprop(~I(hivst_use == 1), design = mwidhs_design_f, method = "logit", level = 0.95)
#se_hivst_use_f <- SE(mwi_prop_f)

##mwiprop_hivst_use_f <-  0.00917 
#mwi_se_hivst_use_f <- 0.0007837255 

#eff_ss_f <- (mwiprop_hivst_use_f * (1 - mwiprop_hivst_use_f)) / (mwi_se_hivst_use_f^2)
#mwidhsf_num <- mwiprop_hivst_use_f * eff_ss_f


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

#2015 malawi dhs male deno and num (2795.972, 30.47609)
#mwidhs_design_m <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = mwidhs2015m, nest = TRUE)
#mwi_prop_m <- svyciprop(~I(hivst_use == 1), design = mwidhs_design_m, method = "logit", level = 0.95)
#se_hivst_use_m <- SE(mwi_prop_m)

#mwiprop_hivst_use_m <-  0.01090 
#mwi_se_hivst_use_m <- 0.001963663 

#eff_ss_m <- (mwiprop_hivst_use_m * (1 - mwiprop_hivst_use_m)) / (mwi_se_hivst_use_m^2)
#mwidhsm_num <- mwiprop_hivst_use_m * eff_ss_m


# Combining male and female for Malawi
combined_mwidhs <- bind_rows(mwidhs2015f, mwidhs2015m)

#-----------------------------------------------------------------------------

# Mali 2018

#-----Female------
mlidhs_female_ind <- read_dta("Mali_2018_DHS\\MLIR7ADT\\MLIR7AFL.DTA")

# Selecting and renaming variables
mlidhs2018f <- mlidhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v473b         # result of last hiv test (not available)
  ) 

# Calculating the median year of interview
median(mlidhs2018f$year, na.rm = TRUE) #2018

# Adding columns for country, survey ID
mlidhs2018f <- mlidhs2018f %>% mutate(
  country = "Mali", survey_id = "MLI2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
mlidhs2018f <- mlidhs2018f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
mlidhs2018f <- mlidhs2018f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mlidhs2018f <- mlidhs2018f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mlidhs2018f$province <- as.character(mlidhs2018f$province)


# Creating the hivst_knwldge and hivst_use variables
mlidhs2018f <- mlidhs2018f %>% mutate(
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
mlidhs2018f <- mlidhs2018f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mlidhs2018f <- mlidhs2018f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mlidhs2018f <- mlidhs2018f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile the specified labels
mlidhs2018f <- mlidhs2018f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
mlidhs2018f <- mlidhs2018f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the last_hivtest
mlidhs2018f <- mlidhs2018f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))


#-----Male------

mlidhs_male_ind <- read_dta("Mali_2018_DHS\\MLMR7ADT\\MLMR7AFL.DTA")

# Selecting and renaming variables
mlidhs2018m <- mlidhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    #last_hivresult = mv861           # result of last hiv test (NA)
  ) 

# Calculating the median year of interview
median(mlidhs2018m$year, na.rm = TRUE) #2018


# Adding columns for country, survey ID
mlidhs2018m <- mlidhs2018m %>% mutate(
  country = "Mali", survey_id = "MLI2018DHS", med_year = 2018)

# Reordering columns to make country and survey_id the leftmost columns
mlidhs2018m <- mlidhs2018m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
mlidhs2018m <- mlidhs2018m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mlidhs2018m <- mlidhs2018m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mlidhs2018m$province <- as.character(mlidhs2018m$province)

# Creating the hivst_knwldge and hivst_use variables
mlidhs2018m <- mlidhs2018m %>% mutate(
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
mlidhs2018m <- mlidhs2018m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mlidhs2018m <- mlidhs2018m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mlidhs2018m <- mlidhs2018m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile for specified labels
mlidhs2018m <- mlidhs2018m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
mlidhs2018m <- mlidhs2018m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
mlidhs2018m <- mlidhs2018m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:23 ~ 2,     # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,          # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))

# Combining male and female for Mali
combined_mlidhs <- bind_rows(mlidhs2018f, mlidhs2018m)


#-------------------------------------------------------------------------------------

# Mauritania 2019-21

#-----Female------
mrtdhs_female_ind <- read_dta("Mauritania_2019-21_DHS\\MRIR71DT\\MRIR71FL.DTA")

# Selecting and renaming variables
mrtdhs2019f <- mrtdhs_female_ind %>%
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
    #exchange_sex = v791a,           #(No obs) Had sex in return for gifts, cash or other in the last 12 months
    #total_partners =  v766b,        # (No obs) Number of sex partners in the last 12 months
    ever_heard = v751,              # Ever heard of HIV
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v473b         # result of last hiv test (not available)
  ) 


# Calculating the median year of interview
median(mrtdhs2019f$year, na.rm = TRUE) #median year 2020


# Adding columns for country, survey ID
mrtdhs2019f <- mrtdhs2019f %>% mutate(country = "Mauritania", 
                                      survey_id = "MRT2019DHS",
                                      med_year = 2020
                                      )

# Reordering columns to make country and survey_id the leftmost columns
mrtdhs2019f <- mrtdhs2019f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
mrtdhs2019f <- mrtdhs2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mrtdhs2019f <- mrtdhs2019f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mrtdhs2019f$province <- as.character(mrtdhs2019f$province)

# Creating the hivst_knwldge and hivst_use variables
mrtdhs2019f <- mrtdhs2019f %>% mutate(
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
mrtdhs2019f <- mrtdhs2019f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mrtdhs2019f <- mrtdhs2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mrtdhs2019f <- mrtdhs2019f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile the specified labels
mrtdhs2019f <- mrtdhs2019f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the last_hivtest
mrtdhs2019f <- mrtdhs2019f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))


#-----Male------

mrtdhs_male_ind <- read_dta("Mauritania_2019-21_DHS\\MRMR71DT\\MRMR71FL.DTA")

# Selecting and renaming variables (no obs for exchange_sex and total_partners)
mrtdhs2019m <- mrtdhs_male_ind %>%
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
    #exchange_sex = mv791a,           # Had sex in return for gifts, cash or other in the last 12 months
    #total_partners = mv766b,         # Number of sex partners in the last 12 months
    ever_heard = mv751,              # Ever heard of HIV
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    #last_hivresult = mv861           # result of last hiv test (NA)
  ) 

# Calculating the median year of interview
median(mrtdhs2019m$year, na.rm = TRUE) #median year 2020

# Adding columns for country, survey ID
mrtdhs2019m <- mrtdhs2019m %>% mutate(country = "Mauritania", 
                                      survey_id = "MRT2019DHS",
                                      med_year = 2020)

# Reordering columns to make country and survey_id the leftmost columns
mrtdhs2019m <- mrtdhs2019m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
mrtdhs2019m <- mrtdhs2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
mrtdhs2019m <- mrtdhs2019m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
mrtdhs2019m$province <- as.character(mrtdhs2019m$province)

# Creating the hivst_knwldge and hivst_use variables
mrtdhs2019m <- mrtdhs2019m %>% mutate(
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
mrtdhs2019m <- mrtdhs2019m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
mrtdhs2019m <- mrtdhs2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
mrtdhs2019m <- mrtdhs2019m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile for specified labels
mrtdhs2019m <- mrtdhs2019m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the last_hivtest
mrtdhs2019m <- mrtdhs2019m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:23 ~ 2,     # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,          # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))


# while combining, conflicting value labels for the strata variable between the two datasets:
#In mrtdhs2019f, strata value 20 is labeled as "guidimagha - rural."
#In mrtdhs2019m, strata value 20 is labeled as "guidimagha - urban.

# Check value labels for strata in each dataset
labelled::val_labels(mrtdhs2019f$strata)
labelled::val_labels(mrtdhs2019m$strata)

# Standardizing the label in mrtdhs2019m to match mrtdhs2019f
mrtdhs2019m <- mrtdhs2019m %>%
  mutate(strata = ifelse(strata == 20, 20, strata)) %>%
  mutate(strata = haven::labelled(strata, 
                                  labels = c("hodh echargui - urban" = 1, "hodh echargui - rural" = 2, 
                                             "hodh gharbi - urban" = 3, "hodh gharbi - rural" = 4,
                                             "assaba - urban" = 5, "assaba - rural" = 6,
                                             "gorgol - urban" = 7, "gorgol - rural" = 8,
                                             "brakna - urban" = 9, "brakna - rural" = 10,
                                             "trarza - urban" = 11, "trarza - rural" = 12,
                                             "adrar - urban" = 13, "adrar - rural" = 14,
                                             "dakhlet nouadhibou - urban" = 15, "dakhlet nouadhibou - rural" = 16,
                                             "tagant - urban" = 17, "tagant - rural" = 18,
                                             "guidimagha - urban" = 19, "guidimagha - rural" = 20,
                                             "tiris zemour - urban" = 21, "tiris zemour - rural" = 22,
                                             "inchiri - urban" = 23, "inchiri - rural" = 24,
                                             "nouakchott ouest" = 25, "nouakchott nord" = 26,
                                             "nouakchott sud" = 27)))

# Combining male and female for Mauritania
combined_mrtdhs <- bind_rows(mrtdhs2019f, mrtdhs2019m)

#-------------------------------------------------------------------------------

# Rwanda 2019-20 

#-----Female------
rwadhs_female_ind <- read_dta("Rwanda_2019-20_DHS\\RWIR81DT\\RWIR81FL.DTA")

# Selecting and renaming variables
rwadhs2019f <- rwadhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v861         # result of last hiv test (not available)
  ) 

# Calculating the median year of interview
median(rwadhs2019f$year, na.rm = TRUE) #median year 2020


# Adding columns for country, survey ID
rwadhs2019f <- rwadhs2019f %>% mutate(country = "Rwanda", 
                                      survey_id = "RWA2019DHS",
                                      med_year = 2020)

# Reordering columns to make country and survey_id the leftmost columns
rwadhs2019f <- rwadhs2019f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
rwadhs2019f <- rwadhs2019f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
rwadhs2019f <- rwadhs2019f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
rwadhs2019f$province <- as.character(rwadhs2019f$province)

# Creating the hivst_knwldge and hivst_use variables
rwadhs2019f <- rwadhs2019f %>% mutate(
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
rwadhs2019f <- rwadhs2019f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
rwadhs2019f <- rwadhs2019f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
rwadhs2019f <- rwadhs2019f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile the specified labels
rwadhs2019f <- rwadhs2019f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
rwadhs2019f <- rwadhs2019f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
rwadhs2019f <- rwadhs2019f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))


#-----Male------

rwadhs_male_ind <- read_dta("Rwanda_2019-20_DHS\\RWMR81DT\\RWMR81FL.DTA")

# Selecting and renaming variables
rwadhs2019m <- rwadhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    #last_hivresult = mv861           # result of last hiv test (NA)
  ) 

# Calculating the median year of interview
median(rwadhs2019m$year, na.rm = TRUE) #median year 2020


# Adding columns for country, survey ID
rwadhs2019m <- rwadhs2019m %>% mutate(country = "Rwanda", 
                                      survey_id = "RWA2019DHS",
                                      med_year = 2020)

# Reordering columns to make country and survey_id the leftmost columns
rwadhs2019m <- rwadhs2019m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
rwadhs2019m <- rwadhs2019m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
rwadhs2019m <- rwadhs2019m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
rwadhs2019m$province <- as.character(rwadhs2019m$province)

# Creating the hivst_knwldge and hivst_use variables
rwadhs2019m <- rwadhs2019m %>% mutate(
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
rwadhs2019m <- rwadhs2019m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
rwadhs2019m <- rwadhs2019m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
rwadhs2019m <- rwadhs2019m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile for specified labels
rwadhs2019m <- rwadhs2019m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
rwadhs2019m <- rwadhs2019m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
rwadhs2019m <- rwadhs2019m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% 0:11 ~ 1,      # Tested less than 1 year ago
  last_hivtest %in% 12:23 ~ 2,     # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,          # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,         # Don't know
  TRUE ~ NA_real_                  # Missing or other values
))

# Combining male and female for Rwanda
combined_rwadhs <- bind_rows(rwadhs2019f, rwadhs2019m)

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

#extract design adjusted SE
sledhs_design <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = sledhs2019f, nest = TRUE)
sle_prop <- svyciprop(~I(hivst_use == 1), design = sledhs_design, method = "logit", level = 0.95)
ci <- confint(sle_prop)  
se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

sleprop_hivst_use_f <- 0.0387
sle_se_hivst_use_f <- 0.003777449

eff_ss_f <- (sleprop_hivst_use_f * (1 - sleprop_hivst_use_f)) / (sle_se_hivst_use_f^2)
sledhsf_num <- sleprop_hivst_use_f * eff_ss_f

#2019 sle_f (denominator: 2607.19, numerator: 100.8983)

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


#extract design adjusted SE
sledhs_design_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = sledhs2019m, nest = TRUE)
sle_prop <- svyciprop(~I(hivst_use == 1), design = sledhs_design_m, method = "logit", level = 0.95)
ci <- confint(sle_prop)  
se_hivst_use <- (ci[2] - ci[1]) / (2 * 1.96)  

sleprop_hivst_use_m <-  0.0213
sle_se_hivst_use_m <- 0.002677955

eff_ss_m <- (sleprop_hivst_use_m * (1 - sleprop_hivst_use_m)) / (sle_se_hivst_use_m^2)
sledhsm_num <- sleprop_hivst_use_m * eff_ss_m

#2019 sle_m (denominator: 2906.85, numerator: 61.91591)

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

# agegrp conflicts in female and male dataset

labelled::val_labels(zafdhs2016f$agegrp)
labelled::val_labels(zafdhs2016m$agegrp)

# Recode 50+ in the female dataset to match 50-54 in the male dataset
zafdhs2016f <- zafdhs2016f %>%
  mutate(agegrp = ifelse(agegrp == 8, 8, agegrp)) %>%
  mutate(agegrp = haven::labelled(agegrp, labels = c(
    "15-19" = 1, "20-24" = 2, "25-29" = 3, "30-34" = 4,
    "35-39" = 5, "40-44" = 6, "45-49" = 7, "50-54" = 8)))


# Combining male and female for South Africa
combined_zafdhs <- bind_rows(zafdhs2016f, zafdhs2016m)


#------------------------------------------------------------------------------

# Tanzania 2022

#-----Female------
tzadhs_female_ind <- read_dta("Tanzania_2022_DHS\\TZIR82DT\\TZIR82FL.DTA")

# Selecting and renaming variables
tzadhs2022f <- tzadhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    last_hivresult = v861           # result of last hiv test 
  ) 

# Calculating the median year of interview
median(tzadhs2022f$year, na.rm = TRUE) #2022

# Adding columns for country, survey ID
tzadhs2022f <- tzadhs2022f %>% mutate(
  country = "Tanzania", survey_id = "TZA2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
tzadhs2022f <- tzadhs2022f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
tzadhs2022f <- tzadhs2022f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
tzadhs2022f <- tzadhs2022f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
tzadhs2022f$province <- as.character(tzadhs2022f$province)


# Creating the hivst_knwldge and hivst_use variables
tzadhs2022f <- tzadhs2022f %>% mutate(
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
tzadhs2022f <- tzadhs2022f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
tzadhs2022f <- tzadhs2022f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
tzadhs2022f <- tzadhs2022f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the schl_years variable
tzadhs2022f <- tzadhs2022f %>%
  mutate(schl_years = case_when(
    schl_years == 8 ~ 88,  # Recode 8 as 88 (don't know)
    is.na(schl_years) ~ NA_real_,  # Ensure missing values remain NA
    TRUE ~ schl_years  # Keep other values the same
  ))

# Recoding the wealth quintile the specified labels
tzadhs2022f <- tzadhs2022f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
tzadhs2022f <- tzadhs2022f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
tzadhs2022f <- tzadhs2022f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest %in% c(24:96) ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))

# Recoding the last hiv result
tzadhs2022f <- tzadhs2022f %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

#-----Male------

tzadhs_male_ind <- read_dta("Tanzania_2022_DHS\\TZMR82DT\\TZMR82FL.DTA")

# Selecting and renaming variables
tzadhs2022m <- tzadhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
    last_hivresult = mv861           # result of last hiv test 
  ) 

# Calculating the median year of interview
median(tzadhs2022m$year, na.rm = TRUE) #2022

# Adding columns for country, survey ID
tzadhs2022m <- tzadhs2022m %>% mutate(
  country = "Tanzania", survey_id = "TZA2022DHS", med_year = 2022)

# Reordering columns to make country and survey_id the leftmost columns
tzadhs2022m <- tzadhs2022m %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 1 (men)
tzadhs2022m <- tzadhs2022m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
tzadhs2022m <- tzadhs2022m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
tzadhs2022m$province <- as.character(tzadhs2022m$province)


# Creating the hivst_knwldge and hivst_use variables
tzadhs2022m <- tzadhs2022m %>% mutate(
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
tzadhs2022m <- tzadhs2022m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
tzadhs2022m <- tzadhs2022m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
tzadhs2022m <- tzadhs2022m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile for specified labels
tzadhs2022m <- tzadhs2022m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))
# Recoding the schl_years variable
tzadhs2022m <- tzadhs2022m %>%
  mutate(schl_years = case_when(
    schl_years == 8 ~ 88,  # Recode 8 as 88 (don't know)
    is.na(schl_years) ~ NA_real_,  # Ensure missing values remain NA
    TRUE ~ schl_years  # Keep other values the same
  ))

# Recoding the total_partners variable
tzadhs2022m <- tzadhs2022m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the last_hivtest
tzadhs2022m <- tzadhs2022m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest %in% c(24:96) ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))

# Recoding the last hiv result
tzadhs2022m <- tzadhs2022m %>% mutate(last_hivresult = case_when(
  last_hivresult == 1 ~ 1,   # Positive
  last_hivresult == 2 ~ 0,   # Negative
  last_hivresult == 3 ~ 3,   # Indeterminate
  last_hivresult == 4 ~ 98,  # Refused
  last_hivresult == 5 ~ 4,   # Didn't receive result
  TRUE ~ NA_real_            # Missing or other values
))

# Combining male and female for Tanzania 2022
combined_tzadhs <- bind_rows(tzadhs2022f, tzadhs2022m)

#-------------------------------------------------------------------------------------


# Uganda 2016

#-----Female------
ugadhs_female_ind <- read_dta("Uganda_2016_DHS\\UGIR7BDT\\UGIR7BFL.DTA")

# Selecting and renaming variables
ugadhs2016f <- ugadhs_female_ind %>%
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
    selftest_dhs= v856,             # Knowledge and use of HIV self-test
    ever_tested = v781,             # Ever tested for HIV
    received_hivresult =  v828,     # Whether received result from last HIV test
    last_hivtest = v826a,           # How many months ago last tested for HIV 
    #last_hivresult = v861           # result of last hiv test (not available)
  ) 

# Calculating the median year of interview
median(ugadhs2016f$year, na.rm = TRUE) #2016

# Adding columns for country, survey ID
ugadhs2016f <- ugadhs2016f %>% mutate(
  country = "Uganda", survey_id = "UGA2016DHS", med_year = 2016)

# Reordering columns to make country and survey_id the leftmost columns
ugadhs2016f <- ugadhs2016f %>% select(country, survey_id, med_year, everything())

# Adding the 'sex' column with value 0 (women)
ugadhs2016f <- ugadhs2016f %>% mutate(sex = 0) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
ugadhs2016f <- ugadhs2016f %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
ugadhs2016f$province <- as.character(ugadhs2016f$province)


# Creating the hivst_knwldge and hivst_use variables
ugadhs2016f <- ugadhs2016f %>% mutate(
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
ugadhs2016f <- ugadhs2016f %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
ugadhs2016f <- ugadhs2016f %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
ugadhs2016f <- ugadhs2016f %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))


# Recoding the wealth quintile the specified labels
ugadhs2016f <- ugadhs2016f %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))

# Recoding the total_partners variable
ugadhs2016f <- ugadhs2016f %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))

# Recoding the last_hivtest
ugadhs2016f <- ugadhs2016f %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))


#-----Male------

ugadhs_male_ind <- read_dta("Uganda_2016_DHS\\UGMR7BDT\\UGMR7BFL.DTA")

# Selecting and renaming variables
ugadhs2016m <- ugadhs_male_ind %>%
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
    selftest_dhs = mv856,            # Knowledge and use of HIV self-test
    ever_tested = mv781,             # Ever tested for HIV
    received_hivresult =  mv828,     # Whether received result from last HIV test
    last_hivtest = mv826a,           # How many months ago last tested for HIV 
   # last_hivresult = mv861         # result of last hiv test(na) 
  ) 

# Calculating the median year of interview
median(ugadhs2016m$year, na.rm = TRUE) #2016

# Adding columns for country, survey ID
ugadhs2016m <- ugadhs2016m %>% mutate(
  country = "Uganda", survey_id = "UGA2016DHS", med_year = 2016)

# Reordering columns to make country and survey_id the leftmost columns
ugadhs2016m <- ugadhs2016m %>% select(country, survey_id, med_year,  everything())

# Adding the 'sex' column with value 1 (men)
ugadhs2016m <- ugadhs2016m %>% mutate(sex = 1) %>% relocate(sex, .after = agegrp)

# Correcting individual weights 
ugadhs2016m <- ugadhs2016m %>% mutate(ind_wt = ind_wt / 1e6)

# Recoding province
ugadhs2016m$province <- as.character(ugadhs2016m$province)


# Creating the hivst_knwldge and hivst_use variables
ugadhs2016m <- ugadhs2016m %>% mutate(
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
ugadhs2016m <- ugadhs2016m %>% relocate(hivst_knwldge, hivst_use, .after = selftest_dhs)

# Recoding region
ugadhs2016m <- ugadhs2016m %>% mutate(region = case_when(
  region == 1 ~ 1,   # Urban
  region == 2 ~ 0,   # Rural
  TRUE ~ NA_real_    # Missing
))

# Recoding the curr_marital 
ugadhs2016m <- ugadhs2016m %>% mutate(curr_marital = case_when(
  curr_marital == 0 ~ 1,   # Single
  curr_marital == 3 ~ 2,   # Widowed
  curr_marital == 4 ~ 3,   # Divorced
  curr_marital == 5 ~ 4,   # Separated
  curr_marital %in% c(1, 2) ~ 5,  # Married/In a Union
  TRUE ~ NA_real_          # Missing or any other values
))

# Recoding the wealth quintile for specified labels
ugadhs2016m <- ugadhs2016m %>% mutate(wealth_index = factor(wealth_index, 
                                                            levels = c(1, 2, 3, 4, 5), 
                                                            labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")))


# Recoding the total_partners variable
ugadhs2016m <- ugadhs2016m %>% mutate(total_partners = case_when(
  total_partners == 98 ~ 88,   # Recode 98 as 88 (don't know)
  total_partners == 95 ~ 98,   # Recode 95 as 98 (refused)
  TRUE ~ total_partners        # Keep other values the same
))


# Recoding the last_hivtest
ugadhs2016m <- ugadhs2016m %>% mutate(last_hivtest = case_when(
  last_hivtest %in% c(0:11) ~ 1,  # Tested <12 months ago
  last_hivtest %in% c(12:23) ~ 2, # Tested 1-2 years ago
  last_hivtest == 95 ~ 3,      # Tested more than 2 years ago
  last_hivtest == 98 ~ 88,     # Don't know
  TRUE ~ NA_real_              # Missing 
))


# Combining male and female for Uganda 2016
combined_ugadhs <- bind_rows(ugadhs2016f, ugadhs2016m)

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

#prop by age and by sex

survey_designs <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = zwedhs2015f, nest = TRUE)
hivst_by_age_zimf <- svyby(~I(hivst_use == 1), ~agegrp, survey_designs,
                      svyciprop, method = "logit", vartype = "ci")

#testing prop by sex (matches statcompiler)

zwedhs_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = combined_zwedhs, nest = TRUE)
#hivst_by_sex <- svyby(~I(hivst_use == 1), ~sex, zwedhs_design, svyciprop, method = "logit", vartype = "ci")

female_hivst_numerator <- svytotal(~I(hivst_use == 1 & sex == 0), zwedhs_design)
male_hivst_numerator <- svytotal(~I(hivst_use == 1 & sex == 1), zwedhs_design)

female_hivst_denominator <- svytotal(~I(sex == 0), zwedhs_design)
male_hivst_denominator <- svytotal(~I(sex == 1), zwedhs_design)


#Female:  Num: 27.08, Deno: 9955, Male: Num: 136.13, Deno: 8396




# Combining all the DHS
list_dhs <- list(
  combined_bendhs,
  combined_bfadhs,
  combined_bdidhs,
  combined_cmrdhs,
  combined_civdhs,
  combined_gabdhs,
  combined_ghadhs,
  combined_gindhs,
  combined_kendhs,
  combined_lbrdhs,
  combined_mdgdhs,
  combined_mwidhs,
  combined_mlidhs,
  combined_mrtdhs,
  combined_rwadhs,
  combined_sendhs,
  combined_sledhs,
  combined_zafdhs,
  combined_tzadhs,
  combined_ugadhs,
  combined_zmbdhs,
  combined_zwedhs
)


# Combining all the DHS females
list_dhs_f <- list(
  bendhs2017f,
  bfadhs2021f,
  bdidhs2016f,
  cmrdhs2018f,
  civdhs2021f,
  gabdhs2019f,
  ghadhs2022f,
  gindhs2018f,
  kendhs2022f,
  lbrdhs2019f,
  mdgdhs2021f,
  mwidhs2015f,
  mlidhs2018f,
  mrtdhs2019f,
  rwadhs2019f,
  sendhs2017f,
  sledhs2019f,
  zafdhs2016f,
  tzadhs2022f,
  ugadhs2016f,
  zmbdhs2018f,
  zwedhs2015f
)


# Combining all the DHS males
list_dhs_m <- list(
  bendhs2017m,
  bfadhs2021m,
  bdidhs2016m,
  cmrdhs2018m,
  civdhs2021m,
  gabdhs2019m,
  ghadhs2022m,
  gindhs2018m,
  kendhs2022m,
  lbrdhs2019m,
  mdgdhs2021m,
  mwidhs2015m,
  mlidhs2018m,
  mrtdhs2019m,
  rwadhs2019m,
  sendhs2017m,
  sledhs2019m,
  zafdhs2016m,
  tzadhs2022m,
  ugadhs2016m,
  zmbdhs2018m,
  zwedhs2015m
)


# Saving
#saveRDS(list_dhs, file = "list_dhs.rds")




