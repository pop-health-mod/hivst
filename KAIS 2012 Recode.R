# Clearing all objects
rm(list = ls())
gc()

setwd("D:/Downloads/MSc Thesis/hiv-selftesting/1. thesis rawdata/KAIS raw data")

library(haven)
library(dplyr)
library(lubridate)
library(labelled)
library(survey)

# Loading the KAIS 2012 dataset
kais_adult_ind <- read_dta("alladults.dta")

# Selecting and renaming our desired variables from KAIS 2012 
kais <- select(kais_adult_ind,
               psu = qclust,
               strata = strata,
               province = qprov,
               region = qresid,
               ind_wt = aiweight,
               year = interviewdate, # survey start year extracted from interview date
               hhid = householdid,
               age = q_102,
               sex = sex,
               curr_marital = q_203,
               schl_years = q_104,
               wealth_index = windex5,
               exchange_sex = q_435,
               total_partners = sexprnsyr,
               ever_heard = q_501,
               hiv_wt = abweight,
               hiv_status = hiv,
               hivst_use = q_607,
               ever_tested = q_603,
               last_hivtest = q_605,
               received_hivresult = q_353, #only asked to females
               last_hivresult = q_702) %>% 
  filter (!is.na(ever_heard))

# Converting the year variable (which contains interviewdate) to a date format
kais <- kais %>%
  mutate(year = as.Date(year, format = "%d%b%Y"))

# Extracting the year from the interview date for year column
kais <- kais %>%
  mutate(year = as.numeric(format(year, "%Y")))

# # Calculating the median year of interview
median(kais$year, na.rm = TRUE) # median interview year 2012


# Adding columns for country and survey ID
kais <- kais %>%
  mutate(country = "Kenya",
         survey_id = "KEN2012KAIS",
         med_year = 2012)

# Reordering columns to make country and survey_id the leftmost columns
kais <- kais %>%
  select(country, survey_id, med_year, everything())

# Converting province
kais$province <- as.character(as_factor(kais$province))
kais$province <- gsub("\\(.*\\)\\s*", "", kais$province)

# Recoding region
kais <- kais %>%
  mutate(region = case_when(
    region == 1 ~ 0,  # Rural
    region == 2 ~ 1,  # Urban
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age
# Create agegrp variable with 5-year age categories
kais <- kais %>%
  mutate(agegrp = case_when(
    age >= 15 & age <= 19 ~ 1,
    age >= 20 & age <= 24 ~ 2,
    age >= 25 & age <= 29 ~ 3,
    age >= 30 & age <= 34 ~ 4,
    age >= 35 & age <= 39 ~ 5,
    age >= 40 & age <= 44 ~ 6,
    age >= 45 & age <= 49 ~ 7,
    age >= 50 & age <= 54 ~ 8,
    age >= 55 & age <= 59 ~ 9,
    age >= 60 & age <= 64 ~ 10,
    age >= 65           ~ 11,  # For all ages 65 and above
    is.na(age)          ~ NA_real_  # missing values
  ))

# Move the agegrp column to the right of the age column
kais <- kais %>%
  select(psu:age, agegrp, everything())

# Recode the sex variable
kais <- kais %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Men remain 1
    sex == 2 ~ 0,   # Women recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

#current marital status is coded as numeric, so I manually labelled the curr_marital variable based on the codebook
kais$curr_marital <- labelled::labelled(kais$curr_marital, 
                                        c("Single (Not in a Union)" = 1, 
                                          "Widowed" = 2, 
                                          "Divorced" = 3, 
                                          "Separated" = 4, 
                                          "Married/In a Union" = 5,
                                         "Missing" = NA_real_))

#Total years of schooling not available, so we took highest level of schooling completed

# recoding schooling
kais <- kais %>%
  mutate(schl_years = case_when(
    schl_years %in% c(1, 2) ~ 1,          # Primary
    schl_years %in% c(3, 4, 5) ~ 2,       # Secondary
    schl_years %in% c(6, 7, 8, 9) ~ 3,    # Tertiary
    schl_years == 88 ~ 88,                # Don't know
    is.na(schl_years) ~ NA_real_          # Missing values remain as NA
  ))


# Recode and label the exchange_sex variable
kais$exchange_sex <- kais$exchange_sex %>%
  dplyr::recode(`2` = 0) %>%  # Recode 'No' from 2 to 0
  labelled::labelled(c("Yes" = 1, 
                       "No" = 0, 
                       "Refused" = 98,
                       "Missing" = NA_real_))


# Recode and label the ever_heard of HIV variable
kais$ever_heard <- kais$ever_heard %>%
  dplyr::recode(`2` = 0, `99` = NA_real_) %>%  
  labelled::labelled(c("Yes" = 1, 
                       "No" = 0, 
                       "Refused" = 98, 
                       "Missing" = NA_real_))  

# Recode and label the hiv_status variable
kais$hiv_status <- kais$hiv_status %>%
  dplyr::recode(`2` = 0, `99` = NA_real_) %>%  # Recode Negative (2) to 0, and missing (99) to NA
  labelled::labelled(c("Positive" = 1, 
                       "Negative" = 0, 
                       "Missing" = NA_real_))  

# Recode and label the hivst_use variable
kais <- kais %>% mutate(hivst_use = case_when(
   hivst_use == 1 ~ 1,         # Keep '1' unchanged
   hivst_use == 2 ~ 0,         # Recode '2' as '0'
    hivst_use == 9 ~ NA_real_,  # Recode '9' as NA
    #is.na(hivst_use) ~ 0          # Recode all NAs as 0 (not used in KAIS)
  )) 

# Manually label the ever_tested variable
kais$ever_tested <- labelled::labelled(kais$ever_tested, 
                                       c("Yes" = 1, 
                                         "No" = 2, 
                                         "Refused" = 98, 
                                         "Missing" = NA_real_))
# Recode the ever_tested variable
kais$ever_tested <- kais$ever_tested %>%
  dplyr::recode(`2` = 0, `99` = NA_real_) %>%  # Recode No (2) to 0, and 99 to NA
  labelled::labelled(c("Yes" = 1, 
                       "No" = 0, 
                       "Refused" = 98, 
                       "Missing" = NA_real_))

# Recode the received_hivresult variable (only asked to females)
kais$received_hivresult <- kais$received_hivresult %>%
  dplyr::recode(`2` = 0, `99` = NA_real_) %>%  # Recode No (2) to 0, and any 99 to NA
  labelled::labelled(c("Yes" = 1, 
                       "No" = 0, 
                       "Missing" = NA_real_))

# Recode and label the last_hivresult variable
kais$last_hivresult <- labelled::labelled(kais$last_hivresult, 
                                          c("Positive" = 1, 
                                            "Negative" = 2, 
                                            "Indeterminate" = 3, 
                                            "Did Not Receive Result" = 4, 
                                            "Don't Know" = 88,
                                            "Missing" = NA_real_))

# Recode Negative (2) to 0 and keep other labels the same
kais$last_hivresult <- kais$last_hivresult %>%
  dplyr::recode(`2` = 0) %>%  # Recode Negative (2) to 0
  labelled::labelled(c("Positive" = 1, 
                       "Negative" = 0, 
                       "Indeterminate" = 3, 
                       "Did Not Receive Result" = 4, 
                       "Don't Know" = 88,
                       "Missing" = NA_real_))



# recoding last hiv test how many months ago
kais <- kais %>%
  mutate(
    last_hivtest = case_when(
      last_hivtest %in% c(1, 2, 3) ~ 1,   # Tested <12 months ago
      last_hivtest == 4 ~ 2,              # Tested 1-2 years ago
      last_hivtest == 5 ~ 3,              # Tested more than 2 years ago
      last_hivtest == 99 ~ 98,            # Refused
      last_hivtest == 8 ~ 88,             # Don't know
      TRUE ~ NA_real_                     # Missing
    )
  ) 


#Create separate dataframes for male and female for KAIS
kais_f <- kais %>% filter(sex == 0)  # Female
kais_m <- kais %>% filter(sex == 1)  # Male

# Saving the kais dataframe
#saveRDS(kais, file = "kais_dataframe.rds")

# checking weighted proportion (manually 0.02)
#kais_design <- svydesign(ids = ~psu, strata = ~strata, weights = ~ind_wt, data = kais, nest = TRUE)

# Calculate the proportion of people who used HIV self-testing
#prop_hivst_use_kais <- svyciprop(~I(hivst_use == 1), design = kais_design, method = "logit", level = 0.95)
#prop_hivst_use_kais #0.034 (0.03, 0.04) 
 
# sample size = 13694


# Calculate hivst proportion by sex
#hivst_by_sex <- svyby(~I(hivst_use == 1), ~sex, kais_design, svyciprop, method = "logit", vartype = "ci")

#sex I(hivst_use == 1)       ci_l       ci_u
#0   0        0.02108996 0.01727204 0.02572972
#1   1        0.05243847 0.03506925 0.07771751


