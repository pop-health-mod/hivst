
rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(lubridate)
library(survey)
library(sjlabelled)


setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/PHIA raw data")

#-----Namibia 2017-----
namphia2017_adult_ind <- read_dta("Namibia 2017 PHIA/namphia2017adultind.dta")
namphia2017_adult_bio <- read_dta("Namibia 2017 PHIA/namphia2017adultbio.dta")

# Merging the datasets on householdid and personid
merged_namibia <- namphia2017_adult_ind %>%
  full_join(namphia2017_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical, keeping only one of each
merged_namibia <- merged_namibia %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# creating exchange_sex variable
merged_namibia <- merged_namibia %>%
  mutate(
    exchange_sex = case_when(
      # Check Yes conditions for each partlastsup variable
      (partlastsup1 == 1 & (partlastsupwhat_b1 == "B" | partlastsupwhat_x1 == "X")) |
        (partlastsup2 == 1 & (partlastsupwhat_b2 == "B" | partlastsupwhat_x2 == "X")) |
        (partlastsup3 == 1 & (partlastsupwhat_b3 == "B" | partlastsupwhat_x3 == "X")) ~ 1,  # Yes
      
      # Check No conditions for each partlastsup variable
      (partlastsup1 == 1 & partlastsupwhat_a1 == "A") |
        (partlastsup2 == 1 & partlastsupwhat_a2 == "A") |
        (partlastsup3 == 1 & partlastsupwhat_a3 == "A") ~ 0,                              # No
      
      # Check Refused cases
      partlastsup1 == -9 | partlastsup2 == -9 | partlastsup3 == -9 ~ 98,              # Refused
      
      # Default to NA if none of the above apply
      TRUE ~ NA_integer_
    )
  )



# Check the distribution of exchange_sex
table(merged_namibia$exchange_sex, useNA = "ifany")

# Selecting and renaming variables
namphia2017 <- merged_namibia %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province = region,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year = surveystyear,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schlhi,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    exchange_sex,                     # Had sex in return for gifts, cash or other in the last 12 months
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    ever_heard = adlthrdhiv,          # Ever heard of HIV
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    confirmatory_testing = hivselfcnfm,# Sought confirmatory testing after self-testing
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
  )

table(namphia2017$hiv_status, useNA = "ifany")


# recode ART for analysis
table(namphia2017$curr_art, useNA = "ifany")
namphia2017 <- namphia2017 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0  # missing ART
  ))

# Calculating the median year of interview
median(namphia2017$year, na.rm = TRUE) # median interview year 2017

# Adding columns for country and survey ID
namphia2017 <- namphia2017 %>%
  mutate(
    country = "Namibia",
    survey_id = "NAM2017PHIA",
    med_year = 2017
  )

# Reordering columns to make country and survey_id the leftmost columns
namphia2017 <- namphia2017 %>%
  select(country, survey_id, med_year, everything())

# Recoding region
namphia2017 <- namphia2017 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
namphia2017 <- namphia2017 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
namphia2017 <- namphia2017 %>%
  select(country:age, agegrp, everything())

# Recoding sex
namphia2017 <- namphia2017 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
namphia2017 <- namphia2017 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# Recoding schooling
namphia2017 <- namphia2017 %>%
  mutate(
    schl_years = case_when(
      schl_years == 1 ~ 1,               # PRIMARY
      schl_years == 2 ~ 2,               # SECONDARY
      schl_years == 3 ~ 3,               # HIGHER/TERTIARY
      schl_years == -8 ~ 88,             # DON’T KNOW
      schl_years == -9 ~ 98,             # REFUSED
      is.na(schl_years) ~ as.integer(NA) # Handle missing values
    )
  )

# Recode the 'total_partners' variable
namphia2017 <- namphia2017 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      TRUE ~ total_partners                # Keep all other values as they are
    )
  )

# Recoding the 'ever_heard' variable
namphia2017 <- namphia2017 %>%
  mutate(
    ever_heard = case_when(
      ever_heard == 1 ~ 1,            # Yes
      ever_heard == 2 ~ 0,            # No
      ever_heard == -9 ~ 98,          # Refused
      ever_heard == -8 ~ 88,         # Don't know
      TRUE ~ NA_integer_              # Catch any unexpected cases as NA
    )
  )

# Recode the 'hiv_status' variable
namphia2017 <- namphia2017 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing or any other values to NA
    )
  )

# Recode the 'hivst_use' variable
namphia2017 <- namphia2017 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ NA_integer_            # Missing or any other values to NA
    )
  )

# Recode the 'confirmatory_testing' variable
namphia2017 <- namphia2017 %>%
  mutate(
    confirmatory_testing = case_when(
      confirmatory_testing == 1 ~ 1,           # Yes
      confirmatory_testing == 2 ~ 0,           # No
      confirmatory_testing == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_                       # Missing or any other values to NA
    )
  )

# Recode the 'ever_tested' variable
namphia2017 <- namphia2017 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
namphia2017 <- namphia2017 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
namphia2017 <- namphia2017 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
namphia2017 <- namphia2017 %>%
  mutate( reference_date = make_date(2017, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
namphia2017 <- namphia2017 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )

# filtering for not on art  and have info and hiv status and hivst use
table(namphia2017$hiv_status, useNA = "ifany")
namphia2017 <- namphia2017 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(namphia2017$curr_art, useNA = "ifany")
namphia2017 <- namphia2017 %>%
  filter(curr_art %in% 0)


#--converting into factor and doing regression----
# hivst use
str(namphia2017$hivst_use)
namphia2017$hivst_use <- factor(namphia2017$hivst_use, 
                                       levels = c(0, 1),
                                       labels = c("No", "Yes"))

# hiv status
str(namphia2017$hiv_status)
namphia2017$hiv_status <- factor(namphia2017$hiv_status,
                                        levels = c(0, 1),
                                        labels = c("Negative", "Positive"))

# sex
str(namphia2017$sex)
namphia2017$sex <- factor(namphia2017$sex, 
                                 levels = c(0, 1),
                                 labels = c("Female", "Male"))

# age group
str(namphia2017$agegrp)
table(namphia2017$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
namphia2017$agegrp <- factor(
  namphia2017$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(namphia2017$region)
str(namphia2017$region)
namphia2017$region <- factor(namphia2017$region,
                                    levels = c("0", "1"),
                                    labels = c("Rural", "Urban"))

# wealth index
table(namphia2017$wealth_index)
str(namphia2017$wealth_index)

namphia2017$wealth_index <- factor(
  namphia2017$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(namphia2017$schl_years)
table(namphia2017$schl_years)
namphia2017$schl_years <- factor(namphia2017$schl_years,
                                        levels = c("1", "2", "3"),
                                        labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(namphia2017$survey_id)
str(namphia2017$survey_id)
namphia2017$survey_id <- factor(namphia2017$survey_id)


# logistic reg for meta analysis (male and female seperately)
namphia2017f <- namphia2017 %>% filter(sex == "Female")  # Female
namphia2017m <- namphia2017 %>% filter(sex == "Male")  # Male

# checking
table(namphia2017$sex)
table(namphia2017f$sex)
table(namphia2017m$sex)


# logistic reg for meta analysis (Female)
logisticf <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                 data = namphia2017f, family = "binomial")
summary(logisticf)

# extracting estimate & SE for RE meta analysis
coef_hiv_statusf <- coef(logisticf)["hiv_statusPositive"]
se_hiv_statusf  <- sqrt(vcov(logisticf)["hiv_statusPositive", "hiv_statusPositive"])

df_survey1f <- data.frame(
  survey  = "NAMPHIA2017F",
  logOR   = coef_hiv_statusf,
  seLogOR = se_hiv_statusf
)

# logistic reg for meta analysis (male)
logisticm <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                 data = namphia2017m, family = "binomial")
summary(logisticm)

# extracting estimate & SE for RE meta analysis
coef_hiv_statusm <- coef(logisticm)["hiv_statusPositive"]
se_hiv_statusm  <- sqrt(vcov(logisticm)["hiv_statusPositive", "hiv_statusPositive"])

df_survey1m <- data.frame(
  survey  = "NAMPHIA2017M",
  logOR   = coef_hiv_statusm,
  seLogOR = se_hiv_statusm
)

# logistic reg for meta analysis (overall)
logistic1 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                      data = namphia2017, family = "binomial")
summary(logistic1)

# extracting estimate & SE for RE meta analysis
coef_hiv_status1 <- coef(logistic1)["hiv_statusPositive"]
se_hiv_status1  <- sqrt(vcov(logistic1)["hiv_statusPositive", "hiv_statusPositive"])

df_survey1 <- data.frame(
  survey  = "NAMPHIA2017",
  logOR   = coef_hiv_status1,
  seLogOR = se_hiv_status1
)



#--------Kenya 2018-----------
kenphia2018_adult_ind <- read_dta("Kenya 2018 PHIA/kenphia2018adultind.dta")
kenphia2018_adult_bio <- read_dta("Kenya 2018 PHIA/kenphia2018adultbio.dta")

# Merge the Kenya datasets on householdid and personid
merged_kenya <- kenphia2018_adult_ind %>%
  full_join(kenphia2018_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_kenya <- merged_kenya %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# create exchange_sex variable
merged_kenya <- merged_kenya %>%
  mutate(
    exchange_sex = case_when(
      # Yes conditions for each partlastsup variable
      (partlastsup1 == 1 & (partlastsupwhat_b1 == "B" | partlastsupwhat_x1 == "X" | partlastsupwhat_c1 == "C" | partlastsupwhat_f1 == "F")) |
        (partlastsup2 == 1 & (partlastsupwhat_b2 == "B" | partlastsupwhat_x2 == "X" | partlastsupwhat_c2 == "C" | partlastsupwhat_f2 == "F")) |
        (partlastsup3 == 1 & (partlastsupwhat_b3 == "B" | partlastsupwhat_x3 == "X" | partlastsupwhat_c3 == "C" | partlastsupwhat_f3 == "F")) ~ 1,
      
      # No conditions for each partlastsup variable
      (partlastsup1 == 1 & partlastsupwhat_a1 == "A") |
        (partlastsup2 == 1 & partlastsupwhat_a2 == "A") |
        (partlastsup3 == 1 & partlastsupwhat_a3 == "A") ~ 0,
      
      # Refused cases
      partlastsup1 == -9 | partlastsup2 == -9 | partlastsup3 == -9 ~ 98,
      
      # NA if none of the above apply
      TRUE ~ NA_integer_
    )
  )

# Selecting and renaming variables
kenphia2018 <- merged_kenya %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province = county,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year = surveystyear,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schlhi,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    exchange_sex,                     # Had sex in return for gifts, cash or other favours in the last 12 months
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    ever_heard = adlthrdhiv,          # Ever heard of HIV
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
    ) 


# Calculating the median year of interview
median(kenphia2018$year, na.rm = TRUE) # median interview year 2018

# Adding columns for country and survey ID
kenphia2018 <- kenphia2018 %>%
  mutate(
    country = "Kenya",
    survey_id = "KEN2018PHIA",
    med_year = 2018
  )

# Reordering columns to make country and survey_id the leftmost columns
kenphia2018 <- kenphia2018 %>%
  select(country, survey_id, med_year, everything())

# Recoding region
kenphia2018 <- kenphia2018 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
kenphia2018 <- kenphia2018 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
kenphia2018 <- kenphia2018 %>%
  select(country:age, agegrp, everything())

# recode ART for analysis
table(kenphia2018$curr_art)
kenphia2018 <- kenphia2018 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0 # missing ART
  ))

# Recoding sex
kenphia2018 <- kenphia2018 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
kenphia2018 <- kenphia2018 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
kenphia2018 <- kenphia2018 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 1:9 ~ 1,                            # Primary
      schl_years %in% 10:18 ~ 2,                          # Secondary
      schl_years %in% 19:22 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# Recode the 'total_partners' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values as they are
    )
  )

# Recoding the 'ever_heard' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    ever_heard = case_when(
      ever_heard == 1 ~ 1,            # Yes
      ever_heard == 2 ~ 0,            # No
      ever_heard == -9 ~ 98,          # Refused
      ever_heard == -8 ~ 88,         # Don't know
      TRUE ~ NA_integer_              # missing
    )
  )

# Recode the 'hiv_status' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# Recode the 'hivst_use' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ 0           # Missing or any other values to 0
    )
  )

# Recode the 'ever_tested' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
kenphia2018 <- kenphia2018 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
kenphia2018 <- kenphia2018 %>%
  mutate( reference_date = make_date(2018, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
kenphia2018 <- kenphia2018 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )


# filtering for not on art  and have info and hiv status and hivst use
table(kenphia2018$hiv_status, useNA = "ifany")
kenphia2018 <- kenphia2018 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(kenphia2018$curr_art, useNA = "ifany")
kenphia2018 <- kenphia2018 %>%
  filter(curr_art %in% 0)

#--converting into factor and doing regression----
# hivst use
str(kenphia2018$hivst_use)
kenphia2018$hivst_use <- factor(kenphia2018$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(kenphia2018$hiv_status)
kenphia2018$hiv_status <- factor(kenphia2018$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(kenphia2018$sex)
kenphia2018$sex <- factor(kenphia2018$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(kenphia2018$agegrp)
table(kenphia2018$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
kenphia2018$agegrp <- factor(
  kenphia2018$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(kenphia2018$region)
str(kenphia2018$region)
kenphia2018$region <- factor(kenphia2018$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(kenphia2018$wealth_index)
str(kenphia2018$wealth_index)

kenphia2018$wealth_index <- factor(
  kenphia2018$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(kenphia2018$schl_years)
table(kenphia2018$schl_years)
kenphia2018$schl_years <- factor(kenphia2018$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(kenphia2018$survey_id)
str(kenphia2018$survey_id)
kenphia2018$survey_id <- factor(kenphia2018$survey_id)

# logistic reg for meta analysis (male and female seperately)

kenphia2018f <- kenphia2018 %>% filter(sex == "Female")  # Female
kenphia2018m <- kenphia2018 %>% filter(sex == "Male")  # Male

# checking
table(kenphia2018$sex)
table(kenphia2018f$sex)
table(kenphia2018m$sex)


# logistic reg for meta analysis (Female)
logistic2f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                 data = kenphia2018f, family = "binomial")
summary(logistic2f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status2f <- coef(logistic2f)["hiv_statusPositive"]
se_hiv_status2f  <- sqrt(vcov(logistic2f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey2f <- data.frame(
  survey  = "KENPHIA2018F",
  logOR   = coef_hiv_status2f,
  seLogOR = se_hiv_status2f
)

# logistic reg for meta analysis (male)
logistic2m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = kenphia2018m, family = "binomial")
summary(logistic2m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status2m <- coef(logistic2m)["hiv_statusPositive"]
se_hiv_status2m  <- sqrt(vcov(logistic2m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey2m <- data.frame(
  survey  = "KENPHIA2018M",
  logOR   = coef_hiv_status2m,
  seLogOR = se_hiv_status2m
)


#----------Lesotho 2020------

lephia2020_adult_ind <- read_dta("Lesotho 2020 PHIA/lephia2020adultind.dta")
lephia2020_adult_bio <- read_dta("Lesotho 2020 PHIA/lephia2020adultbio.dta")

# Merge the Lesotho datasets on householdid and personid
merged_lesotho <- lephia2020_adult_ind %>%
  full_join(lephia2020_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_lesotho <- merged_lesotho %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# Selecting and renaming variables
lsophia2020 <- merged_lesotho %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province = district,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year,                             # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schcom,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    sellsx12mo_ls,                    # sold sex in return for gifts, cash or other favours in the last 12 months
    buysx12mo_ls,                     #bought sex in return for gifts, cash or other favours in the last 12 months
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal, # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
)

# recode ART for analysis
table(lsophia2020$curr_art)
lsophia2020 <- lsophia2020 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0  # missing ART
  ))

# Calculating the median year of interview
median(lsophia2020$year, na.rm = TRUE) # median interview year 2020


# Adding columns for country and survey ID
lsophia2020 <- lsophia2020 %>%
  mutate(
    country = "Lesotho",
    survey_id = "LSO2020PHIA",
    med_year = 2020
  )

# Reordering columns to make country and survey_id the leftmost columns
lsophia2020 <- lsophia2020 %>%
  select(country, survey_id, med_year, everything())

# Recoding region
lsophia2020 <- lsophia2020 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
lsophia2020 <- lsophia2020 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
lsophia2020 <- lsophia2020 %>%
  select(country:age, agegrp, everything())

# Recoding sex
lsophia2020 <- lsophia2020 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
lsophia2020 <- lsophia2020 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
lsophia2020 <- lsophia2020 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 1:2 ~ 1,                            # Primary
      schl_years %in% 2:4 ~ 2,                          # Secondary
      schl_years %in% 5:7 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# creating exchange_sex variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    exchange_sex = case_when(
      # If either is 1 (Yes), or both are 1
      sellsx12mo_ls == 1 | buysx12mo_ls == 1 ~ 1,            # Yes
      
      # If both are 2 (No)
      sellsx12mo_ls == 2 & buysx12mo_ls == 2 ~ 0,            # No
      
      # If either is Don't know (-8) and the other is not Yes
      (sellsx12mo_ls == -8 | buysx12mo_ls == -8) & !(sellsx12mo_ls == 1 | buysx12mo_ls == 1) ~ 88,  # Don't know
      
      # If either is Refused (-9) and the other is not Yes
      (sellsx12mo_ls == -9 | buysx12mo_ls == -9) & !(sellsx12mo_ls == 1 | buysx12mo_ls == 1) ~ 98,  # Refused
      
      # If both are missing or any combination where the result should be NA
      is.na(sellsx12mo_ls) & is.na(buysx12mo_ls) ~ NA_integer_,  # Missing
      
      # Default case if none of the above apply
      TRUE ~ NA_integer_
    )
  )


# recoding partners in last 12m
lsophia2020 <- lsophia2020 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values unchanged
    )
  )


# Recode the 'hiv_status' variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# Recode the 'hivst_use' variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ NA_integer_            # Missing or any other values to NA
    )
  )

# Recode the 'ever_tested' variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
lsophia2020 <- lsophia2020 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
lsophia2020 <- lsophia2020 %>%
  mutate( reference_date = make_date(2020, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
lsophia2020 <- lsophia2020 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )

# filtering for not on art  and have info and hiv status and hivst use
table(lsophia2020$hiv_status, useNA = "ifany")
lsophia2020 <- lsophia2020 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(lsophia2020$curr_art, useNA = "ifany")
lsophia2020 <- lsophia2020 %>%
  filter(curr_art %in% 0)

#--converting into factor and doing regression----
# hivst use
str(lsophia2020$hivst_use)
lsophia2020$hivst_use <- factor(lsophia2020$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(lsophia2020$hiv_status)
lsophia2020$hiv_status <- factor(lsophia2020$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(lsophia2020$sex)
lsophia2020$sex <- factor(lsophia2020$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(lsophia2020$agegrp)
table(lsophia2020$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
lsophia2020$agegrp <- factor(
  lsophia2020$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(lsophia2020$region)
str(lsophia2020$region)
lsophia2020$region <- factor(lsophia2020$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(lsophia2020$wealth_index)
str(lsophia2020$wealth_index)

lsophia2020$wealth_index <- factor(
  lsophia2020$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(lsophia2020$schl_years)
table(lsophia2020$schl_years)
lsophia2020$schl_years <- factor(lsophia2020$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(lsophia2020$survey_id)
str(lsophia2020$survey_id)
lsophia2020$survey_id <- factor(lsophia2020$survey_id)


# logistic reg for meta analysis (male and female seperately)

lsophia2020f <- lsophia2020 %>% filter(sex == "Female")  # Female
lsophia2020m <- lsophia2020 %>% filter(sex == "Male")  # Male

# checking
table(lsophia2020$sex)
table(lsophia2020f$sex)
table(lsophia2020m$sex)


# logistic reg for meta analysis (Female)
logistic3f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = lsophia2020f, family = "binomial")
summary(logistic3f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status3f <- coef(logistic3f)["hiv_statusPositive"]
se_hiv_status3f  <- sqrt(vcov(logistic3f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey3f <- data.frame(
  survey  = "LSOPHIA2020F",
  logOR   = coef_hiv_status3f,
  seLogOR = se_hiv_status3f
)


# logistic reg for meta analysis (male)
logistic3m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = lsophia2020m, family = "binomial")
summary(logistic3m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status3m <- coef(logistic3m)["hiv_statusPositive"]
se_hiv_status3m  <- sqrt(vcov(logistic3m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey3m <- data.frame(
  survey  = "LSOPHIA2020M",
  logOR   = coef_hiv_status3m,
  seLogOR = se_hiv_status3m
)


# logistic reg for meta analysis
logistic3 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = lsophia2020, family = "binomial")
summary(logistic3)

# extracting estimate & SE
coef_hiv_status3 <- coef(logistic3)["hiv_statusPositive"]
se_hiv_status3   <- sqrt(vcov(logistic3)["hiv_statusPositive", "hiv_statusPositive"])

df_survey3 <- data.frame(
  survey  = "LSOPHIA2020",
  logOR   = coef_hiv_status3,
  seLogOR = se_hiv_status3
)



#----------Zimbabwe 2020-----------

zimphia2020_adult_ind <- read_dta("Zimbabwe 2020 PHIA/zimphia2020adultind.dta")
zimphia2020_adult_bio <- read_dta("Zimbabwe 2020 PHIA/zimphia2020adultbio.dta")

# Merge the Zimbabwe datasets on householdid and personid
merged_zimbabwe <- zimphia2020_adult_ind %>%
  full_join(zimphia2020_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_zimbabwe <- merged_zimbabwe %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# Selecting and renaming variables
zwephia2020 <- merged_zimbabwe %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schcom_zw,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
  )

# recode ART for analysis
table(zwephia2020$curr_art)
zwephia2020 <- zwephia2020 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0  # missing ART
  ))

# median year of interview
median(zwephia2020$year, na.rm = TRUE) #2020

# Adding columns for country and survey ID
zwephia2020 <- zwephia2020 %>%
  mutate(
    country = "zimbabwe",
    survey_id = "ZWE2020PHIA",
    med_year = 2020
  )

# Reordering columns to make country and survey_id the leftmost columns
zwephia2020 <- zwephia2020 %>%
  select(country, survey_id, med_year, everything())

# Recoding region
zwephia2020 <- zwephia2020 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
zwephia2020 <- zwephia2020 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
zwephia2020 <- zwephia2020 %>%
  select(country:age, agegrp, everything())

# Recoding sex
zwephia2020 <- zwephia2020 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
zwephia2020 <- zwephia2020 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated/Single
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
zwephia2020 <- zwephia2020 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 3:9 ~ 1,                            # Primary
      schl_years %in% 10:15 ~ 2,                          # Secondary
      schl_years %in% 16:18 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# recoding partners in last 12m
zwephia2020 <- zwephia2020 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values unchanged
    )
  )


# Recode the 'hiv_status' variable
zwephia2020 <- zwephia2020 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# Recode the 'hivst_use' variable
zwephia2020 <- zwephia2020 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ NA_integer_            # Missing or any other values to NA
    )
  )

# Recode the 'ever_tested' variable
zwephia2020 <- zwephia2020 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
zwephia2020 <- zwephia2020 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
zwephia2020 <- zwephia2020 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
zwephia2020 <- zwephia2020 %>%
  mutate( reference_date = make_date(2020, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
zwephia2020 <- zwephia2020 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )



# filtering for not on art  and have info and hiv status and hivst use
table(zwephia2020$hiv_status, useNA = "ifany")
zwephia2020 <- zwephia2020 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(zwephia2020$curr_art, useNA = "ifany")
zwephia2020 <- zwephia2020 %>%
  filter(curr_art %in% 0)

#--converting into factor and doing regression----
# hivst use
str(zwephia2020$hivst_use)
zwephia2020$hivst_use <- factor(zwephia2020$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(zwephia2020$hiv_status)
zwephia2020$hiv_status <- factor(zwephia2020$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(zwephia2020$sex)
zwephia2020$sex <- factor(zwephia2020$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(zwephia2020$agegrp)
table(zwephia2020$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
zwephia2020$agegrp <- factor(
  zwephia2020$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(zwephia2020$region)
str(zwephia2020$region)
zwephia2020$region <- factor(zwephia2020$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(zwephia2020$wealth_index)
str(zwephia2020$wealth_index)

zwephia2020$wealth_index <- factor(
  zwephia2020$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(zwephia2020$schl_years)
table(zwephia2020$schl_years)
zwephia2020$schl_years <- factor(zwephia2020$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(zwephia2020$survey_id)
str(zwephia2020$survey_id)
zwephia2020$survey_id <- factor(zwephia2020$survey_id)


# logistic reg for meta analysis (male and female seperately)
zwephia2020f <- zwephia2020 %>% filter(sex == "Female")  # Female
zwephia2020m <- zwephia2020 %>% filter(sex == "Male")  # Male

# checking
table(zwephia2020$sex)
table(zwephia2020f$sex)
table(zwephia2020m$sex)

# logistic reg for meta analysis (Female)
logistic4f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = zwephia2020f, family = "binomial")
summary(logistic4f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status4f <- coef(logistic4f)["hiv_statusPositive"]
se_hiv_status4f  <- sqrt(vcov(logistic4f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey4f <- data.frame(
  survey  = "ZWEPHIA2020F",
  logOR   = coef_hiv_status4f,
  seLogOR = se_hiv_status4f
)


# logistic reg for meta analysis (male)
logistic4m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = zwephia2020m, family = "binomial")
summary(logistic4m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status4m <- coef(logistic4m)["hiv_statusPositive"]
se_hiv_status4m  <- sqrt(vcov(logistic4m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey4m <- data.frame(
  survey  = "ZWEPHIA2020M",
  logOR   = coef_hiv_status4m,
  seLogOR = se_hiv_status4m
)

# logistic reg for meta analysis
logistic4 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = zwephia2020, family = "binomial")
summary(logistic4)

# extracting estimate & SE
coef_hiv_status4 <- coef(logistic4)["hiv_statusPositive"]
se_hiv_status4   <- sqrt(vcov(logistic4)["hiv_statusPositive", "hiv_statusPositive"])

df_survey4 <- data.frame(
  survey  = "ZWEPHIA2020",
  logOR   = coef_hiv_status4,
  seLogOR = se_hiv_status4
)



#----------Malawi 2020-------------

mphia2020_adult_ind <- read_dta("Malawi 2020-2021 PHIA/mphia2020adultind.dta")
mphia2020_adult_bio <- read_dta("Malawi 2020-2021 PHIA/mphia2020adultbio.dta")

# Merge the Malawi datasets on householdid and personid
merged_malawi <- mphia2020_adult_ind %>%
  full_join(mphia2020_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_malawi <- merged_malawi %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# Selecting and renaming variables
mwiphia2020 <- merged_malawi %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province = zone,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schcom_mw,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_knwldge = hivselftsthear,   # Knowledge of hiv self-test kit
    hivst_use = hivselftst,           # Used HIV self-test kit
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
  )

# recode ART for analysis
table(mwiphia2020$curr_art)
mwiphia2020 <- mwiphia2020 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0 # missing ART
  )
  )

#median y of interview
median(mwiphia2020$year, na.rm = TRUE) # 2020

# Adding columns for country and survey ID
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    country = "malawi",
    survey_id = "MWI2020PHIA",
    med_year = 2020
  )

# Reordering columns to make country and survey_id the leftmost columns
mwiphia2020 <- mwiphia2020 %>%
  select(country, survey_id, everything())

# Recoding region
mwiphia2020 <- mwiphia2020 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
mwiphia2020 <- mwiphia2020 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
mwiphia2020 <- mwiphia2020 %>%
  select(country:age, agegrp, everything())

# Recoding sex
mwiphia2020 <- mwiphia2020 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
  ))

# Recoding current marital status
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated/Single
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 1:2 ~ 1,                            # Primary
      schl_years %in% 3:4 ~ 2,                          # Secondary
      schl_years %in% 5 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# recoding partners in last 12m
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values unchanged
    )
  )


# Recode the 'hiv_status' variable
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# recoding hivst_knwldge
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    hivst_knwldge = case_when(
      hivst_knwldge == 1 ~ 1,    # Yes
      hivst_knwldge == 2 ~ 0,    # No
      hivst_knwldge == -9 ~ 98,  # Refused
      hivst_knwldge == -8 ~ 88,  # Don't know
      is.na(hivst_knwldge) ~ 0
    )
  )

# Recode the 'hivst_use' variable
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    hivst_use = case_when(
      hivst_knwldge == 1 & hivst_use == 1 ~ 1,           # Yes
      hivst_knwldge == 0 & hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 0,         # Don't Know
      hivst_use == -9 ~ 0,         # Refused
      TRUE ~ 0 
    )
  )

# Recode the 'ever_tested' variable
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )


# Set the reference date to end of the survey year
mwiphia2020 <- mwiphia2020 %>%
  mutate( reference_date = make_date(2020, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
mwiphia2020 <- mwiphia2020 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )

# filtering for not on art  and have info and hiv status and hivst use
table(mwiphia2020$hiv_status, useNA = "ifany")
mwiphia2020 <- mwiphia2020 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(mwiphia2020$curr_art, useNA = "ifany")
mwiphia2020 <- mwiphia2020 %>%
  filter(curr_art %in% 0)


#--converting into factor and doing regression----
# hivst use
str(mwiphia2020$hivst_use)
mwiphia2020$hivst_use <- factor(mwiphia2020$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(mwiphia2020$hiv_status)
mwiphia2020$hiv_status <- factor(mwiphia2020$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(mwiphia2020$sex)
mwiphia2020$sex <- factor(mwiphia2020$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(mwiphia2020$agegrp)
table(mwiphia2020$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
mwiphia2020$agegrp <- factor(
  mwiphia2020$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(mwiphia2020$region)
str(mwiphia2020$region)
mwiphia2020$region <- factor(mwiphia2020$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(mwiphia2020$wealth_index)
str(mwiphia2020$wealth_index)

mwiphia2020$wealth_index <- factor(
  mwiphia2020$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(mwiphia2020$schl_years)
table(mwiphia2020$schl_years)
mwiphia2020$schl_years <- factor(mwiphia2020$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(mwiphia2020$survey_id)
str(mwiphia2020$survey_id)
mwiphia2020$survey_id <- factor(mwiphia2020$survey_id)

# logistic reg for meta analysis (male and female seperately)
mwiphia2020f <- mwiphia2020 %>% filter(sex == "Female")  # Female
mwiphia2020m <- mwiphia2020 %>% filter(sex == "Male")  # Male

# checking
table(mwiphia2020$sex)
table(mwiphia2020f$sex)
table(mwiphia2020m$sex)

# logistic reg for meta analysis (Female)
logistic5f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = mwiphia2020f, family = "binomial")
summary(logistic5f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status5f <- coef(logistic5f)["hiv_statusPositive"]
se_hiv_status5f  <- sqrt(vcov(logistic5f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey5f <- data.frame(
  survey  = "MWIPHIA2020F",
  logOR   = coef_hiv_status5f,
  seLogOR = se_hiv_status5f
)


# logistic reg for meta analysis (male)
logistic5m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = mwiphia2020m, family = "binomial")
summary(logistic5m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status5m <- coef(logistic5m)["hiv_statusPositive"]
se_hiv_status5m  <- sqrt(vcov(logistic5m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey5m <- data.frame(
  survey  = "MWIPHIA2020M",
  logOR   = coef_hiv_status5m,
  seLogOR = se_hiv_status5m
)


# logistic reg for meta analysis
logistic5 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = mwiphia2020, family = "binomial")
summary(logistic5)

# extracting estimate & SE
coef_hiv_status5 <- coef(logistic5)["hiv_statusPositive"]
se_hiv_status5   <- sqrt(vcov(logistic5)["hiv_statusPositive", "hiv_statusPositive"])

df_survey5 <- data.frame(
  survey  = "MWIPHIA2020",
  logOR   = coef_hiv_status5,
  seLogOR = se_hiv_status5
)




#----------Mozambique 2021------------
insida2021_adult_ind <- read_dta("Mozambique 2021 PHIA/insida2021adultind.dta")
insida2021_adult_bio <- read_dta("Mozambique 2021 PHIA/insida2021adultbio.dta")

# Merge the Mozambique datasets on householdid and personid
merged_mozambique <- insida2021_adult_ind %>%
  full_join(insida2021_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_mozambique <- merged_mozambique %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# Selecting and renaming variables
mozphia2021 <- merged_mozambique %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province,                         # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schcom_mz,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
  )

# recode ART for analysis
table(mozphia2021$curr_art)
mozphia2021 <- mozphia2021 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0  # missing ART
  )
  )

# med y 
median(mozphia2021$year, na.rm = TRUE) # 2021

# Adding columns for country and survey ID
mozphia2021 <- mozphia2021 %>%
  mutate(
    country = "mozambique",
    survey_id = "MOZ2021PHIA",
    med_year = 2021
  )

# Reordering columns to make country and survey_id the leftmost columns
mozphia2021 <- mozphia2021 %>%
  select(country, survey_id, everything())

# Recoding region
mozphia2021 <- mozphia2021 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
mozphia2021 <- mozphia2021 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
mozphia2021 <- mozphia2021 %>%
  select(country:age, agegrp, everything())

# Recoding sex
mozphia2021 <- mozphia2021 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
mozphia2021 <- mozphia2021 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated/Single
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
mozphia2021 <- mozphia2021 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 1:3 ~ 1,                            # Primary
      schl_years %in% 4:8 ~ 2,                          # Secondary
      schl_years %in% 9:10 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# recoding partners in last 12m
mozphia2021 <- mozphia2021 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values unchanged
    )
  )


# Recode the 'hiv_status' variable
mozphia2021 <- mozphia2021 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# Recode the 'hivst_use' variable
mozphia2021 <- mozphia2021 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ NA_integer_            # Missing or any other values to NA
    )
  )

# Recode the 'ever_tested' variable
mozphia2021 <- mozphia2021 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
mozphia2021 <- mozphia2021 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
mozphia2021 <- mozphia2021 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
mozphia2021 <- mozphia2021 %>%
  mutate( reference_date = make_date(2020, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
mozphia2021 <- mozphia2021 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )

# filtering hiv results 0and 1 and exclude art=1
table(mozphia2021$hiv_status, useNA = "ifany")
mozphia2021 <- mozphia2021 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(mozphia2021$curr_art, useNA = "ifany")
mozphia2021 <- mozphia2021 %>%
  filter(curr_art %in% 0)

#--converting into factor and doing regression----
# hivst use
str(mozphia2021$hivst_use)
mozphia2021$hivst_use <- factor(mozphia2021$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(mozphia2021$hiv_status)
mozphia2021$hiv_status <- factor(mozphia2021$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(mozphia2021$sex)
mozphia2021$sex <- factor(mozphia2021$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(mozphia2021$agegrp)
table(mozphia2021$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
mozphia2021$agegrp <- factor(
  mozphia2021$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(mozphia2021$region)
str(mozphia2021$region)
mozphia2021$region <- factor(mozphia2021$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(mozphia2021$wealth_index)
str(mozphia2021$wealth_index)

mozphia2021$wealth_index <- factor(
  mozphia2021$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(mozphia2021$schl_years)
table(mozphia2021$schl_years)
mozphia2021$schl_years <- factor(mozphia2021$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(mozphia2021$survey_id)
str(mozphia2021$survey_id)
mozphia2021$survey_id <- factor(mozphia2021$survey_id)

# logistic reg for meta analysis (male and female seperately)
mozphia2021f <- mozphia2021 %>% filter(sex == "Female")  # Female
mozphia2021m <- mozphia2021 %>% filter(sex == "Male")  # Male

# checking
table(mozphia2021$sex)
table(mozphia2021f$sex)
table(mozphia2021m$sex)

# logistic reg for meta analysis (Female)
logistic6f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = mozphia2021f, family = "binomial")
summary(logistic6f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status6f <- coef(logistic6f)["hiv_statusPositive"]
se_hiv_status6f  <- sqrt(vcov(logistic6f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey6f <- data.frame(
  survey  = "MOZPHIA2021F",
  logOR   = coef_hiv_status6f,
  seLogOR = se_hiv_status6f
)

# logistic reg for meta analysis (male)
logistic6m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = mozphia2021m, family = "binomial")
summary(logistic6m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status6m <- coef(logistic6m)["hiv_statusPositive"]
se_hiv_status6m  <- sqrt(vcov(logistic6m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey6m <- data.frame(
  survey  = "MOZPHIA2021F",
  logOR   = coef_hiv_status6m,
  seLogOR = se_hiv_status6m
)


# logistic reg for meta analysis
logistic6 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = mozphia2021, family = "binomial")
summary(logistic6)

# extracting estimate & SE
coef_hiv_status6 <- coef(logistic6)["hiv_statusPositive"]
se_hiv_status6   <- sqrt(vcov(logistic6)["hiv_statusPositive", "hiv_statusPositive"])

df_survey6 <- data.frame(
  survey  = "MOZPHIA2021",
  logOR   = coef_hiv_status6,
  seLogOR = se_hiv_status6
)




#-------------Eswatini 2021-----------
shims3_2021_adult_ind <- read_dta("Eswatini 2021 PHIA/shims32021adultind.dta")
shims3_2021_adult_bio <- read_dta("Eswatini 2021 PHIA/shims32021adultbio.dta")

# Merge the Eswatini datasets on householdid and personid
merged_eswatini <- shims3_2021_adult_ind %>%
  full_join(shims3_2021_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# Removing redundant '.bio' columns if they are identical
merged_eswatini <- merged_eswatini %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

# Selecting and renaming variables
swzphia2021 <- merged_eswatini %>%
  select(
    psu = varunit,                    # PSU/Cluster
    strata = varstrat,                # Strata
    province = region,                # Area/Province
    region = urban,                   # Urban/ Rural Area
    ind_wt = intwt0,                  # Individual weight
    year,              # Survey year
    hhid = householdid,               # Household ID
    age = age,                        # Age of the respondent (no renaming needed)
    sex = gender,                     # Sex of the respondent
    curr_marital = curmar,            # Current marital status
    schl_years = schcom_sz,              # Highest level of schooling
    wealth_index = wealthquintile,    # Wealth index quintile
    total_partners = part12monum,     # Number of sex partners in the last 12 months
    hiv_wt = btwt0,                   # HIV weight
    hiv_status = hivstatusfinal,      # Final HIV status
    hivst_use = hivselftst,           # Used HIV self-test kit
    confirmatory_testing = hivselftstconfirm_sz, # confirmatory testing at nearest facility after hivst
    ever_tested = hivtstever,         # Ever tested for HIV
    last_hivresult = hivtstrslt,      # Result of the last HIV test
    last_hivtest_month = hivtestm,   # which month last tested for HIV 
    last_hivtest_year = hivtesty,    # which month last tested for HIV
    curr_art = art
  )

# recode ART for analysis
table(swzphia2021$curr_art)
swzphia2021 <- swzphia2021 %>%
  mutate(curr_art = case_when(
    curr_art == 1 ~ 1,   # on ART
    curr_art == 2 ~ 0,   # not on ART
    curr_art == 99 ~ 0  # not on ART
  )
  )

# med y
median(swzphia2021$year, na.rm = TRUE) #2021

# Adding columns for country and survey ID
swzphia2021 <- swzphia2021 %>%
  mutate(
    country = "eswatini",
    survey_id = "SWZ2021PHIA",
    med_year = 2021
  )

# Reordering columns to make country and survey_id the leftmost columns
swzphia2021 <- swzphia2021 %>%
  select(country, survey_id, med_year, everything())

# Recoding region
swzphia2021 <- swzphia2021 %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age into 5-year age groups
swzphia2021 <- swzphia2021 %>%
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
    TRUE                ~ NA_real_  # Handles missing values as NA
  ))

# Moving the agegrp column to the right of the age column
swzphia2021 <- swzphia2021 %>%
  select(country:age, agegrp, everything())

# Recoding sex
swzphia2021 <- swzphia2021 %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))

# Recoding current marital status
swzphia2021 <- swzphia2021 %>%
  mutate(
    curr_marital = case_when(
      curr_marital == 1 | curr_marital == 2 ~ 5,   # Married/Living together
      curr_marital == 3 ~ 2,                      # Widowed
      curr_marital == 4 ~ 3,                      # Divorced
      curr_marital == 5 ~ 4,                      # Separated/Single
      curr_marital == -8 ~ 88,                    # Don't Know
      curr_marital == -9 ~ 98,                    # Refused
      is.na(curr_marital) ~ as.integer(NA),       # Handle missing values
    )
  )

# recoding schooling
swzphia2021 <- swzphia2021 %>%
  mutate(
    schl_years = case_when(
      schl_years %in% 2:8 ~ 1,                            # Primary
      schl_years %in% 9:14 ~ 2,                           # Secondary
      schl_years %in% 15:17 ~ 3,                          # Tertiary
      schl_years == -8 ~ 88,                              # Don't know
      schl_years == -9 ~ 98,                              # Refused
      TRUE ~ NA_integer_                                  # Missing
    )
  )

# recoding partners in last 12m
swzphia2021 <- swzphia2021 %>%
  mutate(
    total_partners = case_when(
      total_partners == -7 ~ NA_integer_,  # Recode -7 to NA
      total_partners == -8 ~ 88,           # Recode -8 to 88 (Don't know)
      total_partners == -9 ~ 98,           # Recode -9 to 98 (Refused)
      TRUE ~ total_partners                # Keep all other values unchanged
    )
  )


# Recode the 'hiv_status' variable
swzphia2021 <- swzphia2021 %>%
  mutate(
    hiv_status = case_when(
      hiv_status == 1 ~ 1,           # Positive
      hiv_status == 2 ~ 0,           # Negative
      TRUE ~ NA_integer_             # Missing 
    )
  )

# Recode the 'hivst_use' variable
swzphia2021 <- swzphia2021 %>%
  mutate(
    hivst_use = case_when(
      hivst_use == 1 ~ 1,           # Yes
      hivst_use == 2 ~ 0,           # No
      hivst_use == -8 ~ 88,         # Don't Know
      hivst_use == -9 ~ 98,         # Refused
      TRUE ~ NA_integer_            # Missing or any other values to NA
    )
  )

# recoding confirmatory testing after self-test
swzphia2021 <- swzphia2021 %>%
  mutate(
    confirmatory_testing = case_when(
      confirmatory_testing == 1 ~ 1,    # Yes
      confirmatory_testing == 2 ~ 0,    # No
      confirmatory_testing == -9 ~ 98,  # Refused
      confirmatory_testing == -8 ~ 88,  # Don't know
      TRUE ~ NA_integer_                # Missing
    )
  )

# Recode the 'ever_tested' variable
swzphia2021 <- swzphia2021 %>%
  mutate(
    ever_tested = case_when(
      ever_tested == 1 ~ 1,           # Yes
      ever_tested == 2 ~ 0,           # No
      ever_tested == -9 ~ 98,         # Refused
      ever_tested == -8 ~ 88,         # Don't Know
      TRUE ~ NA_integer_              # Missing or any other values to NA
    )
  )

# Recode the 'last_hivresult' variable
swzphia2021 <- swzphia2021 %>%
  mutate(
    last_hivresult = case_when(
      last_hivresult == 1 ~ 1,            # Positive
      last_hivresult == 2 ~ 0,            # Negative
      last_hivresult == 3 ~ 3,            # Indeterminate
      last_hivresult == 4 ~ 4,            # Did Not Receive Result
      last_hivresult == -8 ~ 88,          # Don't Know
      last_hivresult == -9 ~ 98,          # Refused
      TRUE ~ NA_integer_                  # Missing 
    )
  )

# Create a date variable from last_hivtest_year and last_hivtest_month
swzphia2021 <- swzphia2021 %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

# Set the reference date to end of the survey year
swzphia2021 <- swzphia2021 %>%
  mutate( reference_date = make_date(2020, 12, 31),
          # Calculate the difference in months between the last HIV test date and the reference date
          diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )

# creating last hiv test variable
swzphia2021 <- swzphia2021 %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )


# filtering for not on art  and have info and hiv status and hivst use
table(swzphia2021$hiv_status, useNA = "ifany")
swzphia2021 <- swzphia2021 %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(swzphia2021$curr_art, useNA = "ifany")
swzphia2021 <- swzphia2021 %>%
  filter(curr_art %in% 0)

#--converting into factor and doing regression----
# hivst use
str(swzphia2021$hivst_use)
swzphia2021$hivst_use <- factor(swzphia2021$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(swzphia2021$hiv_status)
swzphia2021$hiv_status <- factor(swzphia2021$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(swzphia2021$sex)
swzphia2021$sex <- factor(swzphia2021$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(swzphia2021$agegrp)
table(swzphia2021$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
swzphia2021$agegrp <- factor(
  swzphia2021$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(swzphia2021$region)
str(swzphia2021$region)
swzphia2021$region <- factor(swzphia2021$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(swzphia2021$wealth_index)
str(swzphia2021$wealth_index)

swzphia2021$wealth_index <- factor(
  swzphia2021$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(swzphia2021$schl_years)
table(swzphia2021$schl_years)
swzphia2021$schl_years <- factor(swzphia2021$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(swzphia2021$survey_id)
str(swzphia2021$survey_id)
swzphia2021$survey_id <- factor(swzphia2021$survey_id)


# logistic reg for meta analysis (male and female seperately)
swzphia2021f <- swzphia2021 %>% filter(sex == "Female")  # Female
swzphia2021m <- swzphia2021 %>% filter(sex == "Male")  # Male

# checking
table(swzphia2021$sex)
table(swzphia2021f$sex)
table(swzphia2021m$sex)

# logistic reg for meta analysis (Female)
logistic7f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = swzphia2021f, family = "binomial")
summary(logistic7f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status7f <- coef(logistic7f)["hiv_statusPositive"]
se_hiv_status7f  <- sqrt(vcov(logistic7f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey7f <- data.frame(
  survey  = "SWZPHIA2021F",
  logOR   = coef_hiv_status7f,
  seLogOR = se_hiv_status7f
)


# logistic reg for meta analysis (male)
logistic7m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = swzphia2021m, family = "binomial")
summary(logistic7m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status7m <- coef(logistic7m)["hiv_statusPositive"]
se_hiv_status7m <- sqrt(vcov(logistic7m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey7m <- data.frame(
  survey  = "SWZPHIA2021M",
  logOR   = coef_hiv_status7m,
  seLogOR = se_hiv_status7m
)


# logistic reg for meta analysis
logistic7 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = swzphia2021, family = "binomial")
summary(logistic7)

# extracting estimate & SE
coef_hiv_status7 <- coef(logistic7)["hiv_statusPositive"]
se_hiv_status7   <- sqrt(vcov(logistic7)["hiv_statusPositive", "hiv_statusPositive"])

df_survey7 <- data.frame(
  survey  = "SWZPHIA2021",
  logOR   = coef_hiv_status7,
  seLogOR = se_hiv_status7
)



# Combine all
bio_list_phia <- list(namphia2017, kenphia2018, lsophia2020, 
                  zwephia2020, mwiphia2020, mozphia2021, swzphia2021)


saveRDS(bio_list_phia, file = "D:/Downloads/MSc Thesis/hivst/surveys with biomarker data/cleaned biomarker surveys/bio_list_phia_art.rds")

#saveRDS(bio_list_phia, file = "D:/Downloads/MSc Thesis/hivst/surveys with biomarker data/cleaned biomarker surveys/bio_list_phia.rds")

