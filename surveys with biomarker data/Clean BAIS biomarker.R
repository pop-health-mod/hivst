# Clearing all objects
rm(list = ls())
gc() 

library(haven)
library(dplyr)
library(lubridate)
library(survey)

setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/BAIS raw data")

# adultind and adultbio need to be merged by hhid and personid (unique identifier)
bais_adult_ind <- read_dta("baisv2021adultind.dta")
bais_adult_bio <- read_dta("baisv2021adultbio.dta")

# Merge the datasets 
merged_bais <- bais_adult_ind %>%
  full_join(bais_adult_bio, by = c("householdid", "personid"), suffix = c(".ind", ".bio"))

# dropping redundant '.bio' columns if identical
merged_bais <- merged_bais %>%
  mutate(across(ends_with(".bio"), ~ifelse(is.na(.), get(sub("\\.bio$", ".ind", cur_column())), .))) %>%
  select(-ends_with(".bio")) %>%
  rename_with(~sub("\\.ind$", "", .), ends_with(".ind"))

#Note: While merging, some observations were not matched because they weren't in the adultbio dataset.

# Selecting and renaming variables
bais <- merged_bais %>%
  select(
    psu = centroidid,         
    strata = varstrat,
    province = hdistrict_bw,
    region = urban,
    ind_wt = intwt0,
    year = year,
    hhid = householdid,
    age = age,
    sex = gender,
    curr_marital = curmar,
    schl_years = schcom,
    wealth_index = wealthquintile,
    total_partners = part12monum,
    hiv_wt = btwt0,
    hiv_status = hivstatusfinal,
    hivst_use = hivselftst,
    ever_tested = hivtstever,
    last_hivtest_month = hivtestm,
    last_hivtest_year = hivtesty,
    last_hivresult = hivtstrslt,
    curr_art = art
  )

# recode ART for analysis
table(bais$curr_art, useNA = "ifany")
bais$curr_art <- as.numeric(bais$curr_art)
bais$curr_art <- ifelse(bais$curr_art == 1, 1, 0)
bais$curr_art[is.na(bais$curr_art)] <- 0

# Adding columns for country and survey ID
bais <- bais %>%
  mutate(country = "Botswana",
         survey_id = "BWA2021BAIS",
         med_year = 2021) # median interview year

# Reordering columns to make country and survey_id the leftmost columns
bais <- bais %>%
  select(country, survey_id, med_year, everything())

# Converting province into character
bais$province <- as.character(as_factor(bais$province))
bais$province <- gsub("\\(.*\\)\\s*", "", bais$province)

# Recoding region
bais <- bais %>%
  mutate(region = case_when(
    region == 1 ~ 1,  # Urban becomes 1
    region == 2 ~ 0,  # Rural becomes 0
    region == 99 ~ NA_real_,  # Missing (99) becomes NA
  ))

# Recoding age
# Creating agegrp variable with 5-year age categories
bais <- bais %>%
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

# Moving the agegrp column to the right of the age column
bais <- bais %>%
  select(country:age, agegrp, everything())

# Recoding sex
bais <- bais %>%
  mutate(sex = case_when(
    sex == 1 ~ 1,   # Male remain 1
    sex == 2 ~ 0,   # Female recoded to 0
    sex == 99 ~ NA_real_  # 99 recoded to NA
  ))


# Recoding current marital status: Note there was no label for single
bais <- bais %>%
  mutate(curr_marital = case_when(
    curr_marital == -9 ~ NA_real_,    # -9 becomes NA
    curr_marital== -8 ~ 88,          # Don't know becomes 88
    curr_marital %in% c(1, 2) ~ 5,    # Married/In a Union (living together) becomes 5
    curr_marital == 3 ~ 2,            # Widowed becomes 2
    curr_marital == 4 ~ 3,            # Divorced becomes 3
    curr_marital == 5 ~ 4,            # Separated becomes 4
    TRUE ~ curr_marital                       
  ))


# Recode the 'schl_years' variable in the bais dataframe
bais <- bais %>%
  mutate(
    schl_years = case_when(
      schl_years == 0 ~ 1,    # Primary/no edu -> 1
      schl_years == 1 ~ 2,    # Secondary -> 2
      schl_years == 2 ~ 3,    # Higher -> 3
      schl_years == -8 ~ 88,  # Don't Know -> 88
      TRUE ~ NA_integer_      # Missing or any other values -> NA
    )
  )

# Recoding the 'total_partners' variable
bais <- bais %>%
  mutate(total_partners = case_when(
    total_partners == -9 ~ NA_real_,       # Refused becomes NA
    total_partners == -8 ~ NA_real_,       # Don't know becomes NA
    TRUE ~ as.numeric(total_partners)      # Keep the existing numeric values, including 6 as 6+
  ))

# Recoding the 'hiv_status' variable
bais <- bais %>%
  mutate(hiv_status = case_when(
    hiv_status == 1 ~ 1,          # HIV positive becomes 1
    hiv_status == 2 ~ 0,          # HIV negative becomes 0
    hiv_status == 99 ~ NA_real_,  # Missing becomes NA
    TRUE ~ as.numeric(hiv_status) # Preserve any other values, though there shouldn't be any
  ))


# Recoding the 'hivst_use' variable
bais <- bais %>%
  mutate(hivst_use = case_when(
    hivst_use == 1 ~ 1,            # Yes becomes 1
    hivst_use == 2 ~ 0,            # No becomes 0
    hivst_use == -9 ~ NA_real_,          # Refused becomes 98
    hivst_use == -8 ~ NA_real_,    # Don't know becomes NA (treated as missing)
  ))


# Recoding the 'ever_tested' variable
bais <- bais %>%
  mutate(ever_tested = case_when(
    ever_tested == 1 ~ 1,            # Yes becomes 1
    ever_tested == 2 ~ 0,            # No becomes 0
    ever_tested == -9 ~ 98,          # Refused becomes 98
    ever_tested == -8 ~ NA_real_,    # Don't know becomes NA (treated as missing)
  ))


# Recode the 'last_hivresult' variable
bais <- bais %>%
  mutate(last_hivresult = case_when(
    last_hivresult == 1 ~ 1,            # Positive becomes 1
    last_hivresult == 2 ~ 0,            # Negative becomes 0
    last_hivresult == 3 ~ 3,            # Unknown/Indeterminate remains 3
    last_hivresult == 4 ~ 4,            # Did Not Receive Result remains 4
    last_hivresult == -8 ~ 88,          # Don't Know becomes 88
    last_hivresult == -9 ~ NA_real_,    # Refused becomes NA (treated as missing)
  ))

# For months ago last hiv test, creating a date variable from last_hivtest_year and last_hivtest_month
bais <- bais %>%
  mutate(
    last_hivtest_date = make_date(last_hivtest_year, last_hivtest_month, 1)
  )

bais <- bais %>%
  mutate(
    # Set the reference date to December 31st, 2021 (end of the survey year)
    reference_date = make_date(2021, 12, 31),
    
    # Calculate the difference in months between the last HIV test date and the reference date
    diff_months = time_length(interval(last_hivtest_date, reference_date), "months")
  )


# creating last hiv test variable
bais <- bais %>%
  mutate(
    last_hivtest = case_when(
      diff_months < 12 ~ 1,                       # Tested <12 months ago
      diff_months >= 12 & diff_months <= 24 ~ 2,  # Tested 1-2 years ago
      diff_months > 24 ~ 3,                       # Tested more than 2 years ago
      TRUE ~ NA_real_                             # For all others, set as missing (NA)
    )
  )

# filtering for not on art  and have info and hiv status and hivst use
table(bais$hiv_status, useNA = "ifany")
bais <- bais %>%
  filter(hiv_status %in% c(0, 1),
         hivst_use %in% c(0, 1))

table(bais$curr_art, useNA = "ifany")
bais <- bais %>%
  filter(curr_art %in% 0)


#--converting into factor and doing regression----
# hivst use
str(bais$hivst_use)
bais$hivst_use <- factor(bais$hivst_use, 
                                levels = c(0, 1),
                                labels = c("No", "Yes"))

# hiv status
str(bais$hiv_status)
bais$hiv_status <- factor(bais$hiv_status,
                                 levels = c(0, 1),
                                 labels = c("Negative", "Positive"))

# sex
str(bais$sex)
bais$sex <- factor(bais$sex, 
                          levels = c(0, 1),
                          labels = c("Female", "Male"))

# age group
str(bais$agegrp)
table(bais$agegrp)
age_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")
age_labels <- c("15-19",  "20-24", "25-29", "30-34", "35-39", 
                "40-44",  "45-49", "50-54", "55-59", "60-64", 
                "65+")
bais$agegrp <- factor(
  bais$agegrp,
  levels = age_levels,    
  labels = age_labels     
)

# region
table(bais$region)
str(bais$region)
bais$region <- factor(bais$region,
                             levels = c("0", "1"),
                             labels = c("Rural", "Urban"))


# wealth index
table(bais$wealth_index)
str(bais$wealth_index)

bais$wealth_index <- factor(
  bais$wealth_index,
  levels = c("1", "2", "3", "4", "5"),
  labels = c("Lowest", "Second", "Middle", "Fourth", "Highest")
)


# edu level
str(bais$schl_years)
table(bais$schl_years)
bais$schl_years <- factor(bais$schl_years,
                                 levels = c("1", "2", "3"),
                                 labels = c("No edu/Primary", "Secondary/Higher Secondary", "Tertiary"))

# survey id
table(bais$survey_id)
str(bais$survey_id)
bais$survey_id <- factor(bais$survey_id)


# logistic reg for meta analysis (male and female seperately)
baisf <- bais %>% filter(sex == "Female")  # Female
baism <- bais %>% filter(sex == "Male")  # Male

# checking
table(bais$sex)
table(baisf$sex)
table(baism$sex)

# logistic reg for meta analysis (Female)
logistic8f <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = baisf, family = "binomial")
summary(logistic8f)

# extracting estimate & SE for RE meta analysis
coef_hiv_status8f <- coef(logistic8f)["hiv_statusPositive"]
se_hiv_status8f  <- sqrt(vcov(logistic8f)["hiv_statusPositive", "hiv_statusPositive"])

df_survey8f <- data.frame(
  survey  = "BWABAIS2021F",
  logOR   = coef_hiv_status8f,
  seLogOR = se_hiv_status8f
)

# logistic reg for meta analysis (male)
logistic8m <- glm(hivst_use ~ hiv_status + region + agegrp +  wealth_index + schl_years, 
                  data = baism, family = "binomial")
summary(logistic8m)

# extracting estimate & SE for RE meta analysis
coef_hiv_status8m <- coef(logistic8m)["hiv_statusPositive"]
se_hiv_status8m <- sqrt(vcov(logistic8m)["hiv_statusPositive", "hiv_statusPositive"])

df_survey8m <- data.frame(
  survey  = "BWABAIS2021M",
  logOR   = coef_hiv_status8m,
  seLogOR = se_hiv_status8m
)



# logistic reg for meta analysis
logistic8 <- glm(hivst_use ~ hiv_status + sex + region + agegrp +  wealth_index + schl_years, 
                 data = bais, family = "binomial")
summary(logistic8)

# extracting estimate & SE
coef_hiv_status8 <- coef(logistic8)["hiv_statusPositive"]
se_hiv_status8   <- sqrt(vcov(logistic8)["hiv_statusPositive", "hiv_statusPositive"])

df_survey8 <- data.frame(
  survey  = "BWABAIS2021",
  logOR   = coef_hiv_status8,
  seLogOR = se_hiv_status8
)




#saveRDS(bais, file = "D:/Downloads/MSc Thesis/hivst/surveys with biomarker data/cleaned biomarker surveys/bio_bais_art.rds")

#saveRDS(bais, file = "D:/Downloads/MSc Thesis/hivst/surveys with biomarker data/cleaned biomarker surveys/bio_bais.rds")


