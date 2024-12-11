library(dplyr)
library(haven)
library(lme4)
library(glmmTMB)

#check zwephia
#---DHS---
list_dhs_cleaned <- lapply(list_dhs, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # Convert labelled columns to character
})

# merge the cleaned list
dhs_combined <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  list_dhs_cleaned
)

dhs_combined$Survey <- "DHS" # Adding the survey column

#---MICS----
list_mics_cleaned <- lapply(list_mics, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # Convert labelled columns to character
})

# merge the cleaned list
mics_combined <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  list_mics_cleaned
)

mics_combined$survey <- "MICS" # Adding the survey column


#----PHIA-------
list_phia_cleaned <- lapply(list_phia, function(df) {
  df %>%
    mutate(across(where(is.labelled), ~ as.character(.))) # Convert labelled columns to character
})

# Now merge the cleaned list
phia_combined <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  list_phia_cleaned
)

phia_combined$survey <- "PHIA" # Adding the survey column

#BAIS
bais$Survey <- "BAIS"

#KAIS
kais$Survey <- "KAIS"


# Combine all cleaned survey datasets into a single list
surveys_list <- list(dhs_combined, mics_combined, phia_combined, bais, kais)

# Merge all surveys in surveys_list into a single data frame, pooled_surveys
pooled_surveys <- Reduce(
  function(x, y) merge(x, y, all = TRUE),
  surveys_list
)


# Save all cleaned data
save(dhs_combined, mics_combined, phia_combined, bais, kais,
     pooled_surveys,
     list_dhs, list_mics, list_phia, 
     file = "New_Cleaned_Pooled_Surveys.RData")


# Load data into a temporary environment
temp_env <- new.env()
load("D:/Downloads/MSc Thesis/hiv-selftesting/combine all/New_Cleaned_Pooled_Surveys.RData", envir = temp_env)
pooled_surveys <- temp_env$pooled_surveys
pooled_surveys$hivst_use <- ifelse(pooled_surveys$hivst_use %in% c(0, 1), 
                                   pooled_surveys$hivst_use, NA)
pooled_surveys <- pooled_surveys[!is.na(pooled_surveys$hivst_use), ]


#mixed effect logistic reg model (FE: agegrp + sex + agegrp:sex, RE: country, survey)
#model <- glmer(hivst_use ~ agegrp * sex + (1 | country) + (1 | survey), data = pooled_surveys, family = binomial(link = "logit"))


model <- glmmTMB(hivst_use ~ agegrp + sex + agegrp * sex + as.factor(survey_id) + (1 | psu), 
                 data = pooled_surveys, 
                 family = binomial(link = "logit"))

summary(model)

df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                sex = 0, survey_id = "MDG2021DHS", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                 sex = 1, survey_id = "MDG2021DHS", psu = 1)
df <- rbind(df0, df1)

df$prd <- predict(model, newdata =  df, type = "response")
df <- df[order(as.numeric(df$agegrp)), ]

plot(prd ~ agegrp, data = subset(df, sex == 1), type = "l", col = "pink4")
lines(prd ~ agegrp, data = subset(df, sex == 0), col = "steelblue4")


glm_fit <- glm(hivst_use ~ agegrp + sex + agegrp * sex + as.factor(survey_id), 
             data = pooled_surveys, family = "binomial")
summary(glm_fit)
             
#----------
model <- glmmTMB(hivst_use ~ agegrp + sex + agegrp * sex + (1 | country) + (1 | survey), 
                 +                  data = pooled_surveys, 
                 +                  family = binomial(link = "logit"))

summary(model)
glm_fit <- glm(hivst_use ~ agegrp + sex + agegrp * sex + survey, 
               +              data = pooled_surveys, family = "binomial")
summary(glm_fit)
summary(pooled_surveys$survey_id)
summary(as.factor(pooled_surveys$survey_id))


df <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                 +                 sex = 0, survey_id = "ZWE2020PHIA")
df


df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                  +                 sex = 0, survey_id = "ZWE2020PHIA")
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                    +                  sex = 1, survey_id = "ZWE2020PHIA")
df <- cbind(df0, df1)


summary(as.factor((pooled_surveys$psu)))
df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                  +                 sex = 0, survey_id = "ZWE2020PHIA", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                    +                  sex = 1, survey_id = "ZWE2020PHIA", psu = 1)
df <- cbind(df0, df1)
predict(model, newdata =  df)


help("predict")
predict(model, newdata =  df, type = "response")
df

df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                  +                 sex = 0, survey_id = "ZWE2020PHIA", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                    +                  sex = 1, survey_id = "ZWE2020PHIA", psu = 1)
df <- rbind(df0, df1)
prd <- predict(model, newdata =  df, type = "response")
prd

df$prd <- predict(model, newdata =  df, type = "response")
 plot(prd ~ age, data = subset(df, sex == 0), type = "l")
#Error in eval(predvars, data, env) : object 'age' not found
plot(prd ~ agegrp, data = subset(df, sex == 0), type = "l", col = "pink4")
 lines(prd ~ agegrp, data = subset(df, sex == 1), col = "steelblue4")
subset(df, sex == 0)

plot(prd ~ as.numeric(agegrp), data = subset(df, sex == 0), type = "l", col = "pink4")
lines(prd ~ as.numeric(agegrp), data = subset(df, sex == 1), col = "steelblue4")
plot(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 0), type = "l", col = "pink4")
lines(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 1), col = "steelblue4")
plot(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 0), type = "l", col = "pink4")
lines(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 1), col = "steelblue4")
plot(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 0), type = "l", col = "pink4")
df[sort(as.numeric(df$agegrp)), ]


df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                  +                 sex = 0, survey_id = "ZWE2020PHIA", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                    +                  sex = 1, survey_id = "ZWE2020PHIA", psu = 1)
df <- rbind(df0, df1)
df

prd ~ age, data = subset(df, sex == 0),
#Error: unexpected ',' in "prd ~ age,"
sort(as.numeric(df$agegrp))
(as.numeric(df$agegrp))
order(as.numeric(df$agegrp))
df[order(as.numeric(df$agegrp)), ]




df <- df[order(as.numeric(df$agegrp)), ]
plot(prd ~ I(as.numeric(agegrp)), data = subset(df, sex == 0), type = "l", col = "pink4")
#Error in (function (formula, data = NULL, subset = NULL, na.action = na.fail,  :  variable lengths differ (found for 'I(as.numeric(agegrp))')
df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                                        +                 sex = 0, survey_id = "ZWE2020PHIA", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                                        +                  sex = 1, survey_id = "ZWE2020PHIA", psu = 1)
df <- rbind(df0, df1)
df$prd <- predict(model, newdata =  df, type = "response")
df <- df[order(as.numeric(df$agegrp)), ]
plot(prd ~ agegrp, data = subset(df, sex == 0), type = "l", col = "pink4")
lines(prd ~ agegrp, data = subset(df, sex == 1), col = "steelblue4")
plot(prd ~ agegrp, data = subset(df, sex == 1), type = "l", col = "pink4")
lines(prd ~ agegrp, data = subset(df, sex == 0), col = "steelblue4")
df0 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                                        +                 sex = 0, survey_id = "MDG2021DHS", psu = 1)
df1 <- data.frame(agegrp = levels(as.factor(pooled_surveys$agegrp)), 
                                        +                  sex = 1, survey_id = "MDG2021DHS", psu = 1)
 df <- rbind(df0, df1)
 df$prd <- predict(model, newdata =  df, type = "response")
df <- df[order(as.numeric(df$agegrp)), ]

plot(prd ~ agegrp, data = subset(df, sex == 1), type = "l", col = "pink4")
lines(prd ~ agegrp, data = subset(df, sex == 0), col = "steelblue4")


summary(as.factor(pooled_surveys$survey_id))





















