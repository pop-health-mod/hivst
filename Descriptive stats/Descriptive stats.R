
library(dplyr)
library(survey)
library(stringr)
library(survey)


# --------Table 1: proportions of people who knows about and used HIVST------------

# Sample size for each survey

sample_sizes_dhs <- lapply(list_dhs, nrow) #DHS
sample_sizes_phia <- lapply(list_phia, nrow) #PHIA
sample_sizes_mics <- lapply(list_mics, function(df) {
  nrow(df %>% filter(!is.na(strata), !is.na(ind_wt), !is.na(ever_heard)))
}) #MICS
sample_sizes_kais <- 13720 #KAIS
sample_sizes_bais <- 17205 #BAIS


# Extracting the survey id 

survey_ids_dhs <- sapply(list_dhs, function(df) unique(df$survey_id))
survey_ids_phia <- sapply(list_phia, function(df) unique(df$survey_id))
survey_ids_mics <- sapply(list_mics, function(df) unique(df$survey_id))
survey_ids_kais <- "KEN2012KAIS"
survey_ids_bais <- "BWA2021BAIS"

# Function to calculate proportions for hivst knowledge and hivst use
calculate_proportions_knwldge <- function(data) {
  survey_designs <- svydesign( 
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  prop <- svyciprop(~I(hivst_knwldge == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}

calculate_proportions_use <- function(data) {
survey_designs <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  prop <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}


# ----------DHS--------------
#calculate_proportions_use(list_dhs[[1]]) # checking function
#calculate_proportions_use(list_dhs[[2]])

# Applying the function to the list of DHS datasets
hivstknwldge_dhs <- lapply(list_dhs, calculate_proportions_knwldge)
hivstuse_dhs <- lapply(list_dhs, calculate_proportions_use)


# ------------PHIA--------------
hivstuse_phia <- lapply(list_phia, calculate_proportions_use)



#------------KAIS(no HIVST knowledge)---------------

#prop_hivst_use_kais #0.03 (0.02, 0.04) 

#----------BAIS (no HIVST knowledge)----------------
#prop_hivst_use_bais #0.0213 (0.0179, 0.0252)


#----------MICS(some have NAs in strata and individual weight)-------

#HIVST knowledge function

calculate_proportions_knwldge <- function(data) {
  # Filter out rows with NA or zero weights
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
    survey_designs <- svydesign( 
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data_clean, nest = TRUE)
    prop <- svyciprop(~I(hivst_knwldge == 1), design = survey_designs, method = "logit", level = 0.95)
  return(prop)
}

hivstknwldge_mics <- lapply(list_mics, calculate_proportions_knwldge)

#HIVST use function

calculate_proportions_use_mics <- function(data) {
  # Filter out rows with NA or zero weights
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
    survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data_clean, nest = TRUE, lonely.psu = "adjust")
    prop <- svyciprop(~I(hivst_use == 1), design = survey_designs, method = "logit", level = 0.95)
   return(prop)
}

hivstuse_mics <- lapply(list_mics, calculate_proportions_use_mics)



#-------------------------------------------------------------

# Creating Table 1

#---------DHS-----------------

temp_df_dhs <- data.frame(do.call(rbind,c(survey_ids_dhs, sample_sizes_dhs)))
temp_df_2_dhs <- data.frame(cbind(temp_df_dhs[c(1:22),1], temp_df_dhs[c(23:44),1]))

#HIVST knowledge
list_hivst_knwldge_dhs <- do.call(rbind, lapply(hivstknwldge_dhs, function(x) {
  estimate <- unname(x)  
  ci <- attr(x, "ci")    
  data.frame(
    Proportion = estimate,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  )
}))


final_hivst_knwldge_dhs <- data.frame(temp_df_2_dhs, list_hivst_knwldge_dhs) %>% as_tibble() %>%
select("survey_id"="X1","sample_size"="X2","hivst_knowledge_prop"="Proportion","hivst_knowledge_Lower_CI"="Lower_CI","hivst_knowledge_Upper_CI"="Upper_CI")

#HIVST use

list_hivst_use_dhs <- do.call(rbind, lapply(hivstuse_dhs, function(x) {
  estimate <- unname(x)  
  ci <- attr(x, "ci")    
  data.frame(
    Proportion = estimate,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  )
}))

final_hivstuse_dhs <- data.frame(temp_df_2_dhs, list_hivst_use_dhs) %>% as_tibble() %>%
select("survey_id"="X1","sample_size"="X2","hivst_use_prop"="Proportion", "hivst_use_lower_CI"="Lower_CI", "hivst_use_upper_CI"="Upper_CI")


#Combining hivst knowledge and use for DHS

final_hivstuse_dhs_clean <- final_hivstuse_dhs %>%
  select(-survey_id, -sample_size)

combined_overall_hivst_dhs <- cbind(final_hivst_knwldge_dhs, final_hivstuse_dhs_clean)

saveRDS(combined_overall_hivst_dhs, "hivst_overall_dhs.rds")


#---------PHIA-----------------

temp_df_phia <- data.frame(do.call(rbind,c(survey_ids_phia, sample_sizes_phia)))
temp_df_2_phia <- data.frame(cbind(temp_df_phia[c(1:7),1], temp_df_phia[c(8:14),1]))

#HIVST use

list_hivst_use_phia <- do.call(rbind, lapply(hivstuse_phia, function(x) {
  estimate <- unname(x)  
  ci <- attr(x, "ci")    
  data.frame(
    Proportion = estimate,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  )
}))

final_hivstuse_phia <- data.frame(temp_df_2_phia, list_hivst_use_phia) %>% as_tibble() %>%
  select("survey_id"="X1","sample_size"="X2","hivst_use_prop"="Proportion", "hivst_use_lower_CI"="Lower_CI", "hivst_use_upper_CI"="Upper_CI")

# only malawi phia has hivst knowledge information
# 0.317(0.304, 0.329)

#Combining hivst knowledge and use for PHIA
final_hivstuse_phia <- final_hivstuse_phia %>%
  mutate(hivst_knowledge_prop = if_else(survey_id == "MWI2020PHIA", 0.317, NA_real_),
         hivst_knowledge_Lower_CI = if_else(survey_id == "MWI2020PHIA", 0.304, NA_real_),
         hivst_knowledge_Upper_CI = if_else(survey_id == "MWI2020PHIA", 0.329, NA_real_))

combined_overall_hivst_phia <- final_hivstuse_phia %>%
  select(survey_id, sample_size, hivst_knowledge_prop, hivst_knowledge_Lower_CI, hivst_knowledge_Upper_CI, everything())


saveRDS(combined_overall_hivst_phia, "hivst_overall_phia.rds")


# ----------MICS---------------

temp_df <- data.frame(do.call(rbind,c(survey_ids_mics, sample_sizes_mics)))
temp_df_2 <- data.frame(cbind(temp_df[c(1:14),1], temp_df[c(15:28),1]))

#HIVST knowledge
list_hivst_knwldge_mics <- do.call(rbind, lapply(hivstknwldge_mics, function(x) {
  estimate <- unname(x)  
  ci <- attr(x, "ci")    
  data.frame(
    Proportion = estimate,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  )
}))

final_hivst_knwldge_mics <- data.frame(temp_df_2, list_hivst_knwldge_mics) %>% as_tibble() %>%
  select("survey_id"="X1","sample_size"="X2","hivst_knowledge_prop"="Proportion","hivst_knowledge_Lower_CI"="Lower_CI","hivst_knowledge_Upper_CI"="Upper_CI")

#HIVST use

list_hivst_use_mics <- do.call(rbind, lapply(hivstuse_mics, function(x) {
  estimate <- unname(x)  
  ci <- attr(x, "ci")    
    data.frame(
    Proportion = estimate,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  )
}))

final_hivstuse_mics <- data.frame(temp_df_2, list_hivst_use_mics) %>% as_tibble() %>%
select("survey_id"="X1","sample_size"="X2","hivst_use_prop"="Proportion", "hivst_use_lower_CI"="Lower_CI", "hivst_use_upper_CI"="Upper_CI")


#Combining hivst knowledge and use for MICS

final_hivstuse_mics_clean <- final_hivstuse_mics %>%
  select(-survey_id, -sample_size)

combined_overall_hivst_mics <- cbind(final_hivst_knwldge_mics, final_hivstuse_mics_clean)

saveRDS(combined_overall_hivst_mics, "hivst_overall_mics.rds")


# Combining all for Table 1

final_overall_hivst_dhs <- readRDS("D:/Downloads/MSc Thesis/hiv-selftesting/Descriptive Table 1/hivst_overall_dhs.rds")
final_overall_hivst_phia <- readRDS("D:/Downloads/MSc Thesis/hiv-selftesting/Descriptive Table 1/hivst_overall_phia.rds")
final_overall_hivst_mics <- readRDS("D:/Downloads/MSc Thesis/hiv-selftesting/Descriptive Table 1/hivst_overall_mics.rds")

final_combined_overall_hivst <- rbind(final_overall_hivst_dhs, final_overall_hivst_phia, final_overall_hivst_mics)

#adding KAIS
new_row_kais <- data.frame(
  survey_id = "KEN2012KAIS",
  sample_size = 13694,
  hivst_knowledge_prop = NA_real_,
  hivst_knowledge_Lower_CI = NA_real_,
  hivst_knowledge_Upper_CI = NA_real_,
  hivst_use_prop = 0.034,
  hivst_use_lower_CI = 0.03,
  hivst_use_upper_CI = 0.04
)

final_combined_overall_hivst <- rbind(final_combined_overall_hivst, new_row_kais)


#adding BAIS
new_row_bais <- data.frame(
  survey_id = "BWA2021BAIS",
  sample_size = 17205,
  hivst_knowledge_prop = NA_real_,
  hivst_knowledge_Lower_CI = NA_real_,
  hivst_knowledge_Upper_CI = NA_real_,
  hivst_use_prop = 0.0213,
  hivst_use_lower_CI = 0.0179,
  hivst_use_upper_CI = 0.0252
)

# Bind the new row to the existing dataframe
final_combined_overall_hivst <- rbind(final_combined_overall_hivst, new_row_bais)


# Extracting country_id, year, and survey_name from survey_id
final_combined_overall_hivst <- final_combined_overall_hivst %>%
  mutate(
    country_id = substr(survey_id, 1, 3),  # First 3 letters for country_id
    year = substr(survey_id, 4, 7),        # Next 4 numbers for year
    survey_name = str_extract(survey_id, "(DHS|PHIA|MICS|KAIS|BAIS)")  # Extract survey name
  )


# list of unique country IDs (35 countries)
unique_country_ids <- final_combined_overall_hivst %>%
  distinct(country_id)
print(unique_country_ids)


# creating country column for iso3 country codes
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

# Creating country column from country_id
final_combined_overall_hivst <- final_combined_overall_hivst %>%
  mutate(country = recode(country_id, !!!iso3_to_country))

final_combined_overall_hivst <- final_combined_overall_hivst %>%
  select(survey_id, country_id, country, year, survey_name, sample_size, everything())

# arranging alphabetically by country
final_combined_overall_hivst <- final_combined_overall_hivst %>%
  arrange(country)

write.csv(final_combined_overall_hivst, "D:/Downloads/MSc Thesis/hiv-selftesting/overall_hivst_knwld_use.csv", row.names = FALSE)


#---------------------------------------------------------------------------------------------------

#-------Table 2: Proportions of HIVST use by sex-------------

# Function to calculate hivst prop by sex

calcbysex_prop_use <- function(data) {
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
    hivst_by_sex <- svyby(~I(hivst_use == 1), ~sex, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_sex)
}

#--------------- DHS -----------------

propsex_dhs <- lapply(list_dhs, calcbysex_prop_use)

#creating tibble of proportion of HIVST use by sex: DHS

temp_df_dhs <- data.frame(do.call(rbind,c(survey_ids_dhs, sample_sizes_dhs)))
temp_df2_dhs <- data.frame(survey_id = temp_df_dhs[1:22, 1], sample_size = temp_df_dhs[23:44, 1])

list_dhs_df <- data.frame(do.call(rbind,lapply(propsex_dhs, data.frame))) %>%
  mutate(sno=row_number())

temp_df_28_dhs <- temp_df2_dhs[rep(1:nrow(temp_df2_dhs),each=2),]

final_df_dhs <- data.frame(temp_df_28_dhs, list_dhs_df) %>% as_tibble() %>%
  select(survey_id, sample_size, sex, "hivst_use_prop"="I.hivst_use....1.", "ci_lower"="ci_l", "ci_upper"="ci_u")


saveRDS(final_df_dhs, "hivstuse_sex_dhs.rds")

#--------------- PHIA ----------------

propsex_phia <- lapply(list_phia, calcbysex_prop_use)

#creating tibble of proportion of HIVST use by sex: PHIA

temp_df_phia <- data.frame(do.call(rbind,c(survey_ids_phia, sample_sizes_phia)))
temp_df2_phia <- data.frame(survey_id = temp_df_phia[1:7, 1], sample_size = temp_df_phia[8:14, 1])

list_phia_df <- data.frame(do.call(rbind,lapply(propsex_phia, data.frame))) %>%
  mutate(sno=row_number())

temp_df_28_phia <- temp_df2_phia[rep(1:nrow(temp_df2_phia),each=2),]

final_df_phia <- data.frame(temp_df_28_phia, list_phia_df) %>% as_tibble() %>%
  select(survey_id, sample_size, sex, "hivst_use_prop"="I.hivst_use....1.", "ci_lower"="ci_l", "ci_upper"="ci_u")

saveRDS(final_df_phia, "hivstuse_sex_phia.rds")



# ---------------- MICS ---------------
calcbysex_prop_use_mics <- function(data) {
  # Filtering out rows with NA or zero weights
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
    survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data_clean, nest = TRUE)
    hivst_by_sex <- svyby(~I(hivst_use == 1), ~sex, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_sex)
}

propsex_mics <- lapply(list_mics, calcbysex_prop_use_mics)

#creating tibble of proportion of HIVST use by sex: MICS

temp_df <- data.frame(do.call(rbind,c(survey_ids_mics, sample_sizes_mics)))
temp_df_2 <- data.frame(cbind(temp_df[c(1:14),1], temp_df[c(15:28),1]))

list_opd_df <- data.frame(do.call(rbind,lapply(propsex_mics, data.frame))) %>%
  mutate(sno=row_number())

temp_df_28 <- temp_df_2[rep(1:nrow(temp_df_2),each=2),]
  

final_df_mics <- data.frame(temp_df_28, list_opd_df) %>% as_tibble() %>%
  select("survey_id"="X1", "sample_size"="X2", sex, "hivst_use_prop"="I.hivst_use....1.", "ci_lower"="ci_l", "ci_upper"="ci_u")

saveRDS(final_df_mics, "hivstuse_sex_mics.rds")


#----Combining all----------

final_df_dhs <- readRDS("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex\\hivstuse_sex_dhs.rds")
final_df_mics <- readRDS("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex\\hivstuse_sex_mics.rds")
final_df_phia <- readRDS("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex\\hivstuse_sex_phia.rds")

final_combined_df <- rbind(final_df_dhs, final_df_mics, final_df_phia)

#adding KAIS
kais_rows <- data.frame(
  survey_id = c("KEN2012KAIS", "KEN2012KAIS"),
  sample_size = c(13694, 13694),
  sex = c(0, 1),
  hivst_use_prop = c(0.02108996, 0.05243847),
  ci_lower = c(0.01727204, 0.03506925),
  ci_upper = c(0.02572972, 0.07771751)
)

final_all_kais <- rbind(final_combined_df, kais_rows)

#adding BAIS
bais_rows <- data.frame(
  survey_id = c("BWA2021BAIS", "BWA2021BAIS"),
  sample_size = c(17205, 17205),
  sex = c(0, 1),
  hivst_use_prop = c(0.02348760, 0.01897265),
  ci_lower = c(0.01860713, 0.01476169),
  ci_upper = c(0.02960954, 0.02435514)
)

final_df_allsurveys <- rbind(final_all_kais, bais_rows)

# Finalizing the dataframe
final_df_allsurveys <- final_df_allsurveys %>%
  mutate(sex = ifelse(sex == 0, "Female", "Male"))

# Extracting country_id, year, and survey name from survey ID
final_df_allsurveys <- final_df_allsurveys %>%
  mutate(
    country_id = substr(survey_id, 1, 3),     
    year = substr(survey_id, 4, 7),             
    survey_name = ifelse(nchar(survey_id) == 10, substr(survey_id, 8, 10), substr(survey_id, 8, 11)))


# list of unique country IDs (35 countries)
unique_country_ids <- final_df_allsurveys %>%
  distinct(country_id)
print(unique_country_ids, n=Inf)


# creating country column for iso3 country codes
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

final_df_allsurveys <- final_df_allsurveys %>%
  mutate(country = iso3_to_country[country_id])

final_df_allsurveys <- final_df_allsurveys %>%
  select(survey_id, country_id, country, year, survey_name, sample_size, everything())

# arranging alphabetically by country
final_df_allsurveys <- final_df_allsurveys %>%
  arrange(country)

print(final_df_allsurveys, n = Inf)

write.csv(final_df_allsurveys, "D:/Downloads/MSc Thesis/hiv-selftesting/HIVST use by sex/final_hivstuse_bysex.csv", row.names = FALSE)


#----------------Table 3: HIVST by age groups--------------------


setwd("D:/Downloads/MSc Thesis/hiv-selftesting/Descriptive stats")


list_dhs <- readRDS("list_dhs.rds")
list_mics <- readRDS("list_mics.rds")
list_phia <- readRDS("list_phia_dataframe.rds")
bais <- readRDS("bais_dataframe.rds")
kais <- readRDS("kais_dataframe.rds")


#function
calcbyage_prop_use <- function(data) {
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  hivst_by_age <- svyby(~I(hivst_use == 1), ~agegrp, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_age)
}

#--------------- DHS -----------------

propage_dhs <- lapply(list_dhs, calcbyage_prop_use)
survey_ids_dhs <- sapply(list_dhs, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age: DHS
list_dhs_df <- data.frame(do.call(rbind, lapply(propage_dhs, data.frame))) %>%
  mutate(sno = row_number())

# Set option to avoid scientific notation globally
options(scipen = 999)

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_dhs, sapply(propage_dhs, nrow))

# Add the survey_id column to the dataframe
final_df_dhs <- data.frame(survey_id = survey_id_col, list_dhs_df) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")


saveRDS(final_df_dhs, "hivstuse_age_dhs.rds")



#--------------- PHIA ----------------

propage_phia <- lapply(list_phia, calcbyage_prop_use)
survey_ids_phia <- sapply(list_phia, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age: DHS
list_phia_df <- data.frame(do.call(rbind, lapply(propage_phia, data.frame))) %>%
  mutate(sno = row_number())


# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_phia, sapply(propage_phia, nrow))

# Add the survey_id column to the dataframe
final_df_phia <- data.frame(survey_id = survey_id_col, list_phia_df) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")


saveRDS(final_df_phia, "hivstuse_age_phia.rds")


#--------MICS-----------
#function
calcbyage_prop_use_mics <- function(data) {
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  hivst_by_age <- svyby(~I(hivst_use == 1), ~agegrp, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_age)
}

propage_mics <- lapply(list_mics, calcbyage_prop_use_mics)
survey_ids_mics <- sapply(list_mics, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age: DHS
list_mics_df <- data.frame(do.call(rbind, lapply(propage_mics, data.frame))) %>%
  mutate(sno = row_number())


# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_mics, sapply(propage_mics, nrow))

# Add the survey_id column to the dataframe
final_df_mics <- data.frame(survey_id = survey_id_col, list_mics_df) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")


saveRDS(final_df_mics, "hivstuse_age_mics.rds")


# BAIS

bais_designs <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = bais, nest = TRUE)

hivst_by_age_bais <- svyby(~I(hivst_use == 1), ~agegrp, bais_designs,
                      svyciprop, method = "logit", vartype = "ci")


final_df_bais <- hivst_by_age_bais %>%
  mutate(survey_id = survey_ids_bais) %>%
  select(survey_id, everything())


# KAIS

kais_designs <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = kais, nest = TRUE)

hivst_by_age_kais <- svyby(~I(hivst_use == 1), ~agegrp, kais_designs,
                           svyciprop, method = "logit", vartype = "ci")

final_df_kais <- hivst_by_age_kais %>%
  mutate(survey_id = survey_ids_kais) %>%
  select(survey_id, everything())

# Standardizing column names for the dataframes that differ
final_df_bais <- final_df_bais %>%
  rename(age_group = agegrp, hivst_use_prop = `I(hivst_use == 1)`, ci_lower = ci_l, ci_upper = ci_u)

final_df_kais <- final_df_kais %>%
  rename(age_group = agegrp, hivst_use_prop = `I(hivst_use == 1)`, ci_lower = ci_l, ci_upper = ci_u)


# Combining all for hivst by age 

combined_df <- bind_rows(final_df_dhs, final_df_mics, final_df_phia, final_df_kais, final_df_bais)


# Recode age_group in combined_df
final_df_allsurveys <- combined_df %>%
  mutate(
    age_group = case_when(
      age_group == 1 ~ "15-19",
      age_group == 2 ~ "20-24",
      age_group == 3 ~ "25-29",
      age_group == 4 ~ "30-34",
      age_group == 5 ~ "35-39",
      age_group == 6 ~ "40-44",
      age_group == 7 ~ "45-49",
      age_group == 8 ~ "50-54",
      age_group == 9 ~ "55-59",
      age_group == 10 ~ "60-64",
      age_group == 11 ~ "65+"
    )
  )

final_df_allsurveys <- final_df_allsurveys %>%
  mutate(
    country_id = substr(survey_id, 1, 3),     # First 3 characters for country_id
    year = substr(survey_id, 4, 7),           # Next 4 characters for year
    survey_name = case_when(                  # Conditional for extracting survey_name based on length
      nchar(survey_id) == 10 ~ substr(survey_id, 8, 10),  # For 10-character survey IDs
      nchar(survey_id) == 11 ~ substr(survey_id, 8, 11),  # For 11-character survey IDs
      TRUE ~ substr(survey_id, 8, nchar(survey_id))       # Default case for other lengths
    )
  )


# creating country column for iso3 country codes
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

final_df_allsurveys <- final_df_allsurveys %>%
  mutate(country = iso3_to_country[country_id])

final_df_allsurveys <- final_df_allsurveys %>%
  select(survey_id, country_id, country, year, survey_name, everything())

# arranging alphabetically by country
final_df_allsurveys <- final_df_allsurveys %>%
  arrange(country)


final_df_allsurveys <- final_df_allsurveys %>%
  mutate(
    hivst_use_prop = round(hivst_use_prop * 100, 2),
    ci_lower = round(ci_lower * 100, 2),
    ci_upper = round(ci_upper * 100, 2)
  )


write.csv(final_df_allsurveys, "D:/Downloads/MSc Thesis/hiv-selftesting/final_hivstuse_byage.csv", row.names = FALSE)


#----------------Table 4: HIVST by sex and by age groups--------------------


#function
calcbyage_prop_use <- function(data) {
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  hivst_by_age <- svyby(~I(hivst_use == 1), ~agegrp, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_age)
}

#--------------- DHS -----------------
#female
propage_dhs_f <- lapply(list_dhs_f, calcbyage_prop_use)
survey_ids_dhs <- sapply(list_dhs, function(df) unique(df$survey_id))
propage_dhs_f <- lapply(propage_dhs_f, function(df) {
  df[] <- lapply(df, function(x) if (is.labelled(x)) as.numeric(x) else x)
  return(df)
})

#creating tibble of proportion of HIVST use by sex and age: DHS

list_dhs_df_f <- data.frame(do.call(rbind, lapply(propage_dhs_f, data.frame))) %>%
  mutate(sno = row_number())

# Set option to avoid scientific notation globally
options(scipen = 999)

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_dhs, sapply(propage_dhs_f, nrow))

# Add the survey_id column to the dataframe
final_df_dhs_f <- data.frame(survey_id = survey_id_col, list_dhs_df_f) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_dhs_f <- final_df_dhs_f %>%
  mutate(sex = 0)  # 0 for Female


#male
propage_dhs_m <- lapply(list_dhs_m, calcbyage_prop_use)
survey_ids_dhs <- sapply(list_dhs, function(df) unique(df$survey_id))
propage_dhs_m <- lapply(propage_dhs_m, function(df) {
  df[] <- lapply(df, function(x) if (is.labelled(x)) as.numeric(x) else x)
  return(df)
})

#creating tibble of proportion of HIVST use by sex and age: DHS

list_dhs_df_m <- data.frame(do.call(rbind, lapply(propage_dhs_m, data.frame))) %>%
  mutate(sno = row_number())

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_dhs, sapply(propage_dhs_m, nrow))

# Add the survey_id column to the dataframe
final_df_dhs_m <- data.frame(survey_id = survey_id_col, list_dhs_df_m) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_dhs_m <- final_df_dhs_m %>%
  mutate(sex = 1)  # 1 for Male


final_dhs_df_combined <- rbind(final_df_dhs_f, final_df_dhs_m)
saveRDS(final_dhs_df_combined, "hivstuse_sex&age_dhs.rds")


#--------------- PHIA ----------------
#female
options(survey.lonely.psu = "adjust")
propage_phia_f <- lapply(list_phia_f, calcbyage_prop_use)
survey_ids_phia <- sapply(list_phia_f, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age
list_phia_df_f <- data.frame(do.call(rbind, lapply(propage_phia_f, data.frame))) %>%
  mutate(sno = row_number())


# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_phia, sapply(propage_phia_f, nrow))

# Add the survey_id column to the dataframe
final_df_phia_f <- data.frame(survey_id = survey_id_col, list_phia_df_f) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_phia_f <- final_df_phia_f %>%
  mutate(sex = 0)  # 0 for Female


#male
options(survey.lonely.psu = "adjust")
propage_phia_m <- lapply(list_phia_m, calcbyage_prop_use)

#creating tibble of proportion of HIVST use by age 
list_phia_df_m <- data.frame(do.call(rbind, lapply(propage_phia_m, data.frame))) %>%
  mutate(sno = row_number())

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col <- rep(survey_ids_phia, sapply(propage_phia_m, nrow))

# Add the survey_id column to the dataframe
final_df_phia_m <- data.frame(survey_id = survey_id_col, list_phia_df_m) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_phia_m <- final_df_phia_m %>%
  mutate(sex = 1)  # 1 for male

final_phia_df_combined <- rbind(final_df_phia_f, final_df_phia_m)

saveRDS(final_phia_df_combined, "hivstuse_sex&age_phia.rds")


#--------MICS-----------
#function
calcbyage_prop_use_mics <- function(data) {
  data_clean <- data %>%
    filter(!is.na(ind_wt), ind_wt > 0)
  survey_designs <- svydesign(
    ids = ~psu, strata = ~strata, weights = ~ind_wt, data = data, nest = TRUE)
  hivst_by_age <- svyby(~I(hivst_use == 1), ~agegrp, survey_designs,
                        svyciprop, method = "logit", vartype = "ci")
  return(hivst_by_age)
}

#--female---
propage_mics_f <- lapply(list_mics_f, calcbyage_prop_use_mics)
survey_ids_mics_f <- sapply(list_mics_f, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age and sex: MICS
list_mics_df_f <- data.frame(do.call(rbind, lapply(propage_mics_f, data.frame))) %>%
  mutate(sno = row_number())

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col_f <- rep(survey_ids_mics_f, sapply(propage_mics_f, nrow))

# Add the survey_id column to the dataframe
final_df_mics_f <- data.frame(survey_id = survey_id_col_f, list_mics_df_f) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_mics_f <- final_df_mics_f %>%
  mutate(sex = 0)  # 0 for Female


#--male---
propage_mics_m <- lapply(list_mics_m, calcbyage_prop_use_mics)
survey_ids_mics_m <- sapply(list_mics_m, function(df) unique(df$survey_id))

#creating tibble of proportion of HIVST use by age and sex: MICS
list_mics_df_m <- data.frame(do.call(rbind, lapply(propage_mics_m, data.frame))) %>%
  mutate(sno = row_number())

# Create a column for survey IDs by repeating each survey_id for each age group in the corresponding country
survey_id_col_m <- rep(survey_ids_mics_m, sapply(propage_mics_m, nrow))

# Add the survey_id column to the dataframe
final_df_mics_m <- data.frame(survey_id = survey_id_col_m, list_mics_df_m) %>%
  select(survey_id, age_group = agegrp, "hivst_use_prop" = "I.hivst_use....1.", "ci_lower" = "ci_l", "ci_upper" = "ci_u")

final_df_mics_m <- final_df_mics_m %>%
  mutate(sex = 1)  # 1 for male

final_mics_df_combined <- rbind(final_df_mics_f, final_df_mics_m)

saveRDS(final_mics_df_combined, "hivstuse_Sex&age_mics.rds")


# BAIS
#female
bais_designs_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = bais_f, nest = TRUE)

hivst_by_age_bais_f <- svyby(~I(hivst_use == 1), ~agegrp, bais_designs_f,
                           svyciprop, method = "logit", vartype = "ci")


final_df_bais_f <- hivst_by_age_bais_f %>%
  mutate(survey_id = survey_ids_bais) %>%
  select(survey_id, everything())

final_df_bais_f <- final_df_bais_f %>%
  mutate(sex = 0)  # 0 for Female

#male
bais_designs_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = bais_m, nest = TRUE)
options(survey.lonely.psu = "adjust")
hivst_by_age_bais_m <- svyby(~I(hivst_use == 1), ~agegrp, bais_designs_m,
                             svyciprop, method = "logit", vartype = "ci")


final_df_bais_m <- hivst_by_age_bais_m %>%
  mutate(survey_id = survey_ids_bais) %>%
  select(survey_id, everything())

final_df_bais_m <- final_df_bais_m %>%
  mutate(sex = 1)  # 1 for male

final_bais_df_combined <- rbind(final_df_bais_f, final_df_bais_m)

saveRDS(final_bais_df_combined, "hivstuse_sex&age_bais.rds")

# KAIS

#female
kais_designs_f <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = kais_f, nest = TRUE)

hivst_by_age_kais_f <- svyby(~I(hivst_use == 1), ~agegrp, kais_designs_f,
                             svyciprop, method = "logit", vartype = "ci")


final_df_kais_f <- hivst_by_age_kais_f %>%
  mutate(survey_id = survey_ids_kais) %>%
  select(survey_id, everything())

final_df_kais_f <- final_df_kais_f %>%
  mutate(sex = 0)  # 0 for Female

#male
kais_designs_m <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~ind_wt, data = kais_m, nest = TRUE)
hivst_by_age_kais_m <- svyby(~I(hivst_use == 1), ~agegrp, kais_designs_m,
                             svyciprop, method = "logit", vartype = "ci")


final_df_kais_m <- hivst_by_age_kais_m %>%
  mutate(survey_id = survey_ids_kais) %>%
  select(survey_id, everything())

final_df_kais_m <- final_df_kais_m %>%
  mutate(sex = 1)  # 1 for male

final_kais_df_combined <- rbind(final_df_kais_f, final_df_kais_m)

saveRDS(final_kais_df_combined, "hivstuse_sex&age_kais.rds")



# finalizing dataframe
setwd("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex and by age")
dhs <- readRDS("hivstuse_sex&age_dhs.rds")
mics <- readRDS("hivstuse_sex&age_mics.rds")
phia <- readRDS("hivstuse_sex&age_phia.rds")
bais <- readRDS("hivstuse_sex&age_bais.rds")
kais <- readRDS("hivstuse_sex&age_kais.rds")

# Standardizing column names for the dataframes that differ
bais <- bais %>%
  rename(age_group = agegrp, hivst_use_prop = `I(hivst_use == 1)`, ci_lower = ci_l, ci_upper = ci_u)

kais <- kais %>%
  rename(age_group = agegrp, hivst_use_prop = `I(hivst_use == 1)`, ci_lower = ci_l, ci_upper = ci_u)

options(scipen = 999)
combined_df <- bind_rows(dhs, mics, phia, kais, bais)


# Recode age_group in combined_df
final_df_allsurveys <- combined_df %>%
  mutate(
    age_group = case_when(
      age_group == 1 ~ "15-19",
      age_group == 2 ~ "20-24",
      age_group == 3 ~ "25-29",
      age_group == 4 ~ "30-34",
      age_group == 5 ~ "35-39",
      age_group == 6 ~ "40-44",
      age_group == 7 ~ "45-49",
      age_group == 8 ~ "50-54",
      age_group == 9 ~ "55-59",
      age_group == 10 ~ "60-64",
      age_group == 11 ~ "65+"
    )
  )

final_df_allsurveys <- final_df_allsurveys %>%
  mutate(
    country_id = substr(survey_id, 1, 3),     # First 3 characters for country_id
    year = substr(survey_id, 4, 7),           # Next 4 characters for year
    survey_name = case_when(                  # Conditional for extracting survey_name based on length
      nchar(survey_id) == 10 ~ substr(survey_id, 8, 10),  # For 10-character survey IDs
      nchar(survey_id) == 11 ~ substr(survey_id, 8, 11),  # For 11-character survey IDs
      TRUE ~ substr(survey_id, 8, nchar(survey_id))       # Default case for other lengths
    )
  )


# creating country column for iso3 country codes
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

final_df_allsurveys <- final_df_allsurveys %>%
  mutate(country = iso3_to_country[country_id])

final_df_allsurveys <- final_df_allsurveys %>%
  select(survey_id, country_id, country, year, survey_name, everything())

# arranging alphabetically by country
final_df_allsurveys <- final_df_allsurveys %>%
  arrange(country)


final_df_allsurveys <- final_df_allsurveys %>%
  mutate(
    hivst_use_prop = round(hivst_use_prop * 100, 2),
    ci_lower = round(ci_lower * 100, 2),
    ci_upper = round(ci_upper * 100, 2)
  )




write.csv(final_df_allsurveys, "D:/Downloads/MSc Thesis/hiv-selftesting/final_hivstuse_byage&sex.csv", row.names = FALSE)















