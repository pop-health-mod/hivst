


#-----DHS----------
setwd("D:/Downloads/MSc Thesis/1. thesis rawdata/DHS raw data")
bio_list_dhs <- readRDS("cleaned biomarker surveys/bio_list_dhs.rds")
#survey_ids_dhs <- sapply(test_list_dhs, function(df) unique(df$survey_id))


# function for calculating hivst use by hiv status
calc_prop_hivst_hivstat <- function(data) {
  # aubsetting data to include only hiv_status 0 and 1
  data_subset <- subset(data, hiv_status %in% c(0, 1))
  
  # creating the survey design object
  design <- svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = ~ind_wt,
    data = data_subset,
    nest = TRUE
  )
  
  # calculating survey-adjusted proportions by hiv_status
  result <- svyby(
    ~I(hivst_use == 1),
    ~hiv_status,
    design = design,
    FUN = svyciprop,
    method = "logit",
    vartype = "se",
    level = 0.95
  )
  
  return(result)
}

# checking whether the function works properly
calc_prop_hivst_hivstat(bio_list_dhs[[1]])
calc_prop_hivst_hivstat(bio_list_dhs[[2]])


# Applying the function to the list of DHS datasets
hivstat_hivstuse_dhs <- lapply(bio_list_dhs, calc_prop_hivst_hivstat)
