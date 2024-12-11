library(tidyr)
library(readxl)
library(dplyr)
library(tibble)
library(purrr)


setwd("D:/Downloads/MSc Thesis/hivst/WHO program data")
pgm_data <- read_excel("WHO_program_data.xlsx")
pgm_data <- pgm_data %>%
  rename(
    iso_code = COUNTRY_CODE,   
    country = COUNTRY_NAME,    
    year = TIME_PERIOD,        
    HTS = `SELF_TEST_DISTRIBUTED-DATA_VALUE` 
  ) %>%
  arrange(country, year)

# HIVST_Spectrum sheet
spectrum_data <- read_excel("WHO_program_data.xlsx", sheet = "HIVST_Spectrum")
spectrum_data_long <- spectrum_data %>%
  pivot_longer(
    cols = `2019`:`2023`,     
    names_to = "year",        
    values_to = "HTS"         
  ) %>%
  select(ISO_Alpha_3, Country, year, HTS)  

spectrum_data_long <- spectrum_data_long %>%
  rename(
    iso_code = ISO_Alpha_3,  
    country = Country        
  ) %>%
  mutate(year = as.numeric(year)) 

filtered_spectrum_data <- spectrum_data_long %>%
  anti_join(pgm_data, by = "country")  

pgm_data <- pgm_data %>%
  bind_rows(filtered_spectrum_data %>% filter(!is.na(HTS))) %>%
  arrange(country, year) 


# program data list for each country
cnt_pgm <- pgm_data %>%
  group_by(country) %>%  
  summarise(
    yr_hts = list(year),  
    ind_hts = list((year - start) / dt),  
    hts_dat = list(HTS),  
    se_hts = list(HTS * 0.1)  
  ) %>%
  mutate(
    country_list = pmap(
      list(yr_hts, ind_hts, hts_dat, se_hts),
      ~ list(
        yr_hts = ..1,    
        ind_hts = ..2,   
        hts_dat = ..3,   
        se_hts = ..4     
      )
    )
  ) %>%
  select(country, country_list) %>%
  deframe()  

#prepping list
dput(cnt_pgm)
output_file <- "formatted_output.txt"
sink(output_file)

for (country in names(cnt_pgm)) {
  cat(country, "= list(\n")
  cat("    yr_hts = c(", toString(cnt_pgm[[country]]$yr_hts), "),\n", sep = "")
  cat("    ind_hts = (c(", toString(cnt_pgm[[country]]$yr_hts), ") - start) / dt,\n", sep = "")
  cat("    hts_dat = c(", toString(cnt_pgm[[country]]$hts_dat), "),\n", sep = "")
  cat("    se_hts = c(", toString(cnt_pgm[[country]]$hts_dat), ") * 0.1\n", sep = "")
  cat("),\n\n")
}

sink()







