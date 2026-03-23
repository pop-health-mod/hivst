
library(dplyr)
library(tidyr)

fit <- readRDS("hivst_stan_fit_sep2.rds") 

# Region mapping
country_to_region_cnt <- c(
  burundi = "Eastern",
  kenya = "Eastern",
  madagascar = "Eastern",
  malawi = "Eastern",
  mozambique = "Eastern",
  rwanda = "Eastern",
  tanzania = "Eastern",
  uganda = "Eastern",
  zambia = "Eastern",
  drc = "Central",
  
  botswana = "Southern",
  eswatini = "Southern",
  lesotho = "Southern",
  namibia = "Southern",
  southafrica = "Southern",
  zimbabwe = "Southern",
  
  benin = "Western",
  burkinafaso = "Western",
  cameroon = "Western",
  cotedivoire = "Western",
  ghana = "Western",
  guinea = "Western",
  guineabissau = "Western",
  liberia = "Western",
  mali = "Western",
  senegal = "Western",
  sierraleone = "Western"
)

is_ESA <- function(region) region %in% c("Eastern", "Southern")
is_WCA <- function(region) region %in% c("Western", "Central")

# posterior draws (survey and program data)
post_prog  <- rstan::extract(fit, pars = "hivst_prd")$hivst_prd
post_svy_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
post_svy_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

# PROGRAM DATA VALIDATION

# observed program data
program_obs_df <- do.call(rbind, lapply(seq_along(cnt_data), function(c) {
  data.frame(
    country = names(cnt_data)[c],
    country_id = c,
    obs_index_within_country = seq_along(cnt_data[[c]]$ind_hts),
    time_index = cnt_data[[c]]$ind_hts,
    year = cnt_data[[c]]$yr_hts,
    observed = cnt_data[[c]]$hts_dat
  )
}))

# time_index
program_validation_df <- do.call(rbind, lapply(seq_len(nrow(program_obs_df)), function(i) {
  c_id <- program_obs_df$country_id[i]
  t_id <- program_obs_df$time_index[i] + 1
  
  pred_draws <- post_prog[, c_id, t_id]
  
  data.frame(
    country = program_obs_df$country[i],
    country_id = c_id,
    year = program_obs_df$year[i],
    time_index = t_id,
    observed = program_obs_df$observed[i],
    pred_median = median(pred_draws),
    pred_lower = quantile(pred_draws, 0.025),
    pred_upper = quantile(pred_draws, 0.975)
  )
}))

program_validation_df <- program_validation_df %>%
  mutate(
    error = pred_median - observed,
    abs_error = abs(error),
    percent_error = 100 * error / observed,
    absolute_percent_error = 100 * abs_error / observed,
    below_lower_CrI = observed < pred_lower,
    above_upper_CrI = observed > pred_upper
  )

program_validation_summary <- program_validation_df %>%
  summarise(
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Median_percent_error = median(percent_error, na.rm = TRUE),
    Median_absolute_percent_error = median(absolute_percent_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE)
  )

print(as.data.frame(program_validation_summary), row.names = FALSE)

# region-level program validation
program_validation_region_df <- program_validation_df %>%
  mutate(
    gbd_region = country_to_region_cnt[country],
    Region = case_when(
      is_ESA(gbd_region) ~ "ESA",
      is_WCA(gbd_region) ~ "WCA",
      TRUE ~ NA_character_
    )
  )

program_validation_by_region <- program_validation_region_df %>%
  group_by(Region) %>%
  summarise(
    N = n(),
    Median_percent_error = median(percent_error, na.rm = TRUE),
    Median_absolute_percent_error = median(absolute_percent_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE),
    .groups = "drop"
  )

program_validation_overall <- program_validation_region_df %>%
  summarise(
    Region = "Overall",
    N = n(),
    Median_percent_error = median(percent_error, na.rm = TRUE),
    Median_absolute_percent_error = median(absolute_percent_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE)
  )

program_validation_table <- bind_rows(
  program_validation_by_region,
  program_validation_overall
)

print(program_validation_table, width = Inf)

# SURVEY DATA VALIDATION

age_labels <- c("15-24", "25-34", "35-49", "50+")
survey_obs_list <- list()

for (c in seq_along(cnt_data)) {
  dat <- cnt_data[[c]]
  n_svy <- length(dat$ind_svy)
  
  for (s in seq_len(n_svy)) {
    for (a in 1:4) {
      
      if (dat$den_svy_m[s, a] >= 0) {
        survey_obs_list[[length(survey_obs_list) + 1]] <- data.frame(
          country = names(cnt_data)[c],
          country_id = c,
          survey_id_within_country = s,
          year = dat$yr_svy[s],
          time_index = dat$ind_svy[s],
          sex = "Male",
          age_group = age_labels[a],
          age_id = a,
          observed = dat$num_svy_m[s, a] / dat$den_svy_m[s, a],
          numerator = dat$num_svy_m[s, a],
          denominator = dat$den_svy_m[s, a]
        )
      }
      
      if (dat$den_svy_f[s, a] >= 0) {
        survey_obs_list[[length(survey_obs_list) + 1]] <- data.frame(
          country = names(cnt_data)[c],
          country_id = c,
          survey_id_within_country = s,
          year = dat$yr_svy[s],
          time_index = dat$ind_svy[s],
          sex = "Female",
          age_group = age_labels[a],
          age_id = a,
          observed = dat$num_svy_f[s, a] / dat$den_svy_f[s, a],
          numerator = dat$num_svy_f[s, a],
          denominator = dat$den_svy_f[s, a]
        )
      }
    }
  }
}

survey_obs_df <- bind_rows(survey_obs_list)

# corrected extraction: use time_index + 1
survey_validation_df <- do.call(rbind, lapply(seq_len(nrow(survey_obs_df)), function(i) {
  c_id <- survey_obs_df$country_id[i]
  t_id <- survey_obs_df$time_index[i] + 1
  a_id <- survey_obs_df$age_id[i]
  sx   <- survey_obs_df$sex[i]
  
  if (sx == "Male") {
    pred_draws <- post_svy_m[, c_id, t_id, a_id]
  } else {
    pred_draws <- post_svy_f[, c_id, t_id, a_id]
  }
  
  data.frame(
    country = survey_obs_df$country[i],
    country_id = c_id,
    year = survey_obs_df$year[i],
    sex = sx,
    age_group = survey_obs_df$age_group[i],
    observed = survey_obs_df$observed[i],
    pred_median = median(pred_draws),
    pred_lower = quantile(pred_draws, 0.025),
    pred_upper = quantile(pred_draws, 0.975)
  )
}))

survey_validation_df <- survey_validation_df %>%
  mutate(
    observed_pct = observed * 100,
    pred_median_pct = pred_median * 100,
    pred_lower_pct = pred_lower * 100,
    pred_upper_pct = pred_upper * 100,
    error = pred_median_pct - observed_pct,
    abs_error = abs(error),
    below_lower_CrI = observed_pct < pred_lower_pct,
    above_upper_CrI = observed_pct > pred_upper_pct
  )

survey_validation_summary <- survey_validation_df %>%
  summarise(
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE)
  )

print(as.data.frame(survey_validation_summary), row.names = FALSE)

# region-level survey validation
survey_validation_region_df <- survey_validation_df %>%
  mutate(
    gbd_region = country_to_region_cnt[country],
    Region = case_when(
      is_ESA(gbd_region) ~ "ESA",
      is_WCA(gbd_region) ~ "WCA",
      TRUE ~ NA_character_
    )
  )

survey_validation_by_region <- survey_validation_region_df %>%
  group_by(Region) %>%
  summarise(
    N = n(),
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE),
    .groups = "drop"
  )

survey_validation_overall <- survey_validation_region_df %>%
  summarise(
    Region = "Overall",
    N = n(),
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE)
  )

survey_validation_table <- bind_rows(
  survey_validation_by_region,
  survey_validation_overall
)

print(survey_validation_table, width = Inf)

# excludng Kenya, Madagascar, Mozambique from survey validation
exclude_ctry <- c("kenya", "madagascar", "mozambique")

survey_validation_region_df_excl <- survey_validation_region_df %>%
  filter(!country %in% exclude_ctry)

survey_validation_by_region_excl <- survey_validation_region_df_excl %>%
  group_by(Region) %>%
  summarise(
    N = n(),
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE),
    .groups = "drop"
  )

survey_validation_overall_excl <- survey_validation_region_df_excl %>%
  summarise(
    Region = "Overall",
    N = n(),
    Median_error = median(error, na.rm = TRUE),
    Median_absolute_error = median(abs_error, na.rm = TRUE),
    Percent_below_lower_CrI = 100 * mean(below_lower_CrI, na.rm = TRUE),
    Percent_above_upper_CrI = 100 * mean(above_upper_CrI, na.rm = TRUE)
  )

survey_validation_table_excl <- bind_rows(
  survey_validation_by_region_excl,
  survey_validation_overall_excl
)

print(survey_validation_table_excl, width = Inf)