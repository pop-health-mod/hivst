
length(cnt_data)

test <- NULL
for (i in 1:length(cnt_data)) {
  c_i <- max(cnt_data[[i]]$hts_dat)
  pop_i <- sum(pop[[i]])
  test <- rbind(test, 
                data.frame(cnt = names(cnt_data)[i], prop = c_i / pop_i * 100, 
                           pop = pop_i, old_csnt = c_i * 4))
  
}

test$new_csnt <- ifelse(test$prop < 0.1, 0.1 * test$pop * 4 / 100, test$prop / 100 * test$pop * 4)





# modified constraint for countries with low number of tests
length(cnt_data)
test <- NULL
for (i in 1:length(cnt_data)) {
  c_i <- max(cnt_data[[i]]$hts_dat)
  pop_i <- sum(pop[[i]])
  test <- rbind(test, 
                data.frame(cnt = names(cnt_data)[i], prop = c_i / pop_i * 100, 
                           pop = pop_i, old_csnt = c_i * 4))
  
}
test$new_csnt <- ifelse(test$prop < 0.1, 0.1 * test$pop * 4 / 100, test$prop / 100 * test$pop * 4)

#----dataframe for survey data------
# age groups (column order: 15–24, 25–34, 35–49, 50+)
age_groups <- c("15-24", "25-34", "35-49", "50+")

df_list <- list()
for (country in names(cnt_data)) {
  data <- cnt_data[[country]]
  n_years <- length(data$yr_svy)
  
  # Female data
  for (i in 1:n_years) {
    for (j in 1:4) {
      den <- data$den_svy_f[i, j]
      num <- data$num_svy_f[i, j]
      if (!is.na(den) && den != -999) {
        df_list[[length(df_list) + 1]] <- data.frame(
          country = country,
          year = data$yr_svy[i],
          sex = "female",
          age_group = age_groups[j],
          numerator = num,
          denominator = den
        )
      }
    }
  }
  
  # Male data
  for (i in 1:n_years) {
    for (j in 1:4) {
      den <- data$den_svy_m[i, j]
      num <- data$num_svy_m[i, j]
      if (!is.na(den) && den != -999) {
        df_list[[length(df_list) + 1]] <- data.frame(
          country = country,
          year = data$yr_svy[i],
          sex = "male",
          age_group = age_groups[j],
          numerator = num,
          denominator = den
        )
      }
    }
  }
}

survey_df <- do.call(rbind, df_list)
survey_df$year <- floor(survey_df$year)
survey_df <- survey_df[order(survey_df$country), ]

write.csv(survey_df,
          file = "D:/Downloads/MSc Thesis/hivst/derived survey data/svy_prop_all.csv",
          row.names = FALSE)


