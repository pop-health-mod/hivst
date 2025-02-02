# fitting for more than 1 country, age stratification

rm(list = ls())
gc()

library(wpp2024)
library(rstan)
library(ggplot2)
library(ggsci)

# male and female pop
data(popM1)
data(popF1)
# mortality data
data(mxM1)   
data(mxF1)  

#---pop at the beginning of the year (as wpp reports mid year pop)----
countries <- c("Kenya", "Ghana")
age_grp <- list("15-24" = 16:25, "25-34" = 26:35, "35-49" = 36:50, "50+"   = 51:101)
# matrix with 4 age groups rows for each country, c1=Male, c2=Female
pop_agegrp_fn <- function(country, popM, popF, age_groups) {
  wpp_m <- popM[popM$name == country, !(colnames(popM) %in% as.character(1949:2009))]
  wpp_f <- popF[popF$name == country, !(colnames(popF) %in% as.character(1949:2009))]
  
  pop <- matrix(0, nrow = length(age_groups), ncol = 2, 
                dimnames = list(names(age_groups), c("Male", "Female")))
  
  for (g in names(age_groups)) {
    pop_2010_m <- sum(wpp_m[ age_groups[[g]], "2010" ]) * 1000
    pop_2011_m <- sum(wpp_m[ age_groups[[g]], "2011" ]) * 1000
    pop[g, "Male"] <- (pop_2010_m + pop_2011_m) / 2
    
    pop_2010_f <- sum(wpp_f[ age_groups[[g]], "2010" ]) * 1000
    pop_2011_f <- sum(wpp_f[ age_groups[[g]], "2011" ]) * 1000
    pop[g, "Female"] <- (pop_2010_f + pop_2011_f) / 2
  }
  
  return(pop)
}

pop <- lapply(countries, function(ctry) {
  pop_agegrp_fn(country = ctry, popM = popM1, popF = popF1, age_groups = age_grp)
})

pop_all_combined <- do.call(rbind, pop)

# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt) + 1
niter <- (end - start) / dt
n_yr <- end - start

# mapping hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}

yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)

#--entry rates-----
# male
get_entry_rates_m <- function(cn, start, end) {
  wpp_m <- popM1[popM1$name == cn, !(colnames(popM1) %in% as.character(1949:2009))]
  entry_rates_m <- numeric(end - start + 1)
  for (t in start:end) {
    if (t == 2024) {
      numerator <- wpp_m[(15 + 1), "2023"] * 1000
      denominator <- sum(wpp_m[(15 + 1):(100 + 1), "2023"]) * 1000
    } else {
      numerator <- wpp_m[(15 + 1), as.character(t)] * 1000
      denominator <- sum(wpp_m[(15 + 1):(100 + 1), as.character(t)]) * 1000
    }
    entry_rates_m[t - start + 1] <- numerator / denominator
  }
  return(data.frame(Year = start:end, EntryRate_m = entry_rates_m))
}

# matrix [r:years,c:country]  
entry_m_vec <- do.call(cbind, lapply(countries, 
                      function(cn) get_entry_rates_m(cn, start, end)$EntryRate_m[2:14])) # 2011 not needed

# female
get_entry_rates_f <- function(cn_f, start, end) {
  wpp_f <- popF1[popF1$name == cn_f, !(colnames(popF1) %in% as.character(1949:2009))]
  entry_rates_f <- numeric(end - start + 1)
  for (t in start:end) {
    if (t == 2024) {
      numerator <- wpp_f[(15 + 1), "2023"] * 1000
      denominator <- sum(wpp_f[(15 + 1):(100 + 1), "2023"]) * 1000
    } else {
      numerator <- wpp_f[(15 + 1), as.character(t)] * 1000
      denominator <- sum(wpp_f[(15 + 1):(100 + 1), as.character(t)]) * 1000
    }
    entry_rates_f[t - start + 1] <- numerator / denominator
  }
  return(data.frame(Year = start:end, EntryRate_f = entry_rates_f))
}

# matrix [r:years,c:country]  
entry_f_vec <- do.call(cbind, lapply(countries, 
              function(cn_f) get_entry_rates_f(cn_f, start, end)$EntryRate_f[2:14])) # 2011 not needed


#--- mortality rate: 4 age groups: 15–24, 25–34, 35–49, 50+ ---
# male mortality rate (with age group)
calc_mort_agegrp_m <- function(year_int, wpp_popM, mx_male, age_grp) {
  sapply(seq_along(age_grp), function(g) {
    age_rows <- age_grp[[g]]
    # pop
    pop_vec <- if (year_int == 2024) {
      wpp_popM[age_rows, "2023"] * 1000
    } else {
      wpp_popM[age_rows, as.character(year_int)] * 1000
    }
    # Mortality
    mx_vec <- if (year_int == 2024) {
      mx_male[age_rows, "2024"]
    } else {
      mx_male[age_rows, as.character(year_int)]
    }

    sum(pop_vec * mx_vec) / sum(pop_vec)
  })
}

# function to loop over start to end for one country
mort_rate_m_agegrp <- function(cn, start_yr, end_yr, age_grp) {
  wpp_m_c <- popM1[
    popM1$name == cn, 
    !(colnames(popM1) %in% as.character(1949:2009))
  ]
  mx_male_c <- mxM1[
    mxM1$name == cn, 
    c("name", "age", as.character(2010:2024))
  ]
  # mortality for each year from start_yr to end_yr
  mort_mat <- t(sapply(seq(start_yr, end_yr), function(yr) {
    calc_mort_agegrp_m(yr, wpp_m_c, mx_male_c, age_grp)
  }))
  dimnames(mort_mat) <- list(seq(start_yr, end_yr), names(age_grp))
  return(mort_mat)
}

mort_mat_m <- lapply(countries, function(cn) {
  mat <- mort_rate_m_agegrp(cn, start, end, age_grp)
  # removing the row for 2024 as model stops at 2023
  mat[1:(end - start), ]
})


# female mortality rate (with age group)
calc_mort_agegrp_f <- function(year_int, wpp_popF, mx_female, age_grp) {
  sapply(seq_along(age_grp), function(g) {
    age_rows <- age_grp[[g]]
    # pop
    pop_vec <- if (year_int == 2024) {
      wpp_popF[age_rows, "2023"] * 1000
    } else {
      wpp_popF[age_rows, as.character(year_int)] * 1000
    }
    # Mortality
    mx_vec <- if (year_int == 2024) {
      mx_female[age_rows, "2024"]
    } else {
      mx_female[age_rows, as.character(year_int)]
    }
    
    sum(pop_vec * mx_vec) / sum(pop_vec)
  })
}

# function to loop over start to end for one country
mort_rate_f_agegrp <- function(cn, start_yr, end_yr, age_grp) {
  wpp_f_c <- popF1[
    popF1$name == cn, 
    !(colnames(popF1) %in% as.character(1949:2009))
  ]
  mx_female_c <- mxF1[
    mxF1$name == cn, 
    c("name", "age", as.character(2010:2024))
  ]
  # mortality for each year from start_yr to end_yr
  mort_mat <- t(sapply(seq(start_yr, end_yr), function(yr) {
    calc_mort_agegrp_f(yr, wpp_f_c, mx_female_c, age_grp)
  }))
  dimnames(mort_mat) <- list(seq(start_yr, end_yr), names(age_grp))
  return(mort_mat)
}

mort_mat_f <- lapply(countries, function(cn) {
  mat <- mort_rate_f_agegrp(cn, start, end, age_grp)
  # removing the row for 2024 as model stops at 2023
  mat[1:(end - start), ]
})

##########--not required--########
# mort_list_f <- lapply(countries, function(cn) {
#   mort_rate_f_agegrp(cn, start, end, age_grp)
# })
# 
# # r=years, c = age groups for each country side by side (so in stan I need to say choose 1:4 and then 5:8 for each country)
# mort_mat_f <- do.call(cbind, mort_list_f)
# mort_mat_f <- mort_mat_f[1:13, ] # removing 2024
##########--not required--########

#--- aging rate alpha ----
# aging is same for first 2 age groups (1/10), for 3rd group it is 1/15
alpha <- c(1/10, 1/10, 1/15)


#-------stan---------
hivst_mod <- '
functions {
  real[,,,] hivst_fun(
    int niter,
    vector beta_t_dt,
    real beta_retest,
    real beta_male,
    vector beta_age, // RR for 4 age groups
    matrix pop, // matrix r age c sex
    real dt,
    vector entry_m_dt, // vector as before
    vector entry_f_dt, // vector as before
    matrix mort_m_dt, // matrix, earlier it was vector
    matrix mort_f_dt, // matrix, earlier it was vector
    vector alpha // aging rate for 4 age groups
  ) {
  
    // in the multi-country version, the inputs are already on their transformed scale.
    vector[niter] rr_t = beta_t_dt;
    real rr_r = beta_retest;
    real rr_m = beta_male;
    vector[4] rr_a = beta_age; // RR for 4 age groups

    real out[niter, 4, 2, 4]; // niter, model outputs, sex, 4 age groups
    out[, , 1, ] = rep_array(0.0, niter, 4, 4); // adding age grp dimension similarly
    out[, , 2, ] = rep_array(0.0, niter, 4, 4); // adding age grp dimension similarly
    
    // Initialization
    out[1,1,1,1] = pop[1, 1]; // niter=1, never tested, men, age group 15-24
    out[1,1,1,2] = pop[2, 1]; // niter=1, never tested, men, age group 25-34
    out[1,1,1,3] = pop[3, 1]; // niter=1, never tested, men, age group 35-49
    out[1,1,1,4] = pop[4, 1]; // niter=1, never tested, men, age group 50+
    out[1,1,2,1] = pop[1, 2]; // niter=1, never tested, women, age group 15-24
    out[1,1,2,2] = pop[2, 2]; // niter=1, never tested, women, age group 25-34
    out[1,1,2,3] = pop[3, 2]; // niter=1, never tested, women, age group 35-49
    out[1,1,2,4] = pop[4, 2]; // niter=1, never tested, women, age group 50+
    
    for (i in 2:niter) {
  // men, age group 1
  out[i,1,1,1] = out[i-1,1,1,1] + dt * (+ entry_m_dt[i] * (sum(out[i-1,1,1, ]) + sum(out[i-1,2,1, ]))
                                        - rr_t[i] * rr_m * rr_a[1] * out[i-1,1,1,1] 
                                        - (alpha[1] + mort_m_dt[i,1]) * out[i-1,1,1,1]);
  out[i,2,1,1] = out[i-1,2,1,1] + dt * (+ rr_t[i] * rr_m * rr_a[1] * out[i-1,1,1,1] 
                                        - (alpha[1] + mort_m_dt[i,1]) *out[i-1,2,1,1]);
  out[i,3,1,1] = rr_t[i] * rr_m * rr_a[1] * (out[i-1,1,1,1] + rr_r * out[i-1,2,1,1]);
  out[i,4,1,1] = out[i,2,1,1] / (out[i,1,1,1] + out[i,2,1,1]);
  
  // Males, age group 2-3
  out[i,1,1,2] = out[i-1,1,1,2] + dt * (+ alpha[1] * out[i-1,1,1,1] 
                                        - rr_t[i] * rr_m * rr_a[2] * out[i-1,1,1,2] 
                                        - (alpha[2] + mort_m_dt[i,2]) * out[i-1,1,1,2]);
  out[i,2,1,2] = out[i-1,2,1,2] + dt * (+ alpha[1] * out[i-1,2,1,1]
                                        + rr_t[i] * rr_m * rr_a[2] * out[i-1,1,1,2] 
                                        - (alpha[2] + mort_m_dt[i,2]) * out[i-1,2,1,2]);
  out[i,3,1,2] = rr_t[i] * rr_m * rr_a[2] * (out[i-1,1,1,2] + rr_r * out[i-1,2,1,2]);
  out[i,4,1,2] = out[i,2,1,2] / (out[i,1,1,2] + out[i,2,1,2]);
  
  // Males, age group 3
  out[i,1,1,3] = out[i-1,1,1,3] + dt * (+ alpha[2] * out[i-1,1,1,2] 
                                        - rr_t[i] * rr_m * rr_a[3] * out[i-1,1,1,3] 
                                        - (alpha[3] + mort_m_dt[i,3]) * out[i-1,1,1,3]);
  out[i,2,1,3] = out[i-1,2,1,3] + dt * (+ alpha[2] * out[i-1,2,1,2] 
                                        + rr_t[i] * rr_m * rr_a[3] * out[i-1,1,1,3] 
                                        - (alpha[3] + mort_m_dt[i,3]) * out[i-1,2,1,3]);
  out[i,3,1,3] = rr_t[i] * rr_m * rr_a[3] * (out[i-1,1,1,3] + rr_r * out[i-1,2,1,3]);
  out[i,4,1,3] = out[i,2,1,3] / (out[i,1,1,3] + out[i,2,1,3]);

 // Males, age group 4
  out[i,1,1,4] = out[i-1,1,1,4] + dt * (+ alpha[3] * out[i-1,1,1,3] 
                                        - rr_t[i] * rr_m * rr_a[4] * out[i-1,1,1,4] 
                                        - mort_m_dt[i,4] * out[i-1,1,1,4]);
  out[i,2,1,4] = out[i-1,2,1,4] + dt * (+ alpha[3] * out[i-1,2,1,3] 
                                        + rr_t[i] * rr_m * rr_a[4] * out[i-1,1,1,4] 
                                        - mort_m_dt[i,4] * out[i-1,2,1,4]);
  out[i,3,1,4] = rr_t[i] * rr_m * rr_a[4] * (out[i-1,1,1,4] + rr_r * out[i-1,2,1,4]);
  out[i,4,1,4] = out[i,2,1,4] / (out[i,1,1,4] + out[i,2,1,4]);
  
  // Females, age group 1
  out[i,1,2,1] = out[i-1,1,2,1] + dt * (+ entry_f_dt[i] * (sum(out[i-1,1,2, ]) + sum(out[i-1,2,2,])) 
                                        - rr_t[i] * rr_a[1] * out[i-1,1,2,1] 
                                        - (alpha[1] + mort_f_dt[i,1]) * out[i-1,1,2,1]);
  out[i,2,2,1] = out[i-1,2,2,1] + dt * (+ rr_t[i] * rr_a[1] * out[i-1,1,2,1] 
                                        - (alpha[1] + mort_f_dt[i,1]) * out[i-1,2,2,1]);
  out[i,3,2,1] = rr_t[i] * rr_a[1] * (out[i-1,1,2,1] + rr_r * out[i-1,2,2,1]);
  out[i,4,2,1] = out[i,2,2,1] / (out[i,1,2,1] + out[i,2,2,1]);
  
  // Females, age group 2
  out[i,1,2,2] = out[i-1,1,2,2] + dt * (+ alpha[1] * out[i-1,1,2,1] 
                                        - rr_t[i] * rr_a[2] * out[i-1,1,2,2] 
                                        - (alpha[2] + mort_f_dt[i,2]) * out[i-1,1,2,2]);
  out[i,2,2,2] = out[i-1,2,2,2] + dt * (+ alpha[1] * out[i-1,2,2,1] 
                                        + rr_t[i] * rr_a[2] * out[i-1,1,2,2] 
                                        - (alpha[2] + mort_f_dt[i,2]) * out[i-1,2,2,2]);
  out[i,3,2,2] = rr_t[i] * rr_a[2] * (out[i-1,1,2,2] + rr_r * out[i-1,2,2,2]);
  out[i,4,2,2] = out[i,2,2,2] / (out[i,1,2,2] + out[i,2,2,2]);
  
  // Females, age group 3
  out[i,1,2,3] = out[i-1,1,2,3] + dt * (+ alpha[2] * out[i-1,1,2,2] 
                                        - rr_t[i] * rr_a[3] * out[i-1,1,2,3] 
                                        - (alpha[3] + mort_f_dt[i,3]) * out[i-1,1,2,3]);
  out[i,2,2,3] = out[i-1,2,2,3] + dt * (+ alpha[2] * out[i-1,2,2,2] 
                                        + rr_t[i] * rr_a[3] * out[i-1,1,2,3] 
                                        - (alpha[3] + mort_f_dt[i,3]) * out[i-1,2,2,3]);
  out[i,3,2,3] = rr_t[i] * rr_a[3] * (out[i-1,1,2,3] + rr_r * out[i-1,2,2,3]);
  out[i,4,2,3] = out[i,2,2,3] / (out[i,1,2,3] + out[i,2,2,3]);
  
  // Females, age group 4
  out[i,1,2,4] = out[i-1,1,2,4] + dt * (+ alpha[3] * out[i-1,1,2,3] 
                                        - rr_t[i] * rr_a[4] * out[i-1,1,2,4] 
                                        - mort_f_dt[i,4] * out[i-1,1,2,4]);
  out[i,2,2,4] = out[i-1,2,2,4] + dt * (+ alpha[3] * out[i-1,2,2,3] 
                                        + rr_t[i] * rr_a[4] * out[i-1,1,2,4] 
                                        - mort_f_dt[i,4] * out[i-1,2,2,4]);
  out[i,3,2,4] = rr_t[i] * rr_a[4] * (out[i-1,1,2,4] + rr_r * out[i-1,2,2,4]);
  out[i,4,2,4] = out[i,2,2,4] / (out[i,1,2,4] + out[i,2,2,4]);
    }
return out;
  }
}

data {
  int<lower = 1> n_cnt;           // number of countries
  int<lower = 1> n_yr;            // number of years to model
  int<lower = 1> niter;           // number of iteration (a function of dt and n_yr)
  int<lower = 1> tot_svy;         // number of surveys accross all countries
  int<lower = 1> tot_hts;         // number of years with hivst data all countries
  int<lower = 1> yr_ind[niter];
  real dt;
  matrix[n_cnt * 4, 2] pop; // array of matrices, each matrix pop of that country, r age c sex 
  int<lower = 1> idx_pop[n_cnt];
  
  // for prgrm data
  int<lower = 1> n_hts_by_cnt[n_cnt]; // number of years with observed HIVST data per country
  int<lower = 1> ind_hts[tot_hts];    // indices pgm data
  int<lower = 1> hivst[tot_hts];      // number of HIVST performed per year per country
  real se_hts[tot_hts];               // se for program data
  
  // for survey data
  int<lower = 1> n_svy_by_cnt[n_cnt];    // number of surveys per country
  int<lower = 1> ind_svy[tot_svy];       // indices for survey data
  int<lower = -999> num_svy1[tot_svy, 2]; 
  int<lower = -999> num_svy2[tot_svy, 2]; 
  int<lower = -999> num_svy3[tot_svy, 2]; 
  int<lower = -999> num_svy4[tot_svy, 2]; 
  int<lower = -999> den_svy1[tot_svy, 2]; 
  int<lower = -999> den_svy2[tot_svy, 2];
  int<lower = -999> den_svy3[tot_svy, 2];
  int<lower = -999> den_svy4[tot_svy, 2];  

  // indices for the unlist data
  int<lower = 1> svy_idx_s[n_cnt];    // starting index (_s) for age groups (15-24, 25-34, 35-49)
  int<lower = 1> svy_idx_e[n_cnt];    // ending index (_e) for age groups (15-24, 25-34, 35-49)
  int<lower = 0> cnt_no_age50[n_cnt]; // indicator for country without any survey on age 50+

  int<lower = 1> hts_idx_s[n_cnt];
  int<lower = 1> hts_idx_e[n_cnt];

  // entry and exit rates for open pop multiple countries, age stratified
  matrix[n_yr, n_cnt] entry_m;
  matrix[n_yr, n_cnt] entry_f;
  real mort_m[n_cnt, n_yr, 4]; // array of matrices, each matrix mortality rates of that country, r year c age groups   
  real mort_f[n_cnt, n_yr, 4]; // array of matrices, each matrix mortality rates of that country, r year c age groups
  
  // aging rate
  vector[3] alpha;
}

parameters {
  // testing rate
  matrix<upper = -1>[n_cnt, n_yr] beta_t;  // yearly HIVST testing rates (rw1) for each country
  
  // standard deviations
  real<lower = 1e-6, upper = 5> sd_rw;     // sd of the rw1 for beta_t
  real<lower = 1e-6, upper = 5> sd_phi;    // sd of the RE for phi
  real<lower = 1e-6, upper = 5> sd_rt;     // sd of the RE for the re-testing rate ratio
  real<lower = 1e-6, upper = 5> sd_male;   // sd of the RE of the male rate ratio
  real<lower = 1e-6, upper = 5> sd_age;    // sd for RR age
  
  // retesting parameters
  real beta_retest_overall;            // overall shared re-testing rate
  vector[n_cnt] beta_rt_raw;           // country-specific re-testing rates
  
  // male rate ratio parameters
  real beta_male_overall;              //overall male relative rate of HIVST
  vector[n_cnt] beta_male_raw;       // country specific male relative rate of HIVST  
  
  // phi parameters
  real phi_overall;           // overall proportion of HIVST kits used
  vector[n_cnt] phi_raw;      // country-specific proportions of HIVST kits used
  
  // age rate ratio parameters
  vector[3] beta_age_overall;       // overall RR age
  vector[n_cnt] beta_age_raw1;
  vector[n_cnt] beta_age_raw2;
  vector[n_cnt] beta_age_raw3;
}


transformed parameters {
// non-centered parameterization
  vector[n_cnt] beta_retest;
  vector[n_cnt] beta_male;
  vector[n_cnt] phi;
  matrix[n_cnt, 4] beta_age;
  
  beta_retest = 0.5 + (2.5 - 0.5) * inv_logit(beta_retest_overall + sd_rt * beta_rt_raw);
  beta_male = exp(beta_male_overall + sd_male * beta_male_raw);
  phi = 0.5 + (1 - 0.5) * inv_logit(phi_overall + sd_phi * phi_raw);
  beta_age[, 1] = rep_vector(1, n_cnt);  // fixed for age group 1
  beta_age[, 2] = exp(beta_age_overall[1] + sd_age * beta_age_raw1);
  beta_age[, 3] = exp(beta_age_overall[2] + sd_age * beta_age_raw2);
  beta_age[, 4] = exp(beta_age_overall[3] + sd_age * beta_age_raw3);
    
  // entry and death rates
  matrix[n_cnt, niter] beta_t_dt;
  matrix[n_cnt, niter] entry_m_dt;
  matrix[n_cnt, niter] entry_f_dt;
  real mort_m_dt[n_cnt, niter, 4]; // array for the men mortality rate (country, iteration, age)
  real mort_f_dt[n_cnt, niter, 4]; // array for the women mortality rate (country, iteration, age)
    
  for (c in 1:n_cnt) {
    for (i in 1:niter) {
      int year = yr_ind[i];
      beta_t_dt[c, i] = exp(beta_t[c, year]);
      entry_m_dt[c, i] = entry_m[year, c]; 
      entry_f_dt[c, i] = entry_f[year, c];
      for (a in 1:4) {
        mort_m_dt[c, i, a] = mort_m[c, year, a];
        mort_f_dt[c, i, a] = mort_f[c, year, a];
     }
    }
   }
 }

model {
  // Priors
  // Overall prior for SDs
  sd_rw ~ normal(0, 0.25) T[1e-6, 5];
  sd_phi ~ normal(0, 0.5) T[1e-6, 5];
  sd_rt ~ normal(0, 0.5) T[1e-6, 5];
  sd_male ~ normal(0, 0.5) T[1e-6, 5];
  sd_age ~ normal(0, 0.5) T[1e-6, 5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(logit(1.2 - 0.5) / (2.5 - 0.5), 0.5); // 0.5 + (2.5 - 0.5) * plogis(qlogis((1.2 - 0.5) / (2.5 - 0.5)) + c(-1, 1) * qnorm(0.975) * 0.5)
  // overall prior for the proportion of tests distributed being used
  phi_overall ~ normal(logit((0.85 - 0.5) / (1 - 0.5)), 1); // 0.5 + (1 - 0.5) * plogis(qlogis((0.85 - 0.5) / (1-0.5)) + c(-1, 1) * qnorm(0.975) * 1)
  // overall prior for male rate ratio
  beta_male_overall ~ normal(log(1), 0.5);
  // overall prior for age rate ratio
  beta_age_overall ~ normal(log(1), 0.25); // exp(log(1 + c(-1,1) * qnorm(0.975) * 0.25))
  
  // for non-centered parameterization
  beta_rt_raw ~ std_normal();
  beta_male_raw ~ std_normal();
  phi_raw ~ std_normal();
  beta_age_raw1 ~ std_normal();
  beta_age_raw2 ~ std_normal();
  beta_age_raw3 ~ std_normal();
    
  // country-specific priors
  for (c in 1:n_cnt) {
    beta_t[c, 1] ~ normal(-10, 1); // exp(-10 + c(-1, 1) * 1.96 * 1);
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);

  // formatting pop as matrix
  matrix[4, 2] pop_mat; // we create a new matrix to put in the population information
  pop_mat[, 1] = pop[idx_pop[c]:(idx_pop[c] + 3), 1]; // men
  pop_mat[, 2] = pop[idx_pop[c]:(idx_pop[c] + 3), 2]; // women
  matrix[niter, 4] mort_m_dt_mat;
  matrix[niter, 4] mort_f_dt_mat;
  for (a in 1:4) {
    mort_m_dt_mat[, a] = to_vector(mort_m_dt[c, , a]);
    mort_f_dt_mat[, a] = to_vector(mort_f_dt[c, , a]);    
  }
  // calling hivst_fun
  real model_pred[niter, 4, 2, 4] = hivst_fun(
    niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], to_vector(beta_age[c, ]), pop_mat, 
    dt, to_vector(entry_m_dt[c, ]), to_vector(entry_f_dt[c, ]), 
    mort_m_dt_mat, mort_f_dt_mat, alpha);
    
  // fitting to survey data (layer of country and surveys)
  // 15-24 (a = 1)
   num_svy1[svy_idx_s[c]:svy_idx_e[c], 1] ~ binomial(den_svy1[svy_idx_s[c]:svy_idx_e[c], 1], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 1, 1]);
   num_svy1[svy_idx_s[c]:svy_idx_e[c], 2] ~ binomial(den_svy1[svy_idx_s[c]:svy_idx_e[c], 2], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 2, 1]);
   // 25-34 (a = 2)                                           
   num_svy2[svy_idx_s[c]:svy_idx_e[c], 1] ~ binomial(den_svy2[svy_idx_s[c]:svy_idx_e[c], 1], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 1, 2]);
   num_svy2[svy_idx_s[c]:svy_idx_e[c], 2] ~ binomial(den_svy2[svy_idx_s[c]:svy_idx_e[c], 2], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 2, 2]);
   // 35-49 (a = 3)                                           
   num_svy3[svy_idx_s[c]:svy_idx_e[c], 1] ~ binomial(den_svy3[svy_idx_s[c]:svy_idx_e[c], 1], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 1, 3]);
   num_svy3[svy_idx_s[c]:svy_idx_e[c], 2] ~ binomial(den_svy3[svy_idx_s[c]:svy_idx_e[c], 2], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 2, 3]);
   // 50+ (a = 4) 
   if (cnt_no_age50[c] == 1) {
      for (s in 1:n_svy_by_cnt[c]) {
          if (num_svy4[(svy_idx_s[c] + s - 1), 1] >= 0) {
           num_svy4[(svy_idx_s[c] + s - 1), 1] ~ binomial(den_svy4[(svy_idx_s[c] + s - 1), 1], 
                                              model_pred[ind_svy[(svy_idx_s[c] + s - 1)], 4, 1, 4]);
           num_svy4[(svy_idx_s[c] + s - 1), 2] ~ binomial(den_svy4[(svy_idx_s[c] + s - 1), 2], 
                                              model_pred[ind_svy[(svy_idx_s[c] + s - 1)], 4, 2, 4]);           
        }
      }
    }

  // fitting to program data
    matrix[niter, n_cnt] hts_mod;
    hts_mod[, c] = (+ to_vector(model_pred[, 3, 1, 1])
                              + to_vector(model_pred[, 3, 1, 2])
                              + to_vector(model_pred[, 3, 1, 3])
                              + to_vector(model_pred[, 3, 1, 4])
                          + to_vector(model_pred[, 3, 2, 1])
                              + to_vector(model_pred[, 3, 2, 2])
                              + to_vector(model_pred[, 3, 2, 3])
                              + to_vector(model_pred[, 3, 2, 4])) / phi[c];
    hivst[hts_idx_s[c]:hts_idx_e[c]] ~ normal(hts_mod[ind_hts[hts_idx_s[c]:hts_idx_e[c]], c], 
                                              se_hts[hts_idx_s[c]:hts_idx_e[c]]);
}
}

generated quantities {
  matrix[n_cnt, niter] hivst_prd;    // predicted annual number of HIVST
  real svy_prd_m[n_cnt, niter, 4];  // predicted survey proportions (males) per country
  real svy_prd_f[n_cnt, niter, 4];  // predicted survey proportions (females) per country

  for (c in 1:n_cnt) {
  // formatting pop as matrix
      matrix[4, 2] pop_mat; // we create a new matrix to put in the population information
      pop_mat[, 1] = pop[idx_pop[c]:(idx_pop[c] + 3), 1]; // men
      pop_mat[, 2] = pop[idx_pop[c]:(idx_pop[c] + 3), 2]; // women
      matrix[niter, 4] mort_m_dt_mat;
      matrix[niter, 4] mort_f_dt_mat;
      for (a in 1:4) {
        mort_m_dt_mat[, a] = to_vector(mort_m_dt[c, , a]);
        mort_f_dt_mat[, a] = to_vector(mort_f_dt[c, , a]);    
      }

    real pred[niter, 4, 2, 4] = hivst_fun(
      niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], to_vector(beta_age[c, ]), pop_mat, 
      dt, to_vector(entry_m_dt[c, ]), to_vector(entry_f_dt[c, ]), 
      mort_m_dt_mat, mort_f_dt_mat, alpha);

      svy_prd_m[c, , ] = pred[, 4, 1, ];
      svy_prd_f[c, , ] = pred[, 4, 2, ];
      hivst_prd[c, ] = to_row_vector(+ to_vector(pred[, 3, 1, 1]) 
                                      + to_vector(pred[, 3, 1, 2]) 
                                      + to_vector(pred[, 3, 1, 3]) 
                                      + to_vector(pred[, 3, 1, 4])
                                   + to_vector(pred[, 3, 2, 1]) 
                                      + to_vector(pred[, 3, 2, 2]) 
                                      + to_vector(pred[, 3, 2, 3]) 
                                      + to_vector(pred[, 3, 2, 4]) ) / phi[c];
  }
}
'

# we compile the model and expose the function to invoke it directly in R
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

# survey and program data list 
cnt_data <- list(
  kenya = list(
yr_svy = c(2012.5, 2018.5, 2022.5),
ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt, 
den_svy_f = matrix(
  c(1900, 1983, 1897, 817, # 2012
    2491, 2554, 4338, 1145, # 2018 
    4980, 5037, 4523, -999), # 2022
  nrow = 3, byrow = TRUE),
num_svy_f = matrix(
  c(29, 45, 33, 14, # 2012
    89, 111, 102, 12, # 2018 
    203, 328, 195, -999), # 2022
  nrow = 3, byrow = TRUE),
den_svy_m = matrix(
  c(253, 888, 611, 390, # 2012 
    2245, 1187, 3448, 622,# 2018
    2691, 1181, 2334, -999), # 2022 
  nrow = 3, byrow = TRUE),
num_svy_m = matrix(
  c(10, 31, 18, 7, # 2012
    54, 54, 106, 14, # 2018 
    108, 181, 227, -999), # 2022
  nrow = 3, byrow = TRUE),
yr_hts = c(2018,  2019,   2020,    2021,   2022,  2023) + 0.5,
ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
),

ghana = list(
yr_svy = c(2017.5, 2022.5),
ind_svy = (c(2017.5, 2022.5) - start) / dt,
den_svy_f = matrix(
  c(3303, 1908, 2386, -999, # 2017
    3133, 2389, 2509, -999), # 2022
  nrow = 2, byrow = TRUE),
num_svy_f = matrix(
  c(52, 72, 47, -999, # 2017
    42, 97, 51, -999), # 2022
  nrow = 2, byrow = TRUE),

den_svy_m = matrix(
  c(1927, 645, 856, -999, # 2017 
    1604, 1547, 1222, -999), # 2022 
  nrow = 2, byrow = TRUE),
num_svy_m = matrix(
  c(9, 21, 13, -999, # 2017
    9, 48, 29, -999), # 2022
  nrow = 2, byrow = TRUE),
yr_hts = c(2020,  2021,   2022,  2023) + 0.5,
ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
hts_dat = c(20000, 1323, 235000, 140500),
se_hts = c(20000, 1323, 235000, 140500) * 0.1
)
)

# adding svy_dat,lci,uci after list as the operations cant be performed inside list
cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat_f <- x$num_svy_f / x$den_svy_f
  x$lci_svy_f <- x$svy_dat_f - qnorm(0.975) * sqrt(x$svy_dat_f * (1 - x$svy_dat_f) / x$den_svy_f)
  x$uci_svy_f <- x$svy_dat_f + qnorm(0.975) * sqrt(x$svy_dat_f * (1 - x$svy_dat_f) / x$den_svy_f)
  return(x)
})

cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat_m <- x$num_svy_m / x$den_svy_m
  x$lci_svy_m <- x$svy_dat_m - qnorm(0.975) * sqrt(x$svy_dat_m * (1 - x$svy_dat_m) / x$den_svy_m)
  x$uci_svy_m <- x$svy_dat_m + qnorm(0.975) * sqrt(x$svy_dat_m * (1 - x$svy_dat_m) / x$den_svy_m)
  return(x)
})

# combining svy and pgm data for all countries
# survey by age (combining gender)
num_svy1 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 1], x$num_svy_f[, 1]))) #rows=countries, cols=sex
den_svy1 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 1], x$den_svy_f[, 1])))
num_svy2 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 2], x$num_svy_f[, 2])))
den_svy2 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 2], x$den_svy_f[, 2])))
num_svy3 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 3], x$num_svy_f[, 3])))
den_svy3 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 3], x$den_svy_f[, 3])))
num_svy4 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$num_svy_m[, 4], x$num_svy_f[, 4])))
den_svy4 <- do.call(rbind, lapply(cnt_data, function(x) cbind(x$den_svy_m[, 4], x$den_svy_f[, 4])))
# to check that every missing value (-999) is both in female or male
if (any(!apply(rbind(num_svy4, den_svy4), 1, function(row) all(row > 0) || all(row < 0)))) { 
  print('stop, missing 50+ age inconsistent by sex') }

# the survey
ind_svy <- unlist(lapply(cnt_data, function(x) x$ind_svy))
hts_dat <- unlist(lapply(cnt_data, function(x) x$hts_dat)) #rows=countries, cols=time points
se_hts <- unlist(lapply(cnt_data, function(x) x$se_hts))
ind_hts <- unlist(lapply(cnt_data, function(x) x$ind_hts))

# survey and hts indices
n_cnt <- length(cnt_data)
n_svy_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$yr_svy))) 
n_hts_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$ind_hts)))

# hts
hts_idx_s <- NULL
hts_idx_e <- NULL
hts_idx_s[1] <- 1  
hts_idx_e[1] <- n_hts_by_cnt[1]

# survey
cnt_no_age50 <- unlist(lapply(cnt_data, function(x) ifelse(all(x$num_svy_m[, 4] == -999), 0, 1)))
age_grp_na <- unlist(lapply(cnt_data, function(x) ifelse(x$num_svy_m[, 4] == -999, 0, 1)))

svy_idx_s <- NULL
svy_idx_e <- NULL
svy_idx_s[1] <- 1
svy_idx_e[1] <- n_svy_by_cnt[1]

# remaining countries
for (c in 2:n_cnt) {
  # survey
  svy_idx_s[c] <- svy_idx_s[c - 1] + n_svy_by_cnt[c - 1]
  svy_idx_e[c] <- svy_idx_s[c] + n_svy_by_cnt[c] - 1   
  # hts
  hts_idx_s[c] <- hts_idx_s[c - 1] + n_hts_by_cnt[c - 1]
  hts_idx_e[c] <- hts_idx_s[c] + n_hts_by_cnt[c] - 1
}

idx_pop <- seq(from = 1, to = (n_cnt * 4), by = 4)
# data for fitting and running (I need to update for age grp)
data_stan <- list(
                  n_cnt = length(cnt_data),
                  n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                pop = pop_all_combined,
                idx_pop = idx_pop,
                cnt_no_age50 = cnt_no_age50,
                n_svy_by_cnt = n_svy_by_cnt,                
                n_hts_by_cnt = n_hts_by_cnt, 
                tot_svy = sum(n_svy_by_cnt),
                tot_hts = sum(n_hts_by_cnt),
                ind_svy = ind_svy,                   
                ind_hts = ind_hts,
                svy_idx_s = svy_idx_s,
                hts_idx_s = hts_idx_s,
                svy_idx_e = svy_idx_e,
                hts_idx_e = hts_idx_e,
                  hivst = hts_dat,
                  se_hts = se_hts,
                num_svy1 = num_svy1,                   
                den_svy1 = den_svy1,
                num_svy2 = num_svy2,                   
                den_svy2 = den_svy2,
                num_svy3 = num_svy3,                   
                den_svy3 = den_svy3,
                num_svy4 = num_svy4,                   
                den_svy4 = den_svy4,
                  entry_m = entry_m_vec,
                  entry_f = entry_f_vec,
                  mort_m = mort_mat_m,
                  mort_f = mort_mat_f,
                  alpha = alpha
)
rstan_options(auto_write = TRUE)

# Fitting 
options(mc.cores = parallel::detectCores())
init_function <- function() {
  list(
    beta_t = matrix(rnorm(data_stan$n_cnt * data_stan$n_yr, -5, 0.1), 
                    ncol = data_stan$n_yr, nrow = data_stan$n_cnt),
    sd_rw = runif(1, min = 0.1, max = 1),
    sd_phi = runif(1, min = 0.1, max = 1),
    sd_rt = runif(1, min = 0.1, max = 1),
    sd_men = runif(1, min = 0.1, max = 1),
    sd_age = runif(1, min = 0.1, max = 1),
    beta_restest_overall = rnorm(1, qlogis((1.2 - 0.5) / (2.5 - 0.5)), 0.1),
    beta_rt_raw = rnorm(data_stan$n_cnt, 0, 0.5),
    beta_men_overall = rnorm(1, log(1), 0.1),
    beta_men_raw = rnorm(data_stan$n_cnt, 0, 0.1),
    beta_age_overall = rnorm(3, log(1), 0.1),
    beta_age_raw1 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_raw2 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_raw3 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_phi_overall = rnorm(1, qlogis((0.8 - 0.5) / (1 - 0.5)), 0.1),
    phi_raw = rnorm(data_stan$n_cnt, 0, 0.2)
  )
}
fit <- sampling(hivst_stan, data = data_stan, iter = 1500, chains = 4, init = init_function,
                warmup = 500, thin = 1, control = list(adapt_delta = 0.9))

#summary(fit)

# traceplots
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "sd_phi")
traceplot(fit, pars = "sd_rt")
traceplot(fit, pars = "sd_men")
traceplot(fit, pars = "beta_retest_overall")
traceplot(fit, pars = "beta_rt_raw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_men_overall")
traceplot(fit, pars = "beta_men_raw")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")
traceplot(fit, pars = "beta_age")
traceplot(fit, pars = "phi_overall")
traceplot(fit, pars = "phi_raw")


# testing rate
pars_beta_t <- matrix(paste0("beta_t[", rep(1:data_stan$n_cnt, each = data_stan$n_yr), ",", rep(1:data_stan$n_yr, data_stan$n_cnt), "]"), 
              nrow = data_stan$n_cnt, ncol = data_stan$n_yr, byrow = TRUE)
r <- as.data.frame(rstan::summary(fit, pars = pars_beta_t, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r$`50%`
max(r$`50%`)
max(r$`97.5%`)
 
# ------ retesting rate ratio --------
rr_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_retest_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)

rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr$`50%`
rr$`2.5%`
rr$`97.5%`

# dataframe for forest plot of RR retesting
df_rr_rt <- data.frame(country = names(cnt_data),
                      median = rr$`50%`,
                      lci = rr$`2.5%`,
                      uci = rr$`97.5%`)
df_rr_ov <- rbind(df_rr_rt,
                 data.frame( country = "overall",
                             median = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`),
                             lci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`),
                             uci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)))

# Renaming selected countries
df_rr_ov$country <- as.character(df_rr_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_ov$country[df_rr_ov$country == old_name] <- rename_map[[old_name]]
}

cap_first_letter <- function(x) {
  if (x == "overall") return("Overall")   
  if (nchar(x) == 0)   return(x)      
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
df_rr_ov$country <- sapply(df_rr_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))
df_rr_ov$country     <- factor(df_rr_ov$country, levels = new_levels)
df_rr_ov$style <- ifelse(df_rr_ov$country == "Overall", "pooled", "individual")

rr_retesting_forest <- ggplot(df_rr_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "coral2", "pooled" = "coral4")) +
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Re-testing rate ratio", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  theme(legend.position = "right")
rr_retesting_forest


#-------- rate ratio male ---------------
rr_m_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_male_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m_overall$`50%`)
exp(rr_m_overall$`2.5%`)
exp(rr_m_overall$`97.5%`)

rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_m$`50%`
rr_m$`2.5%`
rr_m$`97.5%`

# dataframe and forest plot for RR male
df_rrm_ <- data.frame(country = names(cnt_data),
                      median = rr_m$`50%`,
                      lci = rr_m$`2.5%`,
                      uci = rr_m$`97.5%`)
df_rr_m <- rbind(df_rrm_,
                 data.frame( country = "overall",
                             median = exp(rr_m_overall$`50%`),
                             lci = exp(rr_m_overall$`2.5%`),
                             uci = exp(rr_m_overall$`97.5%`)))

# Renaming selected countries
df_rr_m$country <- as.character(df_rr_m$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_m$country[df_rr_m$country == old_name] <- rename_map[[old_name]]
}

df_rr_m$country <- sapply(df_rr_m$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_m$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_m$country     <- factor(df_rr_m$country, levels = new_levels)
df_rr_m$style <- ifelse(df_rr_m$country == "Overall", "pooled", "individual")


rr_male_forest <- ggplot(df_rr_m, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "steelblue4", "pooled" = "firebrick4")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio for men", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right")
rr_male_forest


#-------- rate ratio age ---------------
rr_age_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_age_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_age_overall$`50%`)
exp(rr_age_overall$`2.5%`)
exp(rr_age_overall$`97.5%`)

rr_age <- as.data.frame(rstan::summary(fit, pars = c("beta_age"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_age$`50%`
rr_age$`2.5%`
rr_age$`97.5%`

# data.frame and forest plot for RR male
df_rr_a <- NULL
for (i in 1:n_cnt) {
  df_rr_a_i <- data.frame(country = names(cnt_data)[i],
                      age = c("25-34", "35-49", "50+"),
                      median = rr_age$`50%`[grepl(paste(paste0("beta_age\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age))],
                      lci = rr_age$`2.5%`[grepl(paste(paste0("beta_age\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age))],
                      uci = rr_age$`97.5%`[grepl(paste(paste0("beta_age\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age))])
  df_rr_a <- rbind(df_rr_a, df_rr_a_i)
}
df_rr_age <- rbind(df_rr_a,
                 data.frame( age = c("25-34", "35-49", "50+"),
                             country = "overall",
                             median = exp(rr_age_overall$`50%`),
                             lci = exp(rr_age_overall$`2.5%`),
                             uci = exp(rr_age_overall$`97.5%`)))

# Renaming selected countries
df_rr_age$country <- as.character(df_rr_age$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_age$country[df_rr_age$country == old_name] <- rename_map[[old_name]]
}

df_rr_age$country <- sapply(df_rr_age$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_age$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_age$country     <- factor(df_rr_age$country, levels = new_levels)
df_rr_age$style <- ifelse(df_rr_age$country == "Overall", "pooled", "individual")
df_rr_age$age <- factor(df_rr_age$age, levels = c("50+", "35-49", "25-34")) # to match with position.dodge()

rr_age_forest <- ggplot(df_rr_age, aes(x = country, y = median, color = age, size = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("25-34" = "#845699", 
                                "35-49" = "#66BBBB",
                                "50+" = "#D61717E6")) +  # Colors
  scale_size_manual(values = c("individual" = 0.2, "pooled" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio by age (ref: 15-24)", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right") +
  guides(color = guide_legend(reverse = TRUE), size = "none")
rr_age_forest


# ------- phi -----------------
phi_overall <- as.data.frame(rstan::summary(fit, pars = c("phi_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (1 - 0.5) * plogis(phi_overall$`50%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# df and forest plot for phi
df_phi <- data.frame(country = names(cnt_data),
                       median = phi$`50%`,
                       lci = phi$`2.5%`,
                       uci = phi$`97.5%`)
df_phi_ov <- rbind(df_phi,
                  data.frame( country = "overall",
                              median = 0.5 + (1 - 0.5) * plogis(phi_overall$`50%`),
                              lci = 0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`),
                              uci = 0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)))

# Renaming selected countries
df_phi_ov$country <- as.character(df_phi_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_phi_ov$country[df_phi_ov$country == old_name] <- rename_map[[old_name]]
}

df_phi_ov$country <- sapply(df_phi_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_phi_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_phi_ov$country     <- factor(df_phi_ov$country, levels = new_levels)
df_phi_ov$style <- ifelse(df_phi_ov$country == "Overall", "pooled", "individual")


phi_forest <- ggplot(df_phi_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "violetred1", "pooled" = "violetred4")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Proportion of distributed HIVST kits that are used", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2)))
phi_forest



