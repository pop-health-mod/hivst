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
countries <- c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
  "Sierra Leone", "Zambia", "Mali", "Uganda",
  "Lesotho", "Mozambique", "Rwanda",
  "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
  "Guinea", "Liberia", "Senegal", "South Africa", 
  "United Republic of Tanzania", "Namibia", "Botswana", 
  "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin")

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
      numerator <- wpp_m[(15), "2023"] * 1000
      denominator <- sum(wpp_m[(15 + 1):(100 + 1), "2023"]) * 1000
    } else {
      numerator <- wpp_m[(15), as.character(t)] * 1000
      denominator <- sum(wpp_m[(15 + 1):(100 + 1), as.character(t - 1)]) * 1000
    }
    entry_rates_m[t - start + 1] <- numerator / denominator
  }
  return(data.frame(Year = start:end, EntryRate_m = entry_rates_m))
}

# matrix [r:years,c:country]  
entry_m_vec <- do.call(cbind, lapply(countries, 
                      function(cn) get_entry_rates_m(cn, start, end)$EntryRate_m[2:14])) # 2011 not needed
entry_m_vec <- -log(1 - entry_m_vec)


# female
get_entry_rates_f <- function(cn_f, start, end) {
  wpp_f <- popF1[popF1$name == cn_f, !(colnames(popF1) %in% as.character(1949:2009))]
  entry_rates_f <- numeric(end - start + 1)
  for (t in start:end) {
    if (t == 2024) {
      numerator <- wpp_f[(15), "2023"] * 1000
      denominator <- sum(wpp_f[(15 + 1):(100 + 1), "2023"]) * 1000
    } else {
      numerator <- wpp_f[(15), as.character(t)] * 1000
      denominator <- sum(wpp_f[(15 + 1):(100 + 1), as.character(t -1)]) * 1000
    }
    entry_rates_f[t - start + 1] <- numerator / denominator
  }
  return(data.frame(Year = start:end, EntryRate_f = entry_rates_f))
}

# matrix [r:years,c:country]  
entry_f_vec <- do.call(cbind, lapply(countries, 
              function(cn_f) get_entry_rates_f(cn_f, start, end)$EntryRate_f[2:14])) # 2011 not needed
entry_f_vec <- -log(1 - entry_f_vec)



#--- mortality rate: 4 age groups: 15–24, 25–34, 35–49, 50+ ---
# male mortality rate (with age group)

calc_mort_agegrp_m <- function(year_int, wpp_popM, mx_male, age_grp) {
  sapply(seq_along(age_grp), function(g) {
    age_rows <- age_grp[[g]]
        pop_vec <- if (year_int == 2024) {
      wpp_popM[age_rows, "2023"] * 1000
    } else {
      wpp_popM[age_rows, as.character(year_int)] * 1000
    }
        mx_vec <- if (year_int == 2024) {
      mx_male[age_rows, "2024"]
    } else {
      mx_male[age_rows, as.character(year_int)]
    }
        -log(1 - (sum(pop_vec * (1 - exp(-mx_vec))) / sum(pop_vec)))
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
    -log(1 - (sum(pop_vec * (1 - exp(-mx_vec))) / sum(pop_vec)))
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
  beta_age_overall ~ normal(log(1), 0.5); // exp(log(1) + c(-1,1) * qnorm(0.975) * 0.5)
  
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
# single survey country, nrow=1 as only 1 year's survey data

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
),

malawi = list(
  yr_svy = c(2015.5, 2019.5, 2020.5),
  ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
  den_svy_f = matrix(
    c( 6698, 4260, 4117, -999, # 2015
       8372, 5690, 5570, -999, # 2019
      3544, 2504, 3145, 1530), # 2020
    nrow = 3, byrow = TRUE),
  num_svy_f = matrix(
    c(50, 48, 39, -999, # 2015
      540, 412, 273, -999, # 2019
       273, 193, 169, 24), # 2020
    nrow = 3, byrow = TRUE),
  den_svy_m = matrix(
    c(858, 1041, 1061, -999, # 2015 
      2546, 1368, 1519, -999,# 2019
      2217, 2199, 2377, 1440), # 2020 
    nrow = 3, byrow = TRUE),
  num_svy_m = matrix(
    c(6, 17, 12, -999, # 2015
     230, 131, 109, -999, # 2019 
      190, 219, 163, 49), # 2020
    nrow = 3, byrow = TRUE),
  yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(408900, 101256, 561282, 602657, 735385, 910088),
  se_hts =  c(408900, 101256, 561282, 602657, 735385, 910088) * 0.1 
),
madagascar = list(
  yr_svy = c(2018.5, 2021.5),
  ind_svy = (c(2018.5, 2021.5) - start) / dt,
  den_svy_f = matrix(
    c(3796, 1712, 2117, -999, # 2018
       4793, 1859, 4414, -999), # 2021
    nrow = 2, byrow = TRUE),
  num_svy_f = matrix(
    c( 41, 48, 30, -999, # 2018
       10, 9, 10, -999), # 2021
    nrow = 2, byrow = TRUE),
  den_svy_m = matrix(
    c(1188, 888, 1711, -999, # 2018 
      2971, 2607, 1610, -999), # 2021 
    nrow = 2, byrow = TRUE),
  num_svy_m = matrix(
    c(9, 17, 18, -999, # 2018
       13, 18, 17, -999), # 2021
    nrow = 2, byrow = TRUE),
  yr_hts = c(2022,  2023) + 0.5,
  ind_hts = (c(2022, 2023) - start + 0.5) / dt,
  hts_dat = c(2500, 2500),
  se_hts = c(2500, 2500) * 0.1
),

zimbabwe = list(
  yr_svy = c(2015.5, 2019.5, 2020.5),
  ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
  den_svy_f = matrix(
    c(4078, 2387, 1661, -999, # 2015
      2332, 1954, 1978, -999, # 2019
      2759, 2343, 3081, 1876), # 2020
    nrow = 3, byrow = TRUE),
  num_svy_f = matrix(
    c(8, 11, 3, -999, # 2015
       116, 137, 95, -999, # 2019
       166, 213, 177, 42), # 2020
    nrow = 3, byrow = TRUE),
  den_svy_m = matrix(
    c(2254, 1950, 1453, -999, # 2015 
       998, 611, 870, -999,# 2019
      2124, 1340, 1960, 1137), # 2020 
    nrow = 3, byrow = TRUE),
  num_svy_m = matrix(
    c(22, 34, 35, -999, # 2015
      39, 46, 47, -999, # 2019 
      112, 117, 122, 31), # 2020
    nrow = 3, byrow = TRUE),
  yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(197408, 174566, 240434, 459517, 414499, 513090),
  se_hts = c(197408, 174566, 240434, 459517, 414499, 513090) * 0.1
),

sierraleone = list(
  yr_svy = c(2017.5, 2019.5),
  ind_svy = (c(2017.5, 2019.5) - start) / dt,
  den_svy_f = matrix(
    c(5854, 4421, 4019, -999, # 2017
       2203, 1346, 2236, -999), # 2019
    nrow = 2, byrow = TRUE),
  num_svy_f = matrix(
    c(167, 170, 125, -999, # 2017
       70, 68, 80, -999), # 2019
    nrow = 2, byrow = TRUE),
  den_svy_m = matrix(
    c(5854, 4421, 4019, -999, # 2017 
      1609, 541, 1157, -999), # 2019 
    nrow = 2, byrow = TRUE),
  num_svy_m = matrix(
    c(167, 170, 125, -999, # 2017
       22, 17, 24, -999), # 2022
    nrow = 2, byrow = TRUE),
  yr_hts = c(2021, 2022, 2023) + 0.5,
  ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(2678, 1173, 50340),
  se_hts = c(2678, 1173, 50340) * 0.1
),

zambia = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(1845, 1419, 1394, -999), # 2018
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(47, 51, 36, -999), # 2018
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1555, 1531, 1820, -999), # 2018 
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(38,52,63, -999), # 2018
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(315348, 781175, 639225, 23750, 33153, 95559),
  se_hts = c(315348, 781175, 639225, 23750, 33153, 95559) * 0.1
),
mali = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(2046, 1434, 1526, -999), # 2018
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(17, 15, 20, -999), # 2018
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1361, 1189, 943, -999), # 2018 
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(2,4,4, -999), # 2018
    nrow = 1, byrow = TRUE),
  yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(7763, 169962, 11375, 235729),
  se_hts = c(7763, 169962, 11375, 235729) * 0.1
),
uganda = list(
  yr_svy =  2016.5,
  ind_svy = (2016.5 - start) / dt,
  den_svy_f = matrix(
    c(6404, 2748, 3072, -999), # 2016
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(223, 205, 97, -999), # 2016
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1639, 1009, 1094, -999), # 2016 
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(58, 95, 59, -999), # 2016
    nrow = 1, byrow = TRUE),
  yr_hts = c(2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(42570, 306421, 750698, 681602),
  se_hts = c(42570, 306421, 750698, 681602) * 0.1
),
lesotho  = list(
  yr_svy =  c(2020.5, 2023.5),
  ind_svy = (c(2020.5, 2023.5) - start) / dt,
  den_svy_f = matrix(
    c(2024, 1786, 2170, 1824, # 2020
      1498, 889, 1464, -999), # 2023
    nrow = 2, byrow = TRUE),
  num_svy_f = matrix(
    c(226, 224, 134, 34, # 2020
     852, 538, 482, -999), # 2023
    nrow = 2, byrow = TRUE),
  den_svy_m = matrix(
    c(1449, 1254, 1643, 1014, # 2020
      388, 478, 490, -999), # 2023
    nrow = 2, byrow = TRUE),
  num_svy_m = matrix(
    c(146, 182, 126, 24, # 2020
      132, 241, 145, -999), # 2023
    nrow = 2, byrow = TRUE),
  yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(58917, 42650, 164236, 281277, 301762, 262915),
  se_hts = c(58917, 42650, 164236, 281277, 301762, 262915) * 0.1
),
mozambique = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy_f = matrix(
    c(2372, 1943, 2263, 1156 ), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(227, 259, 215, 43), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1828, 1294, 1709, 899),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(110, 149, 170, 843), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2021, 2022, 2023) + 0.5,
  ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(67883, 203966, 683345),
  se_hts = c(67883, 203966, 683345) * 0.1
),

rwanda = list(
  yr_svy =  2019.5,
  ind_svy = (2019.5 - start) / dt,
  den_svy_f = matrix(
    c(4995, 3401, 4987, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(57, 70, 43, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1809, 1046, 1173, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(14, 39, 17, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = 2023 + 0.5,
  ind_hts = (2023 - start + 0.5) / dt,
  hts_dat = 62683,
  se_hts = 62683 * 0.1
),
burkinafaso = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy_f = matrix(
    c(3870, 5248, 3926, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(7, 17, 10, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(2336, 1026, 1451, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(4, 9, 9, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2022, 2023) + 0.5,
  ind_hts = (c(2022, 2023) - start + 0.5) / dt,
  hts_dat = c(968, 3367),
  se_hts = c(968, 3367) * 0.1
),
burundi = list(
  yr_svy =  2016.5,
  ind_svy = (2016.5 - start) / dt,
  den_svy_f = matrix(
    c(3182, 4642, 1929, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(7, 16, 3, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(2756, 1372, 2230, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(5, 10, 10, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = 2023 + 0.5,
  ind_hts = (2023 - start + 0.5) / dt,
  hts_dat = 78013,
  se_hts = 78013 * 0.1
),
cameroon = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(3721, 2750, 2138, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(48, 88, 50, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1381, 1151, 1008, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(27, 44, 52, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2021, 2022) + 0.5,
  ind_hts = (c(2021,2022) - start + 0.5) / dt,
  hts_dat = c(15000, 33073),
  se_hts = c(15000,33073) * 0.1
),
cotedivoire = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy_f = matrix(
    c(3313, 2067, 1231, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(17, 31, 16, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(872, 815, 853, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(5, 12, 16, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(1159, 111184, 117556, 41774, 60154),
  se_hts = c(1159, 111184, 117556, 41774, 60154) * 0.1
),
guinea = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(2279, 1720, 2194, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(14, 20, 15, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1006, 904, 959, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(3, 6, 5, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2019, 2022) + 0.5,
  ind_hts = (c(2019, 2022) - start + 0.5) / dt,
  hts_dat = c(12, 152),
  se_hts = c(12, 152) * 0.1
),
liberia = list(
  yr_svy =  2019.5,
  ind_svy = (2019.5 - start) / dt,
  den_svy_f = matrix(
    c(2313, 829, 1095, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(18, 18, 19, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(952, 444, 849, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(5, 9, 10, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = 2023 + 0.5,
  ind_hts = (2023 - start + 0.5) / dt,
  hts_dat = 12129,
  se_hts = 12129 * 0.1
),
senegal = list(
  yr_svy =  2017.5,
  ind_svy = (2017.5 - start) / dt,
  den_svy_f = matrix(
    c(4458, 3271, 5904, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(4, 10, 10, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(3384, 655, 3226, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(1, 1, 2, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(7307, 18860, 5505, 4056, 11932),
  se_hts = c(7307, 18860, 5505, 4056, 11932) * 0.1
),
southafrica = list(
  yr_svy =  2016.5,
  ind_svy = (2016.5 - start) / dt,
  den_svy_f = matrix(
    c(1734, 1637, 1470, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(39, 62, 42, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(264, 526, 469, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(6, 17, 17, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2019, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(1200000, 794034, 913418, 212000),
  se_hts = c(1200000, 794034, 913418, 212000) * 0.1
),
tanzania = list(
  yr_svy =  2022.5,
  ind_svy = (2022.5 - start) / dt,
  den_svy_f = matrix(
    c(2937, 2173, 2523, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(68, 103, 71, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1439, 916, 1096, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(28, 61, 66, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(25810, 14940, 19000, 38717, 809603, 1447029),
  se_hts = c(25810, 14940, 19000, 38717, 809603, 1447029) * 0.1
),

namibia = list(
  yr_svy =  2017.5,
  ind_svy = (2017.5 - start) / dt,
  den_svy_f = matrix(
    c(1851, 2159, 2696, 650), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(66, 105, 70, 6), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1050, 1342, 1782, 398),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(15, 59, 50, 4), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(3910, 40075, 47258, 130000, 27974),
  se_hts = c(3910, 40075, 47258, 130000, 27974) * 0.1
),

botswana = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy_f = matrix(
    c(1958, 2002, 3199, 811), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(50, 71, 46, 6), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(1559, 1401, 2348, 475),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(22, 30, 50, 3), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(7000, 3848, 8403, 16405),
  se_hts = c(7000, 3848, 8403, 16405) * 0.1
),

guineabissau = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(2347, 1097, 1339, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(38, 44, 28, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(279, 233, 210, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(7, 11, 11, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = 2020 + 0.5,
  ind_hts = (2020 - start + 0.5) / dt,
  hts_dat = 37500,
  se_hts = 37500 * 0.1
),

drc = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy_f = matrix(
    c(1733, 1626, 1404, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(22, 37, 24, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(244, 419, 259, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(5, 16, 11, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2018, 2020, 2021) + 0.5,
  ind_hts = (c(2018, 2020, 2021) - start + 0.5) / dt,
  hts_dat = c(500, 7158, 7480),
  se_hts = c(500, 7158, 7480) * 0.1
),
eswatini = list(
  yr_svy = c(2021.5, 2022.5),
  ind_svy = (c(2021.5, 2022.5) - start) / dt,
  den_svy_f = matrix(
    c(642, 484, 479, -999, # 2021
       1552, 1348, 1612, 1076), # 2022 phia
    nrow = 2, byrow = TRUE),
  num_svy_f = matrix(
    c(194, 178, 101, -999, #2021
      470, 360, 213, 54), # 2022 pia
    nrow = 2, byrow = TRUE),
  den_svy_m = matrix(
    c(601, 377, 349, -999, # 2021 MICS
       1425, 924, 1110, 587),  # 2022 PHIA
    nrow = 2, byrow = TRUE),
  num_svy_m = matrix(
    c(118, 122, 85, -999, # 2021
       271, 283, 177, 22), # 2022 phia
    nrow = 2, byrow = TRUE),
  yr_hts = c(2018, 2019, 2020, 2021, 2022) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022) - start + 0.5) / dt,
  hts_dat = c(33159, 32531, 191990, 78570, 111912),
  se_hts = c(33159, 32531, 191990, 78570, 111912) * 0.1
),

benin = list(
  yr_svy =  2017.5,
  ind_svy = (2017.5 - start) / dt,
  den_svy_f = matrix(
    c(4180, 5512, 3754, -999), 
    nrow = 1, byrow = TRUE),
  num_svy_f = matrix(
    c(20, 34, 20, -999), 
    nrow = 1, byrow = TRUE),
  den_svy_m = matrix(
    c(3054, 1961, 1868, -999),  
    nrow = 1, byrow = TRUE),
  num_svy_m = matrix(
    c(3, 17, 15, -999), 
    nrow = 1, byrow = TRUE),
  yr_hts = c(2022, 2023) + 0.5,
  ind_hts = (c(2022, 2023) - start + 0.5) / dt,
  hts_dat = c(5173, 8149),
  se_hts = c(5173, 8149) * 0.1
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
fit <- sampling(hivst_stan, data = data_stan, iter = 4000, chains = 4, init = init_function,
                warmup = 2000, thin = 1, control = list(adapt_delta = 0.9))

# saving the model results
# fit
saveRDS(fit, file = "D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_fit.rds")
fit <- readRDS("D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_fit.rds")

# compiled StanModel object 
saveRDS(hivst_stan, file = "D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_model.rds")
hivst_stan <- readRDS("D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_model.rds")

# posterior summaries
fit_summary <- summary(fit)
saveRDS(fit_summary, file = "D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_summary.rds")
fit_summary <- readRDS("D:/Downloads/MSc Thesis/hivst/Model results/hivst_stan_summary.rds")


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



#--- function code to check for survey and program fit--------
#  no separate sex dimension in svy_prd_m because it is only for males
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]

# men results
svy_m_full <- rstan::summary(fit, "svy_prd_m", probs=c(0.025, 0.5, 0.975))$summary
svy_m_full <- as.data.frame(svy_m_full)
svy_m_full$param <- rownames(svy_m_full)

# splitting by countries
n_cnt <- length(countries)
svy_m_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_m_full$param)   # only country c
  tmp_c <- svy_m_full[ix_c, ]
  
  #  within that subset, separating each age group
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_m_list[[c]] <- ages
}

# plotting for each country
par(mfrow = c(n_cnt, 1))
for (c in seq_len(n_cnt)) {
  plot(NA, xlim=range(time), ylim=c(0, 0.2), 
       main=paste("Men -", countries[c]),
       xlab="Year", ylab="Ever used HIVST")
  
  for (a in 1:4) {
    df_age <- svy_m_list[[c]][[a]] 
    lines(df_age$`50%` ~ time, col=a, lwd=2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col=adjustcolor(a, alpha.f=0.3), border=NA
    )
    obs <- cnt_data[[c]]$svy_dat_m[, a]   
    lci <- cnt_data[[c]]$lci_svy_m[, a]
    uci <- cnt_data[[c]]$uci_svy_m[, a]
    t_obs <- cnt_data[[c]]$yr_svy  # 2012.5, 2018.5 ...
    points(obs ~ t_obs, pch=16, col=a)
    segments(t_obs, lci, t_obs, uci, col=a)
  }
  legend("topleft", legend=c("15-24","25-34","35-49","50+"),
         col=1:4, lwd=2, bty="n")
}

# women results
svy_f_full <- rstan::summary(fit, "svy_prd_f", probs=c(0.025, 0.5, 0.975))$summary
svy_f_full <- as.data.frame(svy_f_full)
svy_f_full$param <- rownames(svy_f_full)

svy_f_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_f_full$param)
  tmp_c <- svy_f_full[ix_c, ]
  
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_f_list[[c]] <- ages
}

# plotting each country
par(mfrow = c(n_cnt, 1))
for (c in seq_len(n_cnt)) {
  plot(NA, xlim=range(time), ylim=c(0, 0.2), 
       main=paste("Women -", countries[c]),
       xlab="Year", ylab="Ever used HIVST")
  
  for (a in 1:4) {
    df_age <- svy_f_list[[c]][[a]] 
    lines(df_age$`50%` ~ time, col=a, lwd=2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col=adjustcolor(a, alpha.f=0.3), border=NA
    )
    #  female survey data
    obs <- cnt_data[[c]]$svy_dat_f[, a]
    lci <- cnt_data[[c]]$lci_svy_f[, a]
    uci <- cnt_data[[c]]$uci_svy_f[, a]
    t_obs <- cnt_data[[c]]$yr_svy
    
    points(obs ~ t_obs, pch=16, col=a)
    segments(t_obs, lci, t_obs, uci, col=a)
  }
  legend("topleft", legend=c("15-24","25-34","35-49","50+"),
         col=1:4, lwd=2, bty="n")
}

# hts results
hts_full <- as.data.frame(rstan::summary(fit, "hivst_prd")$summary)
hts_full$param <- rownames(hts_full)

hts_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), hts_full$param)
  hts_list[[c]] <- hts_full[ix_c, ]
}

# each hts_list[[c]] is the time series (rows) for country c
par(mfrow = c(n_cnt,1))
for (c in seq_len(n_cnt)) {
  df_c <- hts_list[[c]]
  
  plot(time, df_c$`50%`, type="l", col="blue", lwd=2,
       main=paste("HTS -", countries[c]),
       xlab="Year", ylab="Number of HIVST kits")
  polygon(x=c(time, rev(time)),
          y=c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col=adjustcolor("blue", alpha.f=0.3),
          border=NA)
  
  # overlaying observed program data
  t_obs <- cnt_data[[c]]$yr_hts
  obs_hts <- cnt_data[[c]]$hts_dat
  points(obs_hts ~ t_obs, pch=16, col="red")
}



#------------plotting trends by sex-------------------------
#-----plot code--------
# overall trend by sex (age aggregated)
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

dim(ext_fit_m) # draws, country, iterations (time), age groups
n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

data_stan$pop #  4 rows per country (one per age group)
# pop_mat_m[c, a] => population of males in country c, age group a
# pop_mat_f[c, a] => population of females in country c, age group a

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = 4)
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = 4)
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1 of data_stan$pop
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2 of data_stan$pop
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}
# total region‐level population (across all countries and all age groups)
total_pop_male <- sum(pop_mat_m)
total_pop_female <- sum(pop_mat_f)


# getting overall proportion for trend stratified by sex
male_prp <- matrix(NA, nrow = n_draws, ncol = niter)
female_prp <- matrix(NA, nrow = n_draws, ncol = niter)
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    # summing for men
    sum_m <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        # ext_fit_m[i, c, t, a] = predicted proportion (0 to 1)
        # multiplying proportion by population in that group
        sum_m <- sum_m + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
      }
    }
    male_prp[i, t] <- sum_m / total_pop_male
    
    # summing for women
    sum_f <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_f <- sum_f + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    female_prp[i, t] <- sum_f / total_pop_female
  }
}

# median and 95% CrI
male_lci <- apply(male_prp, 2, quantile, 0.025)
male_med <- apply(male_prp, 2, quantile, 0.5)
male_uci <- apply(male_prp, 2, quantile, 0.975)
female_lci <- apply(female_prp, 2, quantile, 0.025)
female_med <- apply(female_prp, 2, quantile, 0.5)
female_uci <- apply(female_prp, 2, quantile, 0.975)

# plotting trend by sex
dev.off()
df_sextrend <- data.frame(
  time       = time,
  male_med   = male_med * 100,
  male_lci   = male_lci * 100,
  male_uci   = male_uci * 100,
  female_med = female_med * 100,
  female_lci = female_lci * 100,
  female_uci = female_uci * 100
)

sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(aes(ymin = male_lci, ymax = male_uci),
              fill = "lightblue", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = male_med, color = "Male"), size = 1.1) +
  geom_ribbon(aes(ymin = female_lci, ymax = female_uci),
              fill = "pink", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = female_med, color = "Female"), size = 1.1) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("Trends in HIVST uptake by sex in Africa")

sex_trend_plot


#------------- overall trend by age groups ------------------
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = n_age)  # male pop
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = n_age)  # female pop
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}

#  array to store sex-aggregated proportions by age group
# prp_age[i, t, a] = proportion who have used HIVST in age group a, time t, draw i, aggregated over all countries & sexes
prp_age <- array(NA, dim = c(n_draws, niter, n_age))
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    for (a in seq_len(n_age)) {
      sum_counts <- 0
      for (c in seq_len(n_cnt)) {
        # male
        sum_counts <- sum_counts + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
        # female
        sum_counts <- sum_counts + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
      total_pop_age <- sum(pop_mat_m[, a]) + sum(pop_mat_f[, a])
      
      prp_age[i, t, a] <- sum_counts / total_pop_age
    }
  }
}
prp_age_median <- apply(prp_age, c(2, 3), median)   
prp_age_lci    <- apply(prp_age, c(2, 3), quantile, 0.025)
prp_age_uci    <- apply(prp_age, c(2, 3), quantile, 0.975)


# plotting trend by age group
age_labels <- c("15-24 years", "25-34 years", "35-49 years", "50+ years")
df_age <- data.frame()
for (a in seq_len(n_age)) {
  df_a <- data.frame(
    time   = time,
    median = prp_age_median[, a] * 100,  
    lci    = prp_age_lci[, a]    * 100,
    uci    = prp_age_uci[, a]    * 100,
    age_grp = age_labels[a]  
  )
  
  df_age <- rbind(df_age, df_a)
}

p_age <- ggplot(df_age, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = age_grp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = median, color = age_grp),
            size = 1.1) +
  scale_color_manual(
    values = c("15-24 years" = "#326df9", 
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  ) +
  scale_fill_manual(
    values = c("15-24 years" = "#326df9",
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  )+
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)",
    color = "Age Group",
    fill  = "Age Group"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("Trends in HIVST uptake by age group in Afria")
p_age


#-----overall trend by region------
#------overall trend---------

prp_total <- matrix(NA, nrow = n_draws, ncol = niter)
total_pop <- sum(pop_mat_m) + sum(pop_mat_f)

for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    sum_counts <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_counts <- sum_counts +
          ext_fit_m[i, c, t, a] * pop_mat_m[c, a] +
          ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    prp_total[i, t] <- sum_counts / total_pop
  }
}

prp_total_median <- apply(prp_total, 2, median)
prp_total_lci    <- apply(prp_total, 2, quantile, 0.025)
prp_total_uci    <- apply(prp_total, 2, quantile, 0.975)


df_total <- data.frame(
  time   = time,
  median = prp_total_median * 100,
  lci    = prp_total_lci    * 100,
  uci    = prp_total_uci    * 100
)

p_total <- ggplot(df_total, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), 
              fill = "dodgerblue", alpha = 0.2) +
  geom_line(aes(y = median), color = "dodgerblue", size = 1.2) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  ggtitle("Trends in HIVST Uptake in Africa (overall)")

p_total


# ---- SSA region classification from GBD 2015-----
country_to_region <- c(
  "Burundi"                       = "Eastern", 
  "Kenya"                         = "Eastern", 
  "Madagascar"                    = "Eastern", 
  "Malawi"                        = "Eastern", 
  "Mozambique"                    = "Eastern", 
  "Rwanda"                        = "Eastern", 
  "United Republic of Tanzania"   = "Eastern",  
  "Uganda"                        = "Eastern", 
  "Zambia"                        = "Eastern", 
  "Democratic Republic of the Congo" = "Central",
  
  "Botswana"                      = "Southern", 
  "Eswatini"                      = "Southern", 
  "Lesotho"                       = "Southern", 
  "Namibia"                       = "Southern", 
  "South Africa"                  = "Southern", 
  "Zimbabwe"                      = "Southern", 
  
  "Benin"                         = "Western", 
  "Burkina Faso"                  = "Western", 
  "Cameroon"                      = "Western", 
  "Cote d'Ivoire"                 = "Western", 
  "Ghana"                         = "Western", 
  "Guinea"                        = "Western", 
  "Guinea-Bissau"                 = "Western", 
  "Liberia"                       = "Western", 
  "Mali"                          = "Western", 
  "Senegal"                       = "Western", 
  "Sierra Leone"                  = "Western"
)




