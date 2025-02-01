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


# pop <- do.call(rbind, lapply(countries, function(ctry) {
#   pop_agegrp_fn(country = ctry, popM = popM1, popF = popF1, age_groups = age_grp)
# }))


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


#--- mortality rate: 4 age groups: 15–24,25–34,35–49,50+ ---
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

##########--not required--########
# mort_list_m <- lapply(countries, function(cn) {
#   mort_rate_m_agegrp(cn, start, end, age_grp)
# })

# # r=years, c = age groups for each country side by side (so in stan I need to say choose 1:4 and then 5:8 for each country)
# mort_mat_m <- do.call(cbind, mort_list_m)
# mort_mat_m <- mort_mat_m[1:13, ] # removing 2024
##########--not required--########


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
    vector beta_age, // log(RR) for 4 age groups
    matrix pop, // matrix r age c sex
    real dt,
    vector entry_m_dt, // vector as before
    vector entry_f_dt, // vector as before
    matrix mort_m_dt, // matrix, earlier it was vector
    matrix mort_f_dt, // matrix, earlier it was vector
    vector alpha // aging rate for 4 age groups
  ) {
  
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    vector[4] rr_a = exp(beta_age); // RR for 4 age groups

    real out[niter, 4, 2, 4]; // niter, model outputs, sex, 4 age groups
    out[, , 1, ] = rep_array(0.0, niter, 4, 4); // adding age grp dimension similarly
    out[, , 2, ] = rep_array(0.0, niter, 4, 4); // adding age grp dimension similarly
    
    // Initialization
    out[1,1,1,1] = pop[1, 1]; // niter=1, never tested, Males,age group 1
    out[1,1,1,2] = pop[2, 1]; // niter=1, never tested, Males, age group 2
    out[1,1,1,3] = pop[3, 1]; // niter=1, never tested, Males, age group 3
    out[1,1,1,4] = pop[4, 1]; // niter=1, never tested, Males, age group 4
    out[1,1,2,1] = pop[1, 2]; // niter=1, never tested, Females, age group 1
    out[1,1,2,2] = pop[2, 2]; // niter=1, never tested, Females, age group 2
    out[1,1,2,3] = pop[3, 2]; // niter=1, never tested, Females, age group 3
    out[1,1,2,4] = pop[4, 2]; // niter=1, never tested, Females, age group 4
    
    for (i in 2:niter) {
  // Males, age group 1
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
  int<lower = 1> n_cnt;            // number of countries
  int<lower = 1> n_yr;
  int<lower = 1> niter;
  int<lower = 1> tot_svy;         // number of surveys accross all countries
  int<lower = 1> tot_hts;         // number of years with hivst data all countries
  int<lower = 1> yr_ind[niter];
  real dt;
  matrix[4, 2] pop[n_cnt]; // array of matrices, each matrix pop of that country, r age c sex 
  
  // for prgrm data
  int<lower = 1> n_hts_by_cnt[n_cnt];   // number of years with observed HIVST data per country
  int<lower = 1> ind_hts[tot_hts]; // indices pgm data
  int<lower = 1> hivst[tot_hts];   // number of HIVST performed per year per counntry
  real se_hts[tot_hts];            // se for program data
  
  // for survey data
  int<lower = 1> n_svy_by_cnt[n_cnt];   // number of surveys per country
  int<lower = 1> ind_svy[tot_svy];     // indices for survey data
  int<lower = -999> num_svy_f[tot_svy, 4]; 
  int<lower = -999> num_svy_m[tot_svy, 4]; 
  int<lower = -999> den_svy_f[tot_svy, 4]; 
  int<lower = -999> den_svy_m[tot_svy, 4]; 
  int<lower = 1> age_up[tot_svy]; // might need to modify

  // indices for the unlist data
  int<lower = 1> svy_idx_s[n_cnt];
  int<lower = 1> svy_idx_e[n_cnt]; 
  int<lower = 1> hts_idx_s[n_cnt];
  int<lower = 1> hts_idx_e[n_cnt];

  // entry and exit rates for open pop multiple countries, age stratified
  matrix[n_yr, n_cnt] entry_m;
  matrix[n_yr, n_cnt] entry_f;
  
  matrix [n_yr, 4] mort_m[n_cnt]; // array of matrices, each matrix mortality rates of that country, r year c age groups   
  matrix [n_yr, 4] mort_f[n_cnt]; // array of matrices, each matrix mortality rates of that country, r year c age groups
  
  // aging rate
  vector[3] alpha;
}

parameters {
  real beta_t[n_yr];                 
  real<lower = 0, upper = 5> sd_rw;  
  real beta_retest;                  
  real beta_male;
  vector[3] beta_age_to_process; // RR age
  real<lower = 0, upper = 1> phi;    
}

 // mapping beta_t, entry rate and exit rate to yearly rate
transformed parameters {
  vector[niter] beta_t_dt;
  vector[niter] entry_m_dt;
  vector[niter] entry_f_dt;
  matrix[niter, 4] mort_m_dt;
  matrix[niter, 4] mort_f_dt;
  vector[4] beta_age;
  
  beta_age[1] = log(1);
  beta_age[2] = beta_age_to_process[1];
  beta_age[3] = beta_age_to_process[2];  
  beta_age[4] = beta_age_to_process[3];
  
  for (i in 1:niter) {
  beta_t_dt[i]   = beta_t[yr_ind[i]];
  entry_m_dt[i]  = entry_m[yr_ind[i]];
  entry_f_dt[i]  = entry_f[yr_ind[i]];
  
  // age grp specific mortality
  for (a in 1:4) {
    mort_m_dt[i, a] = mort_m[yr_ind[i], a];
    mort_f_dt[i, a] = mort_f[yr_ind[i], a];
  }
 }
}

model {
    // matrix[niter, n_cnt] hts_mod
  
  // Priors
  beta_t[1] ~ normal(-10,1);
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr-1)], sd_rw);
  sd_rw ~ normal(0,0.5);
  beta_retest ~ normal(log(1.2), 0.5);
  beta_male ~ normal(log(1), 0.5);
  beta_age_to_process[1] ~ normal(0, 1); // normal log follow exp(log(1) + c(-1, 1) * qnorm(0.975) * 1)
  beta_age_to_process[2] ~ normal(0, 1);
  beta_age_to_process[3] ~ normal(0, 1);  
  phi ~ beta(24,6);
  
  // overall prior for SDs
  sd_rw ~ normal(0, 0.25) T[1e-6, 5];
  sd_phi ~ normal(0, 0.5) T[1e-6, 5];
  sd_rt ~ normal(0, 0.5) T[1e-6, 5];
  sd_male ~ normal(0, 0.5) T[1e-6, 5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(logit(1.2 - 0.5) / (2.5 - 0.5), 0.5); // 0.5 + (2.5 - 0.5) * plogis(qlogis((1.2 - 0.5) / (2.5 - 0.5)) + c(-1, 1) * qnorm(0.975) * 0.5)
  // overall prior for the proportion of tests distributed being used
  phi_overall ~ normal(logit((0.85 - 0.5) / (1 - 0.5)), 1); // 0.5 + (1 - 0.5) * plogis(qlogis((0.85 - 0.5) / (1-0.5)) + c(-1, 1) * qnorm(0.975) * 1)
  // overall prior for male rate ratio
  beta_male_overall ~ normal(log(1), 0.5);
  // overall prior for age rate ratio
  beta_age_overall ~
  
  // for non-centered parameterization
  beta_rt_raw ~ std_normal();
  beta_male_raw ~ std_normal();
  phi_raw ~ std_normal();
  beta_age_raw ~ std_normal();
    
  // country-specific priors
  for (c in 1:n_cnt) {
    beta_t[c, 1] ~ normal(-10, 1); // exp(-10 + c(-1, 1) * 1.96 * 1)
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);
  }
    
  // choosing each country pop matrix from multiple country pop array
  for (c in 1:n_cnt) {
  int row_start = (c - 1)*4 + 1;  // first row for country c
  for (a in 1:4) {
    for (s in 1:2) {
      pop[a, s] = pop[row_start + (a - 1), s];
    }
  } // a bit unsure whether we need this since pop is already an array

  // calling hivst_fun
  real model_pred[niter, 4, 2, 4] = hivst_fun(
    niter, beta_t_dt, beta_retest, beta_male, beta_age, pop[c], dt,
    entry_m_dt, entry_f_dt, mort_m_dt, mort_f_dt, alpha);
  
  // Likelihood: survey
  for (t in 1:n_svy) {
    num_svy_m[t, 1:age_up[t]] ~ binomial(den_svy_m[t, 1:age_up[t]], model_pred[ind_svy[t], 4, 1, 1:age_up[t]]);
    num_svy_f[t, 1:age_up[t]] ~ binomial(den_svy_f[t, 1:age_up[t]], model_pred[ind_svy[t], 4, 2, 1:age_up[t]]);  
    }

  // Likelihood: program data (HTS)
    vector[niter] hts_m = (+ to_vector(model_pred[, 3, 1, 1])
                              + to_vector(model_pred[, 3, 1, 2])
                              + to_vector(model_pred[, 3, 1, 3])
                              + to_vector(model_pred[, 3, 1, 4])
                          + to_vector(model_pred[, 3, 2, 1])
                              + to_vector(model_pred[, 3, 2, 2])
                              + to_vector(model_pred[, 3, 2, 3])
                              + to_vector(model_pred[, 3, 2, 4])) / phi;
    hivst ~ normal(hts_m[ind_hts], se_hts);
    
}
}

generated quantities {
  vector[niter] hivst_prd;
  real svy_prd_m[niter, 4];
  real svy_prd_f[niter, 4];
  real pred[niter, 4, 2, 4] = hivst_fun(
    niter, beta_t_dt, beta_retest, beta_male, beta_age, pop, dt,
    entry_m_dt, entry_f_dt, mort_m_dt, mort_f_dt, alpha
  ); 
  
  svy_prd_m = pred[, 4, 1, ];
  svy_prd_f = pred[, 4, 2, ];
  
  hivst_prd = (+ to_vector(pred[,3,1,1]) 
                  + to_vector(pred[,3,1,2]) 
                  + to_vector(pred[,3,1,3]) 
                  + to_vector(pred[,3,1,4])
               + to_vector(pred[,3,2,1]) 
                  + to_vector(pred[,3,2,2]) 
                  + to_vector(pred[,3,2,3]) 
                  + to_vector(pred[,3,2,4]) ) / phi;
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
  nrow = 3, byrow = TRUE),
num_svy_f = matrix(
  c(52, 72, 47, -999, # 2017
    42, 97, 51, -999), # 2022
  nrow = 3, byrow = TRUE),

den_svy_m = matrix(
  c(1927, 645, 856, -999, # 2017 
    1604, 1547, 1222, -999), # 2022 
  nrow = 3, byrow = TRUE),
num_svy_m = matrix(
  c(9, 21, 13, -999, # 2017
    9, 48, 29, -999), # 2022
  nrow = 3, byrow = TRUE),
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


# age_up index (need to fix for multiple countries)
age_up <- NULL
age_up[1] <- 
for (c in 2:n_cnt) {
for (i in 1:nrow(den_svy_f)) {
  if (sum(den_svy_f[i, ] == -999) == 0) { age_up_i <- 4  
  } else {
    age_up_i <- which(den_svy_f[i, ] == -999) - 1
  }
  age_up <- c(age_up, age_up_i)
}
}

# survey and hts indices
n_cnt <- length(cnt_data)
n_svy_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$yr_svy))) 
n_hts_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$ind_hts)))

# survey
svy_idx_s <- NULL
svy_idx_e <- NULL
svy_idx_s[1] <- 1  
svy_idx_e[1] <- n_svy_by_cnt[1]
# hts
hts_idx_s <- NULL
hts_idx_e <- NULL
hts_idx_s[1] <- 1  
hts_idx_e[1] <- n_hts_by_cnt[1]

# remaining countries
for (c in 2:n_cnt) {
  # survey
  svy_idx_s[c] <- svy_idx_s[c - 1] + n_svy_by_cnt[c - 1]
  svy_idx_e[c] <- svy_idx_s[c] + n_svy_by_cnt[c] - 1   
  # hts
  hts_idx_s[c] <- hts_idx_s[c - 1] + n_hts_by_cnt[c - 1]
  hts_idx_e[c] <- hts_idx_s[c] + n_hts_by_cnt[c] - 1
}


# data for fitting and running (I need to update for age grp)
data_stan <- list(n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                  pop = pop,
                  n_svy = length(ind_svy),
                  age_up = age_up,
                  n_hts = length(hts_dat),
                  ind_svy = ind_svy,
                  ind_hts = ind_hts,
                  hivst = hts_dat,
                  se_hts = se_hts,
                  num_svy_f = num_svy_f,
                  num_svy_m = num_svy_m,
                  den_svy_f = den_svy_f,
                  den_svy_m = den_svy_m,
                  entry_m = entry_m_vec,
                  entry_f = entry_f_vec,
                  mort_m = mort_mat_m,
                  mort_f = mort_mat_f,
                  alpha = alpha
)
rstan_options(auto_write = TRUE)

# Fitting 
options(mc.cores = parallel::detectCores())
fit <- sampling(hivst_stan, data = data_stan, iter = 3000, chains = 4,
                warmup = 1500, thin = 1, control = list(adapt_delta = 0.9))

#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "beta_age") # new
traceplot(fit, pars = "phi")
