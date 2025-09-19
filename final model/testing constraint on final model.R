# ---testing code for the final age and sex stratified model to code plots----
rm(list = ls())
gc()

setwd("D:\\Downloads\\MSc Thesis\\hivst\\Model results")

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
               "Sierra Leone")

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
    vector beta_age_male, 
    vector beta_age_female,
    matrix pop, // matrix r age c sex
    real dt,
    vector entry_m_dt, // vector 
    vector entry_f_dt, // vector 
    matrix mort_m_dt, // matrix
    matrix mort_f_dt, // matrix
    vector alpha // aging rate for 4 age groups
  ) {
    
    // in the multi-country version, the inputs are already on their transformed scale.
    vector[niter] rr_t = beta_t_dt;
    real rr_r = beta_retest;
    real rr_m = beta_male;
    vector[4] rr_a_m = beta_age_male; 
    vector[4] rr_a_f = beta_age_female; 
    
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
                                              - rr_t[i] * rr_m * rr_a_m[1] * out[i-1,1,1,1] 
                                              - (alpha[1] + mort_m_dt[i,1]) * out[i-1,1,1,1]);
        out[i,2,1,1] = out[i-1,2,1,1] + dt * (+ rr_t[i] * rr_m * rr_a_m[1] * out[i-1,1,1,1] 
                                              - (alpha[1] + mort_m_dt[i,1]) *out[i-1,2,1,1]);
        out[i,3,1,1] = rr_t[i] * rr_m * rr_a_m[1] * (out[i-1,1,1,1] + rr_r * out[i-1,2,1,1]);
        out[i,4,1,1] = out[i,2,1,1] / (out[i,1,1,1] + out[i,2,1,1]);
        
        // Males, age group 2-3
        out[i,1,1,2] = out[i-1,1,1,2] + dt * (+ alpha[1] * out[i-1,1,1,1] 
                                              - rr_t[i] * rr_m * rr_a_m[2] * out[i-1,1,1,2] 
                                              - (alpha[2] + mort_m_dt[i,2]) * out[i-1,1,1,2]);
        out[i,2,1,2] = out[i-1,2,1,2] + dt * (+ alpha[1] * out[i-1,2,1,1]
                                              + rr_t[i] * rr_m * rr_a_m[2] * out[i-1,1,1,2] 
                                              - (alpha[2] + mort_m_dt[i,2]) * out[i-1,2,1,2]);
        out[i,3,1,2] = rr_t[i] * rr_m * rr_a_m[2] * (out[i-1,1,1,2] + rr_r * out[i-1,2,1,2]);
        out[i,4,1,2] = out[i,2,1,2] / (out[i,1,1,2] + out[i,2,1,2]);
        
        // Males, age group 3
        out[i,1,1,3] = out[i-1,1,1,3] + dt * (+ alpha[2] * out[i-1,1,1,2] 
                                              - rr_t[i] * rr_m * rr_a_m[3] * out[i-1,1,1,3] 
                                              - (alpha[3] + mort_m_dt[i,3]) * out[i-1,1,1,3]);
        out[i,2,1,3] = out[i-1,2,1,3] + dt * (+ alpha[2] * out[i-1,2,1,2] 
                                              + rr_t[i] * rr_m * rr_a_m[3] * out[i-1,1,1,3] 
                                              - (alpha[3] + mort_m_dt[i,3]) * out[i-1,2,1,3]);
        out[i,3,1,3] = rr_t[i] * rr_m * rr_a_m[3] * (out[i-1,1,1,3] + rr_r * out[i-1,2,1,3]);
        out[i,4,1,3] = out[i,2,1,3] / (out[i,1,1,3] + out[i,2,1,3]);
        
        // Males, age group 4
        out[i,1,1,4] = out[i-1,1,1,4] + dt * (+ alpha[3] * out[i-1,1,1,3] 
                                              - rr_t[i] * rr_m * rr_a_m[4] * out[i-1,1,1,4] 
                                              - mort_m_dt[i,4] * out[i-1,1,1,4]);
        out[i,2,1,4] = out[i-1,2,1,4] + dt * (+ alpha[3] * out[i-1,2,1,3] 
                                              + rr_t[i] * rr_m * rr_a_m[4] * out[i-1,1,1,4] 
                                              - mort_m_dt[i,4] * out[i-1,2,1,4]);
        out[i,3,1,4] = rr_t[i] * rr_m * rr_a_m[4] * (out[i-1,1,1,4] + rr_r * out[i-1,2,1,4]);
        out[i,4,1,4] = out[i,2,1,4] / (out[i,1,1,4] + out[i,2,1,4]);
        
        // Females, age group 1
        out[i,1,2,1] = out[i-1,1,2,1] + dt * (+ entry_f_dt[i] * (sum(out[i-1,1,2, ]) + sum(out[i-1,2,2,])) 
                                              - rr_t[i] * rr_a_f[1] * out[i-1,1,2,1] 
                                              - (alpha[1] + mort_f_dt[i,1]) * out[i-1,1,2,1]);
        out[i,2,2,1] = out[i-1,2,2,1] + dt * (+ rr_t[i] * rr_a_f[1] * out[i-1,1,2,1] 
                                              - (alpha[1] + mort_f_dt[i,1]) * out[i-1,2,2,1]);
        out[i,3,2,1] = rr_t[i] * rr_a_f[1] * (out[i-1,1,2,1] + rr_r * out[i-1,2,2,1]);
        out[i,4,2,1] = out[i,2,2,1] / (out[i,1,2,1] + out[i,2,2,1]);
        
        // Females, age group 2
        out[i,1,2,2] = out[i-1,1,2,2] + dt * (+ alpha[1] * out[i-1,1,2,1] 
                                              - rr_t[i] * rr_a_f[2] * out[i-1,1,2,2] 
                                              - (alpha[2] + mort_f_dt[i,2]) * out[i-1,1,2,2]);
        out[i,2,2,2] = out[i-1,2,2,2] + dt * (+ alpha[1] * out[i-1,2,2,1] 
                                              + rr_t[i] * rr_a_f[2] * out[i-1,1,2,2] 
                                              - (alpha[2] + mort_f_dt[i,2]) * out[i-1,2,2,2]);
        out[i,3,2,2] = rr_t[i] * rr_a_f[2] * (out[i-1,1,2,2] + rr_r * out[i-1,2,2,2]);
        out[i,4,2,2] = out[i,2,2,2] / (out[i,1,2,2] + out[i,2,2,2]);
        
        // Females, age group 3
        out[i,1,2,3] = out[i-1,1,2,3] + dt * (+ alpha[2] * out[i-1,1,2,2] 
                                              - rr_t[i] * rr_a_f[3] * out[i-1,1,2,3] 
                                              - (alpha[3] + mort_f_dt[i,3]) * out[i-1,1,2,3]);
        out[i,2,2,3] = out[i-1,2,2,3] + dt * (+ alpha[2] * out[i-1,2,2,2] 
                                              + rr_t[i] * rr_a_f[3] * out[i-1,1,2,3] 
                                              - (alpha[3] + mort_f_dt[i,3]) * out[i-1,2,2,3]);
        out[i,3,2,3] = rr_t[i] * rr_a_f[3] * (out[i-1,1,2,3] + rr_r * out[i-1,2,2,3]);
        out[i,4,2,3] = out[i,2,2,3] / (out[i,1,2,3] + out[i,2,2,3]);
        
        // Females, age group 4
        out[i,1,2,4] = out[i-1,1,2,4] + dt * (+ alpha[3] * out[i-1,1,2,3] 
                                              - rr_t[i] * rr_a_f[4] * out[i-1,1,2,4] 
                                              - mort_f_dt[i,4] * out[i-1,1,2,4]);
        out[i,2,2,4] = out[i-1,2,2,4] + dt * (+ alpha[3] * out[i-1,2,2,3] 
                                              + rr_t[i] * rr_a_f[4] * out[i-1,1,2,4] 
                                              - mort_f_dt[i,4] * out[i-1,2,2,4]);
        out[i,3,2,4] = rr_t[i] * rr_a_f[4] * (out[i-1,1,2,4] + rr_r * out[i-1,2,2,4]);
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

  int<lower = 1> hts_idx_s[n_cnt];
  int<lower = 1> hts_idx_e[n_cnt];

  // entry and exit rates for open pop multiple countries, age stratified
  matrix[n_yr, n_cnt] entry_m;
  matrix[n_yr, n_cnt] entry_f;
  real mort_m[n_cnt, n_yr, 4]; // array of matrices, each matrix mortality rates of that country, r year c age groups   
  real mort_f[n_cnt, n_yr, 4]; // array of matrices, each matrix mortality rates of that country, r year c age groups
  
  // aging rate
  vector[3] alpha;
  
  
  // for constraint
  int<lower=1>  max_n_miss;                       // widest row of ragged array
  int<lower=0>  n_miss[n_cnt];                    // number of missing mid years per country
  int<lower=1,upper=niter> missing_i_mat[n_cnt, max_n_miss]; // matrix for missing time steps
  real<lower=0> threshold[n_cnt];                 // four times max HTS for each country
}

parameters {
  // testing rate
  matrix<upper = -1>[n_cnt, n_yr] beta_t;  // yearly HIVST testing rates (rw1) for each country
  
  // standard deviations
  real<lower = 1e-6, upper = 5> sd_rw;     // sd of the rw1 for beta_t
  real<lower = 1e-6, upper = 5> sd_phi;    // sd of the RE for phi
  real<lower = 1e-6, upper = 5> sd_rt;     // sd of the RE for the re-testing rate ratio
  real<lower = 1e-6, upper = 5> sd_male;   // sd of the RE of the male rate ratio
  real<lower=1e-6, upper=5> sd_age_male;   // sd for RR age male
  real<lower=1e-6, upper=5> sd_age_female; // sd for RR age female
  
  // retesting parameters
  real beta_retest_overall;            // overall shared re-testing rate
  vector[n_cnt] beta_rt_raw;           // country-specific re-testing rates
  
  // male rate ratio parameters
  real beta_male_overall;              //overall male relative rate of HIVST
  vector[n_cnt] beta_male_raw;       // country specific male relative rate of HIVST  
  
  // phi parameters
  real phi_overall;           // overall proportion of HIVST kits used
  vector[n_cnt] phi_raw;      // country-specific proportions of HIVST kits used
  
  // age rate ratio parameters males
  vector[3]    beta_age_male_overall;    // overall RR age male
  vector[n_cnt] beta_age_male_raw1;      
  vector[n_cnt] beta_age_male_raw2;      
  vector[n_cnt] beta_age_male_raw3;      
  
  // age rate ratio parameters females
  vector[3]    beta_age_female_overall;  
  vector[n_cnt] beta_age_female_raw1;
  vector[n_cnt] beta_age_female_raw2;
  vector[n_cnt] beta_age_female_raw3;
}


transformed parameters {
// non-centered parameterization
  vector[n_cnt] beta_retest;
  vector[n_cnt] beta_male;
  vector[n_cnt] phi;
  matrix[n_cnt, 4] beta_age_male;
  matrix[n_cnt, 4] beta_age_female;
  
  beta_retest = 0.5 + (2.5 - 0.5) * inv_logit(beta_retest_overall + sd_rt * beta_rt_raw);
  beta_male = exp(beta_male_overall + sd_male * beta_male_raw);
  phi = 0.5 + (1 - 0.5) * inv_logit(phi_overall + sd_phi * phi_raw);
  beta_age_male[, 1] = rep_vector(1, n_cnt);  // fixed for age group 1
  beta_age_male[, 2] = exp(beta_age_male_overall[1] + sd_age_male * beta_age_male_raw1);
  beta_age_male[, 3] = exp(beta_age_male_overall[2] + sd_age_male * beta_age_male_raw2);
  beta_age_male[, 4] = exp(beta_age_male_overall[3] + sd_age_male * beta_age_male_raw3);
  beta_age_female[, 1] = rep_vector(1, n_cnt);  // fixed for age group 1
  beta_age_female[, 2] = exp(beta_age_female_overall[1] + sd_age_female * beta_age_female_raw1);
  beta_age_female[, 3] = exp(beta_age_female_overall[2] + sd_age_female * beta_age_female_raw2);
  beta_age_female[, 4] = exp(beta_age_female_overall[3] + sd_age_female * beta_age_female_raw3);  
  
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
  sd_age_male ~ normal(0, 0.5) T[1e-6, 5];
  sd_age_female ~ normal(0, 0.5) T[1e-6, 5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(logit((1.2 - 0.5) / (2.5 - 0.5)), 0.5); // 0.5 + (2.5 - 0.5) * plogis(qlogis((1.2 - 0.5) / (2.5 - 0.5)) + c(-1, 1) * qnorm(0.975) * 0.5)
  // overall prior for the proportion of tests distributed being used
  phi_overall ~ normal(logit((0.85 - 0.5) / (1 - 0.5)), 1); // 0.5 + (1 - 0.5) * plogis(qlogis((0.85 - 0.5) / (1-0.5)) + c(-1, 1) * qnorm(0.975) * 1)
  // overall prior for male rate ratio
  beta_male_overall ~ normal(log(1), 0.5);
  // overall prior for age rate ratio male
  beta_age_male_overall ~ normal(log(1), 0.5); // exp(log(1) + c(-1,1) * qnorm(0.975) * 0.5)
// overall prior for age rate ratio female
  beta_age_female_overall ~ normal(log(1), 0.5); // exp(log(1) + c(-1,1) * qnorm(0.975) * 0.5)  
  // for non-centered parameterization
  beta_rt_raw ~ std_normal();
  beta_male_raw ~ std_normal();
  phi_raw ~ std_normal();
  beta_age_male_raw1 ~ std_normal();
  beta_age_male_raw2 ~ std_normal();
  beta_age_male_raw3 ~ std_normal();
  beta_age_female_raw1 ~ std_normal();
  beta_age_female_raw2 ~ std_normal();
  beta_age_female_raw3 ~ std_normal();    
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
    niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], to_vector(beta_age_male[c, ]), to_vector(beta_age_female[c, ]), pop_mat, 
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
for (s in svy_idx_s[c]:svy_idx_e[c]) {

  // Males 50+
  if (den_svy4[s, 1] >= 0) {
    //  valid data for 50+ males
    num_svy4[s, 1] ~ binomial(
      den_svy4[s, 1],
      model_pred[ind_svy[s], 4, 1, 4]
    );
  }
  // else do nothing if den_svy4 is -999

  // Females 50+
  if (den_svy4[s, 2] >= 0) {
    // valid data for 50+ females
    num_svy4[s, 2] ~ binomial(
      den_svy4[s, 2],
      model_pred[ind_svy[s], 4, 2, 4]
    );
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
                                              
  
 // constraint
  real thr      = threshold[c];
  real scale_c  = thr / 4;          // slope; change 4 → 5, 6, … to tighten

  for (mm in 1:n_miss[c]) {
    int i = missing_i_mat[c, mm];
    if (hts_mod[i, c] > thr) {
      target += -0.5 * square( (hts_mod[i, c] - thr) / scale_c );
    }
  }
  
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
      niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], to_vector(beta_age_male[c, ]),  to_vector(beta_age_female[c, ]), pop_mat, 
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
        2691, 1181, 2334, 556), # 2022 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(10, 31, 18, 7, # 2012
        54, 54, 106, 14, # 2018 
        108, 181, 227, 36), # 2022
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
        1604, 1547, 1222, 617), # 2022 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(9, 21, 13, -999, # 2017
        9, 48, 29, 7), # 2022
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
      c(858, 1041, 1061, 148, # 2015 DHS
        2546, 1368, 1519, -999,# 2019
        2217, 2199, 2377, 1440), # 2020 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(6, 17, 12, 2, # 2015 DHS
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
        2971, 2607, 1610, 749), # 2021 DHS
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(9, 17, 18, -999, # 2018
        13, 18, 17, 7), # 2021 DHS
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
      c(2254, 1950, 1453, 333, # 2015 dhs
        998, 611, 870, -999,# 2019
        2124, 1340, 1960, 1137), # 2020 
      nrow = 3, byrow = TRUE),
    num_svy_m = matrix(
      c(22, 34, 35, 6, # 2015 dhs
        39, 46, 47, -999, # 2019 
        112, 117, 122, 31), # 2020
      nrow = 3, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
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
        1609, 541, 1157, 773), # 2019 DHS
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(167, 170, 125, -999, # 2017
        22, 17, 25, 15), # 2019 DHS
      nrow = 2, byrow = TRUE),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2678, 1173, 50340),
    se_hts = c(2678, 1173, 50340) * 0.1
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


# ----- for constraint ----
# creating matrix for the years of missing program data 
all_mid_idx <- ((start:(end - 1)) - start + 0.5) / dt     
missing_idx_list <- vector("list", n_cnt)
threshold_vec    <- numeric(n_cnt)

for (c in seq_len(n_cnt)) {
  obs_idx_c  <- ind_hts[ hts_idx_s[c] : hts_idx_e[c] ]        # observed indices
  miss_idx_c <- setdiff(all_mid_idx, obs_idx_c)               # missing indices
  missing_idx_list[[c]] <- miss_idx_c
  thr_c <- 4 * max( hts_dat[ hts_idx_s[c] : hts_idx_e[c] ] )
  threshold_vec[c] <- thr_c
}

max_n_miss <- max( lengths(missing_idx_list) )                # widest row
missing_i_mat <- matrix(1L, nrow = n_cnt, ncol = max_n_miss)  # filler value 1
n_miss <- integer(n_cnt)

for (c in seq_len(n_cnt)) {
  n_miss[c] <- length(missing_idx_list[[c]])
  if (n_miss[c] > 0)
    missing_i_mat[c, 1:n_miss[c]] <- missing_idx_list[[c]]
}



# data stan for fitting and running 
data_stan <- list(
  n_cnt = length(cnt_data),
  n_yr = n_yr,
  yr_ind = yr_ind,
  niter = niter,
  dt = dt,
  pop = pop_all_combined,
  idx_pop = idx_pop,
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
  alpha = alpha,
  max_n_miss = max_n_miss,
  n_miss = n_miss,
  missing_i_mat = missing_i_mat,
  threshold = threshold_vec
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
    sd_male = runif(1, min = 0.1, max = 1),
    sd_age_male = runif(1, min = 0.1, max = 1),
    sd_age_female = runif(1, min = 0.1, max = 1),
    beta_restest_overall = rnorm(1, qlogis((1.2 - 0.5) / (2.5 - 0.5)), 0.1),
    beta_rt_raw = rnorm(data_stan$n_cnt, 0, 0.5),
    beta_male_overall = rnorm(1, log(1), 0.1),
    beta_male_raw = rnorm(data_stan$n_cnt, 0, 0.1),
    beta_age_male_overall = rnorm(3, log(1), 0.1),
    beta_age_male_raw1 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_male_raw2 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_male_raw3 = rnorm(data_stan$n_cnt, 0, 0.1),
    beta_age_female_overall = rnorm(3, log(1), 0.1),
    beta_age_female_raw1 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_female_raw2 = rnorm(data_stan$n_cnt, 0, 0.1),  
    beta_age_female_raw3 = rnorm(data_stan$n_cnt, 0, 0.1),
    beta_phi_overall = rnorm(1, qlogis((0.8 - 0.5) / (1 - 0.5)), 0.1),
    phi_raw = rnorm(data_stan$n_cnt, 0, 0.2)
  )
}

fit <- sampling(hivst_stan, data = data_stan, iter = 1500, chains = 4, init = init_function,
                warmup = 500, thin = 1, control = list(adapt_delta = 0.9))
# traceplots
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "sd_phi")
traceplot(fit, pars = "sd_rt")
traceplot(fit, pars = "sd_male")
traceplot(fit, pars = "beta_retest_overall")
traceplot(fit, pars = "beta_rt_raw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male_overall")
traceplot(fit, pars = "beta_male_raw")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")
traceplot(fit, pars = "beta_age_male")
traceplot(fit, pars = "beta_age_female")
traceplot(fit, pars = "phi_overall")
traceplot(fit, pars = "phi_raw")

#------saving the model fit and posterior summaries------------

# saving the fit object
saveRDS(fit, file = "hivst_stan_fit_jul14.rds")
fit <- readRDS("hivst_stan_fit_jul14.rds")



#-----------------------------------------------------------------------------

# survey data for men and women (no age group)
cnt_data <- list(
  kenya = list(
    yr_svy = c(2012.5, 2018.5, 2022.5),
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(4605, 16082, 11562), c(6350, 17880, 25725))),
    num_svy = round(cbind(c(148, 340, 1044), c(116, 436, 1242))),
    yr_hts = c(2018,  2019,   2020,    2021,   2022,  2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  ghana = list(
    yr_svy = c(2017.5, 2022.5),
    ind_svy = (c(2017.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(2553, 4558), c(5575, 6250))),
    num_svy = round(cbind(c(37, 83), c(132, 151))),
    yr_hts = c(2020,  2021,   2022,  2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  ),
  malawi = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy = round(cbind(c(2796, 2150, 5165), c(14792, 6669, 5920))),
    num_svy = round(cbind(c(30, 214, 406), c(136, 443, 373))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(408900, 101256, 561282, 602657, 735385, 910088),
    se_hts =  c(408900, 101256, 561282, 602657, 735385, 910088) * 0.1 
  ),
  madagascar = list(
    yr_svy = c(2018.5, 2021.5),
    ind_svy = (c(2018.5, 2021.5) - start) / dt,
    den_svy = round(cbind(c(3055, 6178), c(5039, 6825))),
    num_svy = round(cbind(c(35, 44), c(84, 20))),
    yr_hts = c(2022,  2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2500, 2500),
    se_hts = c(2500, 2500) * 0.1
  ),
  zimbabwe = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy = round(cbind(c(6717, 3343, 6576), c(7964, 8104, 10058))),
    num_svy = round(cbind(c(118, 171, 381), c(21, 447, 594))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197408, 174566, 240434, 459517, 414499, 513090),
    se_hts = c(197408, 174566, 240434, 459517, 414499, 513090) * 0.1
  ),
  sierraleone = list(
    yr_svy = c(2017.5, 2019.5),
    ind_svy = (c(2017.5, 2019.5) - start) / dt,
    den_svy = round(cbind(c(2465, 2907), c(5096, 2607))),
    num_svy = round(cbind(c(50, 62), c(165, 101))),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2678, 1173, 50340),
    se_hts = c(2678, 1173, 50340) * 0.1
  )
)



# ----sum across all age groups for male and female----
svy_m_full <- rstan::summary(fit, "svy_prd_m", probs = c(0.025, 0.5, 0.975))$summary
svy_m_full <- as.data.frame(svy_m_full)
svy_m_full$param <- rownames(svy_m_full)


# Split out each country, then each age group
n_cnt <- length(countries)
svy_m_list <- vector("list", n_cnt)  # each element is a list of 4 data frames (age groups)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_m_full$param)
  tmp_c <- svy_m_full[ix_c, ]
  
  # Separate the 4 age groups
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_m_list[[c]] <- ages
}

# Similarly for women
svy_f_full <- rstan::summary(fit, "svy_prd_f", probs = c(0.025, 0.5, 0.975))$summary
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

# new code for aggregation across all age groups for male and female
time_vec <- time  
n_time <- length(time_vec)

# aggregated results in a list of length n_cnt
svy_m_agg <- vector("list", n_cnt)  # each element: data.frame with columns "time", "p2.5", "p50", "p97.5"
svy_f_agg <- vector("list", n_cnt)  # same for women

for (c_idx in seq_len(n_cnt)) {
  # population for weighting
  pop_c <- pop[[c_idx]]  # 4 x 2 matrix
  pop_m <- pop_c[,1]      # men in each of the 4 age groups
  pop_f <- pop_c[,2]      # women
  
  # retrieve the 4 data frames for men:
  # each df has n_time rows, columns: mean, se_mean, sd, 2.5%, 25%, 50%, 75%, 97.5%, ...
  age_df_m <- svy_m_list[[c_idx]]  # length=4
  age_df_f <- svy_f_list[[c_idx]]
  
  # numeric vectors for aggregated 2.5%, 50%, 97.5%
  p2.5_m <- p50_m <- p97.5_m <- numeric(n_time)
  p2.5_f <- p50_f <- p97.5_f <- numeric(n_time)
  
  # Weighted sums of proportions:
  sum_pop_m <- sum(pop_m)
  sum_pop_f <- sum(pop_f)
  
  for (i in seq_len(n_time)) {
    # For men, each age group a:
    #   proportion in age_df_m[[a]]$`50%`[i]
    # Weighted by pop_m[a]
    
    # 2.5% for men
    num_2.5_m <- 0
    num_50_m  <- 0
    num_97.5_m<- 0
    for (a in 1:4) {
      num_2.5_m  <- num_2.5_m  + pop_m[a] * age_df_m[[a]]$`2.5%`[i]
      num_50_m   <- num_50_m   + pop_m[a] * age_df_m[[a]]$`50%`[i]
      num_97.5_m <- num_97.5_m + pop_m[a] * age_df_m[[a]]$`97.5%`[i]
    }
    p2.5_m[i]  <- num_2.5_m  / sum_pop_m
    p50_m[i]   <- num_50_m   / sum_pop_m
    p97.5_m[i] <- num_97.5_m / sum_pop_m
    
    num_2.5_f  <- 0
    num_50_f   <- 0
    num_97.5_f <- 0
    for (a in 1:4) {
      num_2.5_f  <- num_2.5_f  + pop_f[a] * age_df_f[[a]]$`2.5%`[i]
      num_50_f   <- num_50_f   + pop_f[a] * age_df_f[[a]]$`50%`[i]
      num_97.5_f <- num_97.5_f + pop_f[a] * age_df_f[[a]]$`97.5%`[i]
    }
    p2.5_f[i]  <- num_2.5_f  / sum_pop_f
    p50_f[i]   <- num_50_f   / sum_pop_f
    p97.5_f[i] <- num_97.5_f / sum_pop_f
  }
  
  # combining into data frames
  svy_m_agg[[c_idx]] <- data.frame(
    time   = time_vec,
    p2.5   = p2.5_m,
    p50    = p50_m,
    p97.5  = p97.5_m
  )
  svy_f_agg[[c_idx]] <- data.frame(
    time   = time_vec,
    p2.5   = p2.5_f,
    p50    = p50_f,
    p97.5  = p97.5_f
  )
}


png("aggregate_survey_plots_new.png", width=12, height=20, units="in", res=300)
par(mfrow=c(3,2), mar=c(4,4,2,1))  

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe",
                   "sierraleone")
alphabetical_cnt <- order(cnt_lowercase)

for (c_idx in alphabetical_cnt) {
  men_df  <- svy_m_agg[[c_idx]]  # columns = time, p2.5, p50, p97.5
  women_df<- svy_f_agg[[c_idx]]
  
  # observed survey data
  obs_yr <- cnt_data[[c_idx]]$yr_svy  
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]  # men
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]  # women
  
  ymax <- max(men_df$p97.5, women_df$p97.5,
              obs_m, obs_f, na.rm = TRUE) * 1.05
  plot(NA, xlim = range(time), ylim = c(0, ymax),
       xlab="Year", ylab="Proporition of HIVST use",
       main=paste("Country:", countries[c_idx]))
  
  # men line
  lines(men_df$time, men_df$p50, col="blue", lwd=2)
  polygon(x = c(men_df$time, rev(men_df$time)),
          y = c(men_df$p2.5, rev(men_df$p97.5)),
          col = adjustcolor("blue", alpha.f=0.3),
          border=NA)
  points(obs_yr, obs_m, pch=19, col="blue")
  
  # women line
  lines(women_df$time, women_df$p50, col="red", lwd=2)
  polygon(x = c(women_df$time, rev(women_df$time)),
          y = c(women_df$p2.5, rev(women_df$p97.5)),
          col = adjustcolor("red", alpha.f=0.3),
          border=NA)
  points(obs_yr+0.2, obs_f, pch=19, col="red") # offset a bit on x-axis
  
  legend("topleft", c("Men","Women"), 
         col=c("blue","red"), lwd=2, bty="n")
}
dev.off()




#--- hts results ----
hts_full <- as.data.frame(rstan::summary(fit, "hivst_prd")$summary)
hts_full$param <- rownames(hts_full)

# Splitting by countries
n_cnt <- length(countries)
hts_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), hts_full$param)
  hts_list[[c]] <- hts_full[ix_c, ]
}

png("program_data_fit_new.png",
    width = 14, height = 28,
    units = "in", res = 320)

par(mfrow = c(3, 2))
par(mar = c(3, 4, 2, 1),  
    oma = c(0, 0, 0, 0))

for (c_idx in alphabetical_cnt) {
  df_c <- hts_list[[c_idx]]
  
  plot(time, df_c$`50%`, type = "l", col = "blue", lwd = 2,
       main = paste("HTS -", countries[c_idx]),
       xlab = "Year", ylab = "Number of HIVST kits")
  
  polygon(x = c(time, rev(time)),
          y = c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col = adjustcolor("blue", alpha.f = 0.3),
          border = NA)
  
  # observed program data
  t_obs   <- cnt_data[[c_idx]]$yr_hts
  obs_hts <- cnt_data[[c_idx]]$hts_dat
  points(obs_hts ~ t_obs, pch = 16, col = "red")
}

dev.off()




#---side by side age aggregated men women plot and program data fit----
plot_survey_agg_one_country <- function(
    c_idx,
    svy_m_agg, svy_f_agg,
    time, 
    cnt_data, 
    countries
) {
  # Extract the aggregated men/women data frames
  men_df   <- svy_m_agg[[c_idx]]   # columns: time, p2.5, p50, p97.5
  women_df <- svy_f_agg[[c_idx]]
  
  # Observed data
  obs_yr <- cnt_data[[c_idx]]$yr_svy  
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]  # men
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]  # women
  
  
  plot(
    x    = NA,
    y    = NA,
    xlim = range(time),
    ylim = c(0,ymax),
    xlab = "Year",
    ylab = "Proportion of HIVST use",
    main = paste("HIVST Uptake -", countries[c_idx])
  )
  
  # men line + ribbon
  lines(men_df$p50 ~ men_df$time, col="blue", lwd=2)
  polygon(
    x = c(men_df$time, rev(men_df$time)),
    y = c(men_df$p2.5, rev(men_df$p97.5)),
    col = adjustcolor("blue", alpha.f=0.3),
    border=NA
  )
  points(obs_yr, obs_m, pch=19, col="blue")
  
  # women line + ribbon
  lines(women_df$p50 ~ women_df$time, col="red", lwd=2)
  polygon(
    x = c(women_df$time, rev(women_df$time)),
    y = c(women_df$p2.5, rev(women_df$p97.5)),
    col = adjustcolor("red", alpha.f=0.3),
    border=NA
  )
  points(obs_yr+0.2, obs_f, pch=19, col="red")
  
  legend("topleft", c("Men","Women"), col=c("blue","red"), lwd=2, bty="n")
}


plot_hts_one_country <- function(
    c_idx,
    hts_list,
    cnt_data,
    time,
    countries
) {
  df_c <- hts_list[[c_idx]]  # columns: 2.5%, 50%, 97.5%, etc.
  
  plot(
    x    = time,
    y    = df_c$`50%`,
    type = "l",
    col  = "blue",
    lwd  = 2,
    main = paste("HIVST kits -", countries[c_idx]),
    xlab = "Year",
    ylab = "Number of HIVST kits",
    ylim = c(0, max(df_c$`97.5%`, na.rm=TRUE))
  )
  
  polygon(
    x = c(time, rev(time)),
    y = c(df_c$`2.5%`, rev(df_c$`97.5%`)),
    col = adjustcolor("blue", alpha.f = 0.3),
    border = NA
  )
  
  # Observed program data
  t_obs   <- cnt_data[[c_idx]]$yr_hts
  obs_hts <- cnt_data[[c_idx]]$hts_dat
  points(t_obs, obs_hts, pch = 16, col = "red")
}



N <- length(alphabetical_cnt)  # 27

png("svypgmfit_new.png",
    width = 20, height = 28,   
    units = "in", res = 300)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))

i <- 1
while (i <= N) {
  c1 <- alphabetical_cnt[i]
  
  plot_survey_agg_one_country(
    c_idx     = c1,
    svy_m_agg = svy_m_agg,
    svy_f_agg = svy_f_agg,
    time      = time,
    cnt_data  = cnt_data,
    countries = countries
  )
  plot_hts_one_country(
    c_idx    = c1,
    hts_list = hts_list,
    cnt_data = cnt_data,
    time     = time,
    countries= countries
  )
  
  if (i + 1 <= N) {
    c2 <- alphabetical_cnt[i + 1]
    
    plot_survey_agg_one_country(
      c_idx     = c2,
      svy_m_agg = svy_m_agg,
      svy_f_agg = svy_f_agg,
      time      = time,
      cnt_data  = cnt_data,
      countries = countries
    )
    plot_hts_one_country(
      c_idx    = c2,
      hts_list = hts_list,
      cnt_data = cnt_data,
      time     = time,
      countries= countries
    )
  }
  
  i <- i + 2
}

dev.off()



#------------------------------------------------------------------------------
#---2 separateplots
## ----------------------------------------------------------

countries[countries %in% c("Democratic Republic of Congo",
                           "Democratic Republic of the Congo")] <- "DRC"

countries[countries %in% c("United Republic of Tanzania",
                           "United Republic of Tanzania")] <- "Tanzania"

plot_survey_agg_one_country <- function(
    c_idx, svy_m_agg, svy_f_agg, time,
    cnt_data, countries, ylim = NULL)
{
  men_df   <- svy_m_agg[[c_idx]]
  women_df <- svy_f_agg[[c_idx]]
  
  obs_yr <- cnt_data[[c_idx]]$yr_svy
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]
  
  ## ――dynamic y-limit ------------------------------------------------
  if (is.null(ylim)) {
    ymax <- max(men_df$p97.5, women_df$p97.5,
                obs_m, obs_f, na.rm = TRUE) * 1.05
    ylim <- c(0, ymax)
  }
  ## ------------------------------------------------------------------
  
  plot(NA, xlim = range(time), ylim = ylim,
       xlab = "Year", ylab = "HIVST use proportion",
       main = paste("HIVST uptake:", countries[c_idx]))
  
  lines(men_df$p50 ~ men_df$time, col = "blue", lwd = 2)
  polygon(c(men_df$time, rev(men_df$time)),
          c(men_df$p2.5, rev(men_df$p97.5)),
          col = adjustcolor("blue", 0.30), border = NA)
  points(obs_yr, obs_m, pch = 19, col = "blue")
  
  lines(women_df$p50 ~ women_df$time, col = "red",  lwd = 2)
  polygon(c(women_df$time, rev(women_df$time)),
          c(women_df$p2.5, rev(women_df$p97.5)),
          col = adjustcolor("red", 0.30),  border = NA)
  points(obs_yr + 0.2, obs_f, pch = 19, col = "red")
  
  legend("topleft", c("Men","Women"), col = c("blue","red"),
         lwd = 2, bty = "n", cex = 0.8)
}

options(scipen=999)
plot_hts_one_country <- function(
    c_idx, hts_list, cnt_data, time, countries, ylim = NULL)
{
  df_c <- hts_list[[c_idx]]
  
  if (is.null(ylim))
    ylim <- c(0, max(df_c$`97.5%`, cnt_data[[c_idx]]$hts_dat, na.rm = TRUE))
  
  plot(time, df_c$`50%`, type = "l", col = "blue", lwd = 2,
       main = paste("HIVST kits:", countries[c_idx]),
       xlab = "Year", ylab = "Number of kits", ylim = ylim)
  
  polygon(c(time, rev(time)),
          c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col = adjustcolor("blue", 0.30), border = NA)
  
  points(cnt_data[[c_idx]]$yr_hts,
         cnt_data[[c_idx]]$hts_dat,
         pch = 16, col = "red")
}




## -----------------------------------------------------------------
make_country_page <- function(idx_vec, file_name,
                              svy_m_agg, svy_f_agg,
                              hts_list, cnt_data,
                              time, countries)
{
  n_cnt   <- length(idx_vec)
  n_row   <- ceiling(n_cnt / 2)      # two countries per row
  n_col   <- 4                       # (survey, hts) × 2
  
  png(file_name,
      width  = 8,     # inches – Word portrait page width
      height = 10.5,  # inches – a bit shorter than full 11"
      units  = "in", res = 300)
  
  par(mfrow = c(n_row, n_col),
      mar   = c(3.2, 3.2, 2, 1),     # tight margins
      mgp   = c(2, 0.6, 0),          # axis-title spacing
      cex   = 0.5)                   # down-scale all text
  
  i <- 1
  while (i <= n_cnt) {
    c1 <- idx_vec[i]
    
    plot_survey_agg_one_country(
      c_idx     = c1,
      svy_m_agg = svy_m_agg,
      svy_f_agg = svy_f_agg,
      time      = time,
      cnt_data  = cnt_data,
      countries = countries)
    
    plot_hts_one_country(
      c_idx     = c1,
      hts_list  = hts_list,
      cnt_data  = cnt_data,
      time      = time,
      countries = countries)
    
    if (i + 1 <= n_cnt) {
      c2 <- idx_vec[i + 1]
      
      plot_survey_agg_one_country(
        c_idx     = c2,
        svy_m_agg = svy_m_agg,
        svy_f_agg = svy_f_agg,
        time      = time,
        cnt_data  = cnt_data,
        countries = countries)
      
      plot_hts_one_country(
        c_idx     = c2,
        hts_list  = hts_list,
        cnt_data  = cnt_data,
        time      = time,
        countries = countries)
    } else {
      ## keep the grid square if n_cnt is odd
      for (k in 1:2) plot.new()
    }
    
    i <- i + 2
  }
  
  dev.off()
}
cnt_first14 <- alphabetical_cnt[1:14]
cnt_last13  <- alphabetical_cnt[15:27]

make_country_page(cnt_first14, "svy_hts_first14.png",
                  svy_m_agg, svy_f_agg, hts_list,
                  cnt_data, time, countries)

make_country_page(cnt_last13,  "svy_hts_last13.png",
                  svy_m_agg, svy_f_agg, hts_list,
                  cnt_data, time, countries)



