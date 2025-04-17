#---April 15: adding separate age rate ratios for men and women----
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
      c(1555, 1531, 1820, 579), # 2018 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(38, 52, 53, 9), # 2018 dhs
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
      c(1361, 1189, 943, 484), # 2018 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(2, 4, 4, 5), # 2018 dhs
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
      c(1639, 1009, 1094, 257), # 2016 dhs
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(58, 95, 59, 8), # 2016 dhs
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
        388, 478, 490, 214), # 2023 dhs
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(146, 182, 126, 24, # 2020
        132, 241, 145, 33), # 2023 dhs
      nrow = 2, byrow = TRUE),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(58917, 42650, 164236, 281277, 301762, 262915),
    se_hts = c(58917, 42650, 164236, 281277, 301762, 262915) * 0.1
  ),
  mozambique = list(
    yr_svy =  c(2021.5, 2022.5),
    ind_svy = (c(2021.5, 2022.5) - start) / dt,
    den_svy_f = matrix(
      c(2372, 1943, 2263, 1156, # 2021
        4269, 3126, 3071, -999), # 2022
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(227, 259, 215, 43, # 2021
        69, 89, 66, -999), # 2022
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(1828, 1294, 1709, 899, # 2021
        1640, 1118, 1150, 146), # 2022 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(110, 149, 170, 56, # 2021
        34, 51, 51, 5), # 2022 dhs
      nrow = 2, byrow = TRUE),
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
      c(1809, 1046, 1173, 715),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(14, 39, 17, 3), 
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
      c(2336, 1026, 1451, 622),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(4, 9, 9, 2), 
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
      c(2756, 1372, 2230, 3009),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 10, 10, 1), 
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
      c(1381, 1151, 1008, 665),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(27, 45, 52, 22), 
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
      c(872, 815, 853, 597),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 12, 16, 3), 
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
      c(1006, 904, 959, 632),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(3, 6, 5, 1), 
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
      c(952, 444, 849, 284),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(5, 9, 10, 7), 
      nrow = 1, byrow = TRUE),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 12129,
    se_hts = 12129 * 0.1
  ),
  senegal = list(
    yr_svy =  c(2017.5, 2023.5),
    ind_svy = (c(2017.5, 2023.5) - start) / dt,
    den_svy_f = matrix(
      c(4458, 3271, 5904, -999, # 2017
        2965, 4634, 2078, -999), # 2023
      nrow = 2, byrow = TRUE),
    num_svy_f = matrix(
      c(4, 10, 10, -999, # 2017
        12, 31, 14, -999), # 2023
      nrow = 2, byrow = TRUE),
    den_svy_m = matrix(
      c(3384, 655, 3226, 330, # 2017
        2170, 1087, 1289, 511), # 2023 
      nrow = 2, byrow = TRUE),
    num_svy_m = matrix(
      c(1, 1, 2, 3, # 2017
        1, 3, 3, 2), # 2023 
      nrow = 2, byrow = TRUE),
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
      c(264, 526, 469, 307),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(6, 17, 17, 2), 
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
      c(3054, 1961, 1868, 1079),  
      nrow = 1, byrow = TRUE),
    num_svy_m = matrix(
      c(3, 17, 15, 3), 
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
# if (any(!apply(rbind(num_svy4, den_svy4), 1, function(row) all(row > 0) || all(row < 0)))) { 
#   print('stop, missing 50+ age inconsistent by sex') }



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
# cnt_no_age50 <- unlist(lapply(cnt_data, function(x) ifelse(all(x$num_svy_m[, 4] == -999), 0, 1)))
# age_grp_na <- unlist(lapply(cnt_data, function(x) ifelse(x$num_svy_m[, 4] == -999, 0, 1)))

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

# data for fitting and running 
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

fit <- sampling(hivst_stan, data = data_stan, iter = 4000, chains = 4, init = init_function,
                warmup = 2000, thin = 1, control = list(adapt_delta = 0.9))
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
saveRDS(fit, file = "hivst_stan_fit_apr17.rds")
fit <- readRDS("hivst_stan_fit_apr17.rds")

# saving the compiled StanModel object 
saveRDS(hivst_stan, "hivst_stan_model_apr17.rds")
hivst_stan <- readRDS("hivst_stan_model_apr17.rds")

# saving posterior summaries
# rstan::summary(fit)
fit_summary <- summary(fit)
saveRDS(fit_summary, file = "hivst_stan_summary_apr17.rds")
fit_summary <- readRDS("hivst_stan_summary_apr17.rds")
# colnames(fit_summary$summary)
# names(rstan::extract(fit))

