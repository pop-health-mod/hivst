
rm(list = ls())
gc()

library(wpp2024)
library(rstan)

# male and female pop
data(popM1)
data(popF1)
# mortality data
data(mxM1)   
data(mxF1)  

#---Kenya----
wpp_m <- popM1[popM1$name == "Kenya", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Kenya", !(colnames(popF1) %in% as.character(c(1949:2009)))]

# linear interpolation to get beginning of the year pop for each age group and sex
age_grp <- list("15-24" = 16:25, "25-34" = 26:35, "35-49" = 36:50, "50+"   = 51:101)
# male initial pop
pop_init_m <- numeric(length(age_grp))
names(pop_init_m) <- names(age_grp)
for (g in names(age_grp)) {
  pop_2010 <- sum(wpp_m[ age_grp[[g]], "2010" ]) * 1000
  pop_2011 <- sum(wpp_m[ age_grp[[g]], "2011" ], na.rm = TRUE) * 1000
  pop_init_m[g] <- (pop_2010 + pop_2011) / 2
}
# female initial pop
pop_init_f <- numeric(length(age_grp))
names(pop_init_f) <- names(age_grp)
for (g in names(age_grp)) {
  pop_2010 <- sum(wpp_f[ age_grp[[g]], "2010" ], na.rm = TRUE) * 1000
  pop_2011 <- sum(wpp_f[ age_grp[[g]], "2011" ], na.rm = TRUE) * 1000
  
  pop_init_f[g] <- (pop_2010 + pop_2011) / 2
}
# pop at the start of the model
pop <- cbind(pop_init_m, pop_init_f)
rownames(pop) <- names(age_grp)
colnames(pop) <- c("Male","Female")

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


#-- open pop, age stratified model ----
# new entry to 15-24 year old group only instead of entry to total popl

# Entry rates for 15y old male (current year's 15y pop / total pop aged 15–100)
entry_rates_m <- numeric(end - start + 1)
for (t in start:end) {
  if (t == 2024) {
    numerator <- wpp_m[(15 + 1), "2023"] * 1000
    denominator <- sum(wpp_m[(15 + 1):(100 + 1), "2023"], na.rm = TRUE) * 1000
  } else {
    numerator <- wpp_m[(15 + 1), as.character(t)] * 1000
    denominator <- sum(wpp_m[(15 + 1):(100 + 1), as.character(t)], na.rm = TRUE) * 1000
  }
  
  entry_rates_m[t - start + 1] <- numerator / denominator
}
entry_rate_male <- data.frame(Year = start:end, EntryRate = entry_rates_m)
entry_m_vec <- entry_rate_male$EntryRate[2:14] # 2011 not needed

# entry rates for 15y old female
entry_rates_f <- numeric(end - start + 1)
for (t in start:end) {
  if (t == 2024) {
    numerator <- wpp_f[(15 + 1), "2023"] * 1000
    denominator <- sum(wpp_f[(15 + 1):(100 + 1), "2023"], na.rm = TRUE) * 1000
  } else {
    numerator <- wpp_f[(15 + 1), as.character(t)] * 1000
    denominator <- sum(wpp_f[(15 + 1):(100 + 1), as.character(t)], na.rm = TRUE) * 1000
  }
  
  entry_rates_f[t - start + 1] <- numerator / denominator
}
entry_rate_female <- data.frame(Year = start:end, EntryRate = entry_rates_f)
entry_f_vec <- entry_rate_female$EntryRate[2:14] # 2011 not needed

#--- mortality rate: age-stratified model, 4 age groups: 15–24,25–34,35–49,50+ ---
# male mortality rate (with age group)
age_grp_m <- list("15-24" = 16:25, "25-34" = 26:35, "35-49" = 36:50, "50+" = 51:101)
mx_male_kenya <- mxM1[mxM1$name == "Kenya", c("name", "age", as.character(2010:2024))] # wpp mortality rates

calc_mort_agegrp <- function(year_int, wpp_popM, mx_male, age_grp) {
  out_vec <- sapply(seq_along(age_grp), function(g) {
    age_rows <- age_grp[[g]]
    pop_vec <- if (year_int == 2024) 
      wpp_popM[age_rows, "2023"] * 1000 
    else 
      wpp_popM[age_rows, as.character(year_int)] * 1000
    mx_vec <- if (year_int == 2024) 
      mx_male[age_rows, "2024"] 
    else 
      mx_male[age_rows, as.character(year_int)]
    sum(pop_vec * mx_vec) / sum(pop_vec)
  })
  return(out_vec)
}

mort_mat_m <- t(sapply(seq(start, end), function(current_yr) 
  calc_mort_agegrp(current_yr, wpp_m, mx_male_kenya, age_grp_m)))
dimnames(mort_mat_m) <- list(seq(start, end), names(age_grp_m))
mort_mat_m <- mort_mat_m[1:13, ] # 2024 not needed as model stops here

# female mortality rate (with age group)
age_grp_f <- list("15-24" = 16:25, "25-34" = 26:35, "35-49" = 36:50, "50+" = 51:101)
mx_female_kenya <- mxF1[mxF1$name == "Kenya", c("name", "age", as.character(2010:2024))]

calc_mort_agegrp_f <- function(year_int, wpp_popF, mx_female, age_grp) {
  out_vec <- sapply(seq_along(age_grp), function(g) {
    age_rows <- age_grp[[g]]
    pop_vec <- if (year_int == 2024) 
      wpp_popF[age_rows, "2023"] * 1000 
    else 
      wpp_popF[age_rows, as.character(year_int)] * 1000
    mx_vec <- if (year_int == 2024) 
      mx_female[age_rows, "2024"] 
    else 
      mx_female[age_rows, as.character(year_int)]
    sum(pop_vec * mx_vec) / sum(pop_vec)
  })
  return(out_vec)
}

mort_mat_f <- t(sapply(seq(start, end), function(current_yr) 
  calc_mort_agegrp_f(current_yr, wpp_f, mx_female_kenya, age_grp_f)))
dimnames(mort_mat_f) <- list(seq(start, end), names(age_grp_f))
mort_mat_f <- mort_mat_f[1:13, ] # 2024 not needed as model stops here

#--- aging rate alpha ----
# aging is same for first 2 age groups (1/10), for 3rd group it is 1/15
alpha <- c(1/10, 1/10, 1/15)


#-------stan block for open pop and age stratification---------
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
  int<lower = 1> n_yr;
  int<lower = 1> niter;
  int<lower = 1> yr_ind[niter];
  real dt;
  matrix[4, 2] pop; // r age c sex
  int<lower = 1> beta_ind[n_yr];
  int<lower = 0> n_hts;
  int<lower = 1> n_svy;
  int<lower = 1> age_up[n_svy];
  int<lower = 0> ind_hts[n_hts];
  int<lower = 0> ind_svy[n_svy];
  int hivst[n_hts];
  real se_hts[n_hts];
  
  int<lower = -999> num_svy_f[n_svy, 4]; // row n_svy, col age grp, for female
  int<lower = -999> num_svy_m[n_svy, 4]; // row n_svy, col age grp, for male
  int<lower = -999> den_svy_f[n_svy, 4]; // row n_svy, col age grp, for female
  int<lower = -999> den_svy_m[n_svy, 4]; // row n_svy, col age grp, for male
  // entry rates and death rates 
  real entry_m[n_yr];   
  real entry_f[n_yr];   
  matrix [n_yr, 4] mort_m;    
  matrix [n_yr, 4] mort_f;  
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
  real model_pred[niter, 4, 2, 4] = hivst_fun(
    niter, beta_t_dt, beta_retest, beta_male, beta_age, pop, dt,
    entry_m_dt, entry_f_dt, mort_m_dt, mort_f_dt, alpha);
  
  // Priors
  beta_t[1] ~ normal(-10,1);
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr-1)], sd_rw);
  sd_rw ~ normal(0,0.5);
  beta_retest ~ normal(log(1.2), 0.5);
  beta_male ~ normal(log(1), 0.5);
  beta_age_to_process[1] ~ normal(0, 1); // normal log follow exp(log(1) + c(-1, 1) * qnorm(0.975) * 1)
  beta_age_to_process[2] ~ normal(0, 1);
  phi ~ beta(24,6);

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

#survey and program data
ind_svy <- (c(2012.5, 2018.5, 2022.5) - start) / dt

# note: since I changed the code format, I haven't yet arranged how to enter the survey data points
# for the time being they are in this agegrp_deno and agegrp_num below which i will arrange later
# for 2022 data, the last age group 4 didnt have any data as it was a DHS survey 
# Kenya female survey denominators
agegrp_deno_f <- matrix(
  c(1900, 1983, 1897, 817,    # 2012 deno female
    2491, 2554, 4338, 1145,   # 2018 deno female
    4980, 5037, 4523, -999),    # 2022 deno female
  nrow = 4,                     #  age grp
  ncol = 3,                     # survey years
  byrow = FALSE                 
)
rownames(agegrp_deno_f) <- c("agegrp1", "agegrp2", "agegrp3", "agegrp4")
colnames(agegrp_deno_f) <- c("2012", "2018", "2022")

# Kenya female survey numerators
agegrp_num_f <- matrix(
  c(29, 45, 33, 14,    # 2012 
    89, 111, 102, 12,   # 2018 
    203, 328, 195, -999),      # 2022
  nrow = 4,                     #  age grp
  ncol = 3,                     # survey years
  byrow = FALSE                 
)
rownames(agegrp_num_f) <- c("agegrp1", "agegrp2", "agegrp3", "agegrp4")
colnames(agegrp_num_f) <- c("2012", "2018", "2022")

# Kenya male survey denominators
agegrp_deno_m <- matrix(
  c(253, 888, 611, 390,    # 2012 
    2245, 1187, 3448, 622,   # 2018 
    2691, 1181, 2334, -999),      # 2022 
  nrow = 4,                     #  age grp
  ncol = 3,                     # survey years
  byrow = FALSE                 
)
rownames(agegrp_deno_m) <- c("agegrp1", "agegrp2", "agegrp3", "agegrp4")
colnames(agegrp_deno_m) <- c("2012", "2018", "2022")

# AISHI:
# the denominator for the survey in 2022 is not 0... it was just not sampled.

# Kenya male survey numerators
agegrp_num_m <- matrix(
  c(10,31,18,7,    # 2012 
    54,54,106,14,   # 2018 
    108,181,227, -999),      # 2022
  nrow = 4,                     #  age grp
  ncol = 3,                     # survey years
  byrow = FALSE                 
)
rownames(agegrp_num_m) <- c("agegrp1", "agegrp2", "agegrp3", "agegrp4")
colnames(agegrp_num_m) <- c("2012", "2018", "2022")


# female denominator and numerator for each age group
den_svy_f <- t(agegrp_deno_f)
num_svy_f <- t(agegrp_num_f)
svy_dat_f <- num_svy_f / den_svy_f
# male denominator and numerator for each age group
den_svy_m <- t(agegrp_deno_m)
num_svy_m <- t(agegrp_num_m)
svy_dat_m <- num_svy_m / den_svy_m

age_up <- NULL
for (i in 1:nrow(den_svy_f)) {
  if (sum(den_svy_f[i, ] == -999) == 0) { age_up_i <- 4  
  } else {
      age_up_i <- which(den_svy_f[i, ] == -999) - 1
    }
  age_up <- c(age_up, age_up_i)
}


lci_svy_f <- svy_dat_f - qnorm(0.975) * sqrt(svy_dat_f * (1 - svy_dat_f) / den_svy_f)
uci_svy_f <- svy_dat_f + qnorm(0.975) * sqrt(svy_dat_f * (1 - svy_dat_f) / den_svy_f)
lci_svy_m <- svy_dat_m - qnorm(0.975) * sqrt(svy_dat_m * (1 - svy_dat_m) / den_svy_m)
uci_svy_m <- svy_dat_m + qnorm(0.975) * sqrt(svy_dat_m * (1 - svy_dat_m) / den_svy_m)

ind_hts <- (c( 2018,  2019,   2020,    2021,   2022,  2023) - start) / dt
yr_hts <- c( 2018,   2019,   2020,    2021,   2022,  2023)
hts_dat <- c(197200, 400000, 595953, 630000, 342610, 617317)
se_hts <- hts_dat * 0.1


# data for fitting and running (I need to update for age grp)
data_stan <- list(n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                  pop = pop,
                  beta_ind = beta_ind,
                  n_svy = length(ind_svy),
                  age_up = age_up,
                  n_hts = length(hts_dat),
                  ind_svy = ind_svy,
                  ind_hts = ind_hts,
                  hivst = hts_dat,
                  se_hts = se_hts,
                  # changed this for age grp
                  num_svy_f = num_svy_f,
                  num_svy_m = num_svy_m,
                  den_svy_f = den_svy_f,
                  den_svy_m = den_svy_m,
                  # new rates for open pop 
                  entry_m = entry_m_vec,
                  entry_f = entry_f_vec,
                  mort_m = mort_mat_m,
                  mort_f = mort_mat_f,
                  # aging
                  alpha = alpha
)
rstan_options(auto_write = TRUE)

#981367

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

#-----------------------------------------------
# format to male and female by age groups
 as.numeric(row[grepl("^Spl_T", colnames(grid_chunk))])

svy_m_ <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f_ <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)

svy_m <- list()
svy_f <- list()
for (a in 1:4) {
  svy_m[[a]] <- svy_m_[grepl(paste0(",", a, "]", sep = ""), rownames(svy_m_)), ]
  svy_f[[a]] <- svy_f_[grepl(paste0(",", a, "]", sep = ""), rownames(svy_f_)), ]
}

hts <- as.data.frame(rstan::summary(fit, pars = c("hivst_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r <- as.data.frame(rstan::summary(fit, pars = c("beta_t"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(r$`50%`)
rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr$`50%`)
exp(rr$`2.5%`)
exp(rr$`97.5%`)


rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m$`50%`)
exp(rr_m$`2.5%`)
exp(rr_m$`97.5%`)

rr_a <- as.data.frame(rstan::summary(fit, pars = c("beta_age_to_process"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_a$`50%`)
exp(rr_a$`2.5%`)
exp(rr_a$`97.5%`)


phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`



# plot ( I will modify it by age structure)
options(scipen = 999)
par(mfrow = c(1, 3), oma = c(0, 0, 0, 0), mar = c(4, 4, 3, 1))
# survey fit
col_age_m <- c("steelblue4", "turquoise3", "seagreen1", "goldenrod3")
col_age_f <- c("violetred4", "orange2", "cornsilk4", "maroon3")
# we plot men first
plot(svy_m[[1]]$`50%` ~ time, type = "l", col = col_age_m[1], 
     lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2),
     main = "Men")
for (a in 1:4) {
 polygon(x = c(time, rev(time)),
        y = c(svy_m[[a]]$`2.5%`, rev(svy_m[[a]]$`97.5%`)),
        col = yarrr::transparent(col_age_m[a], trans.val = 0.5), border = NA)
 lines(svy_m[[a]]$`50%` ~ time, col = col_age_m[a], lwd = 3) 
 points(svy_dat_m[, a] ~ time[ind_svy], pch = 16, col = col_age_m[a])
  segments(x0 = time[ind_svy], y0 = lci_svy_m[, a],
         x1 = time[ind_svy], y1 = uci_svy_m[, a], col = col_age_m[a])  
}
legend("topleft", legend = c("15-24", "25-34", "35-49", "50+"),
       col = col_age_m, lwd = 2, bty = "n")

# we plot women
plot(svy_f[[1]]$`50%` ~ time, type = "l", col = col_age_f[1], 
     lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2),
     main = "Women")
for (a in 1:4) {
 polygon(x = c(time, rev(time)),
        y = c(svy_f[[a]]$`2.5%`, rev(svy_f[[a]]$`97.5%`)),
        col = yarrr::transparent(col_age_f[a], trans.val = 0.5), border = NA)
 lines(svy_f[[a]]$`50%` ~ time, col = col_age_f[a], lwd = 3) 
 points(svy_dat_f[, a] ~ time[ind_svy], pch = 16, col = col_age_f[a])
  segments(x0 = time[ind_svy], y0 = lci_svy_f[, a],
         x1 = time[ind_svy], y1 = uci_svy_f[, a], col = col_age_f[a])  
 }
legend("topleft", legend = c("15-24", "25-34", "35-49", "50+"),
       col = col_age_f, lwd = 2, bty = "n")

# hts fit
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ I(yr_hts + 0.5), pch = 16, col = "goldenrod3", cex = 1.25)
mtext("Kenya", outer = TRUE, side = 3, line = 1, cex = 1.5)


# AISHI. We have the most disaggregated plot here. But it would be good to have
# the one by sex again (so take the age-disaggregated results and add them up).

#pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
#pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
#pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
#pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
#pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
#pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
#pop <- c(pop_2011_0_m, pop_2011_0_f)

# weighted male mortality rate (without age group): not using now
#mx_male_kenya <- mxM1[mxM1$name == "Kenya", c("name", "age", as.character(2010:2024))]

# wt_mort_rate_m <- numeric(end - start + 1)
# for (t in as.character(start:end)) 
# {
# if (t == "2024") {pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
# mort_male <- mx_male_kenya[(15 + 1):(100 + 1), "2024"]
# } else {
# pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
# mort_male <- mx_male_kenya[(15 + 1):(100 + 1), t]
# }
# wt_deaths_m <- (pop_male) * (mort_male)
# tot_deaths_m <- sum(wt_deaths_m)
# tot_pop_m <- sum(pop_male)
# wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
# }

# wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)

# female mortality rate
# mx_female_kenya <- mxF1[mxF1$name == "Kenya", c("name", "age", as.character(2010:2024))]

# wt_mort_rate_f <- numeric(end - start + 1)
# for (t in as.character(start:end)) {
#  if (t == "2024") {
   # pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
   # mort_female <- mx_female_kenya[(15 + 1):(100 + 1), "2024"]
 # } else {
 #   pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
  #  mort_female <- mx_female_kenya[(15 + 1):(100 + 1), t]
 # }
#  wt_deaths_f <- (pop_female) * (mort_female)
#  tot_deaths_f <- sum(wt_deaths_f)
 # tot_pop_f <- sum(pop_female)
#  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
# }
# wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

