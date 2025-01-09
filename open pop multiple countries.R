
# all 26 countries with program data

rm(list = ls())
gc()

library(wpp2024)
library(rstan)
library(LaplacesDemon)
library(ggplot2)
library(scales)
library(patchwork)

# male & female pop data from wpp
data(popM1)
data(popF1)
# mortality data
data(mxM1)   
data(mxF1) 

#---start of year pop---
countries <- c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
               "Sierra Leone", "Zambia", "Mali", "Uganda",
               "Lesotho", "Mozambique", "Rwanda",
               "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
                "Guinea", "Liberia", "Senegal", "South Africa", 
               "United Republic of Tanzania", "Namibia", "Botswana", 
               "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini")

get_pop_2011 <- function(cnt_name) {
  wpp_m <- popM1[popM1$name == cnt_name, !(colnames(popM1) %in% as.character(1949:2009))]
  wpp_f <- popF1[popF1$name == cnt_name, !(colnames(popF1) %in% as.character(1949:2009))]
  
  pop_2010_m <- sum(wpp_m[16:101, "2010"]) * 1000
  pop_2011_m <- sum(wpp_m[16:101, "2011"]) * 1000
  pop_2010_f <- sum(wpp_f[16:101, "2010"]) * 1000
  pop_2011_f <- sum(wpp_f[16:101, "2011"]) * 1000
  
  pop_2011_m <- (pop_2010_m + pop_2011_m) / 2
  pop_2011_f <- (pop_2010_f + pop_2011_f) / 2
  
  return(c(pop_2011_m, pop_2011_f))
}

pop <- sapply(countries, get_pop_2011) #(rows=sex,n col=cnt)

#----time----
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

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

# matrix [row:y,col:cnt]  
# matrix for passing to stan, need to declare in data block as matrix and also enter in data_stan
entry_m_vec <- do.call(cbind, lapply(countries, function(cn) get_entry_rates_m(cn, start, end)$EntryRate_m[2:14]))


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

entry_f_vec <- do.call(cbind, lapply(countries, function(cn_f) get_entry_rates_f(cn_f, start, end)$EntryRate_f[2:14]))

#------- death rates ------
# male
get_mort_rate_m <- function(cn, start, end) {
  wpp_m <- popM1[popM1$name == cn, !(colnames(popM1) %in% as.character(1949:2009))]
  mx_male <- mxM1[mxM1$name == cn, c("name", "age", as.character(2010:2024))]
  wt_mort_rate_m <- numeric(end - start + 1)
  for (t in as.character(start:end)) {
    if (t == "2024") {
      pop_male <- wpp_m[16:101, "2023"] * 1000
      mort_male <- mx_male[16:101, "2024"]
    } else {
      pop_male <- wpp_m[16:101, t] * 1000
      mort_male <- mx_male[16:101, t]
    }
    wt_deaths_m <- pop_male * mort_male
    tot_deaths_m <- sum(wt_deaths_m)
    tot_pop_m <- sum(pop_male)
    wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
  }
  return(wt_mort_rate_m)
}

# mortality matrix [r:y,c:cnt]
mort_m_vec <- do.call(cbind, lapply(countries, function(cn) get_mort_rate_m(cn, start, end)[1:13]))

# female
get_mort_rate_f <- function(cn, start, end) {
  wpp_f <- popF1[popF1$name == cn, !(colnames(popF1) %in% as.character(1949:2009))]
  mx_female <- mxF1[mxF1$name == cn, c("name", "age", as.character(2010:2024))]
  wt_mort_rate_f <- numeric(end - start + 1)
  for (t in as.character(start:end)) {
    if (t == "2024") {
      pop_female <- wpp_f[16:101, "2023"] * 1000
      mort_female <- mx_female[16:101, "2024"]
    } else {
      pop_female <- wpp_f[16:101, t] * 1000
      mort_female <- mx_female[16:101, t]
    }
    wt_deaths_f <- pop_female * mort_female
    tot_deaths_f <- sum(wt_deaths_f)
    tot_pop_f <- sum(pop_female)
    wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
  }
  return(wt_mort_rate_f)
}

mort_f_vec <- do.call(cbind, lapply(countries, function(cn) get_mort_rate_f(cn, start, end)[1:13]))

#-------mapping HIVST rate to the appropriate yearly indices-----
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)


#---stan code open pop multiple countries-----
hivst_mod <- '
functions {
  real[, , ] hivst_fun(
    int niter,
    vector beta_t_dt,
    real beta_retest,
    real beta_male,
    vector pop,
    real dt,
    vector entry_m_dt, // kept vector as before as it will loop over each country
    vector entry_f_dt,
    vector mort_m_dt,
    vector mort_f_dt) { 
    
  // converting rates from log scale to exponent
    vector[niter] rr_t = beta_t_dt;
    real rr_r = beta_retest;
    real rr_m = beta_male;
    
    real out[niter,4,2];
    out = rep_array(0.0, niter,4,2);
    
    // Initialization
    out[1,1,1] = pop[1];  // Males, never tested
    out[1,1,2] = pop[2];  // Females, never tested
    
    for (i in 2:niter) {
    // Males
    out[i,1,1] = out[i-1,1,1] + dt * (entry_m_dt[i] * (out[i-1,1,1] + out[i-1,2,1]) - rr_t[i] * rr_m * out[i-1,1,1] - mort_m_dt[i] * out[i-1,1,1]);
    out[i,2,1] = out[i-1,2,1] + dt * (rr_t[i] * rr_m * out[i-1,1,1] - mort_m_dt[i] * out[i-1,2,1]);
    out[i,3,1] = rr_t[i] * rr_m * (out[i-1,1,1] + rr_r * out[i-1,2,1]);
    out[i,4,1] = out[i, 2, 1] / (out[i,1,1] + out[i,2,1]);
  
    // Females
    out[i,1,2] = out[i-1,1,2] + dt * (entry_f_dt[i] * (out[i-1,1,2] + out[i-1,2,2]) - rr_t[i] * out[i-1,1,2] - mort_f_dt[i] * out[i-1,1,2]);
    out[i,2,2] = out[i-1,2,2] + dt * (rr_t[i] * out[i-1,1,2] - mort_f_dt[i] * out[i-1,2,2]);
    out[i,3,2] = rr_t[i] * (out[i-1,1,2] + rr_r * out[i-1,2,2]);
    out[i,4,2] = out[i,2,2] / (out[i,1,2] + out[i,2,2]);
    }
return out;
  }
}

data {
  int<lower = 1> n_cnt;             // nb countries
  int<lower = 1> n_yr;
  int<lower = 1> niter;           // nb iterations (same for all countries)
  int<lower = 1> tot_svy;         // nb of surveys accross all countries
  int<lower = 1> tot_hts;         // nb of year with hivst data all countries
  int<lower = 1> yr_ind[niter];
  real dt;
  matrix[2, n_cnt] pop;               // row sex col cnt
  
  // for prgrm data
  int<lower = 1> n_hts_by_cnt[n_cnt];   // nb of years with observed HIVST data per cntry
  int<lower = 1> ind_hts[tot_hts]; // indices pgm data
  int<lower = 1> hivst[tot_hts];   // nb of HIVST performed per year per cnty
  real se_hts[tot_hts];            // se for program data
  
  // for survey data
  int<lower = 1> n_svy_by_cnt[n_cnt];     // nb of surveys per country
  int<lower = 1> ind_svy[tot_svy]; // indices for survey data
  int<lower = 1> num_svy[tot_svy, 2]; // numerator for survey proportions
  int<lower = 1> den_svy[tot_svy, 2]; // denominator for survey proportions
  
  // indices for the unlist data
  int<lower = 1> svy_idx_s[n_cnt];
  int<lower = 1> svy_idx_e[n_cnt]; 
  int<lower = 1> hts_idx_s[n_cnt];
  int<lower = 1> hts_idx_e[n_cnt];

  // entry and exit rates for open pop multiple countries
  matrix[n_yr, n_cnt] entry_m;
  matrix[n_yr, n_cnt] entry_f;
  matrix[n_yr, n_cnt] mort_m;
  matrix[n_yr, n_cnt] mort_f;
}

parameters {
  matrix<upper = -1>[n_cnt, n_yr] beta_t;          // yearly HIVST rates (rw1) for each country
  real<lower = 1e-6, upper = 5> sd_rw;    // sd of the rw1 for beta_t
  real<lower = 1e-6, upper = 5> sd_phi;    // sd of the RE for phi
  real<lower = 1e-6, upper = 5> sd_rt;    // sd of the RE for the re-testing rate ratio
  real<lower = 1e-6, upper = 5> sd_men;    // sd of the RE of the male rate ratio
  real beta_retest_overall;            // overall shared re-testing rate
  vector[n_cnt] beta_rt_raw;            // country-specific re-testing rates
  real beta_men_overall;               //overall male relative rate of HIVST
  vector[n_cnt] beta_men_raw;          // country specific male relative rate of HIVST  
  real phi_overall;           // overall proportion of HIVST kits used
  vector[n_cnt] phi_raw;      // country-specific proportions of HIVST kits used
}

transformed parameters {
// new non-centered parameterization
  vector[n_cnt] beta_retest;
  vector[n_cnt] beta_male;
  vector[n_cnt] phi;
  
  beta_retest = 0.5 + (2.5 - 0.5) * inv_logit(beta_retest_overall + sd_rt * beta_rt_raw);
  beta_male = exp(beta_men_overall + sd_men * beta_men_raw);
  phi = 0.5 + (1 - 0.5) * inv_logit(phi_overall + sd_phi * phi_raw);

  // entry and death rates
  matrix[n_cnt, niter] beta_t_dt;
  matrix[n_cnt, niter] entry_m_dt;
  matrix[n_cnt, niter] entry_f_dt;
  matrix[n_cnt, niter] mort_m_dt;
  matrix[n_cnt, niter] mort_f_dt;

  for (c in 1:n_cnt) {
    for (i in 1:niter) {
      int year = yr_ind[i];
      beta_t_dt[c, i] = exp(beta_t[c, year]);
      entry_m_dt[c, i] = entry_m[year, c]; // extracting in correct format as entrymvec has years as rows and cnt as col
      entry_f_dt[c, i] = entry_f[year, c];
      mort_m_dt[c, i] = mort_m[year, c];
      mort_f_dt[c, i] = mort_f[year, c];
    }
  }
}

model {
    matrix[niter, n_cnt] hts_mod;
  
  // priors
  // overall prior for the SD of the RW1 for testing rate
  sd_rw ~ normal(0, 0.25) T[1e-6, 5];
  sd_phi ~ normal(0, 0.5) T[1e-6, 5];
  sd_rt ~ normal(0, 0.5) T[1e-6, 5];
  sd_men ~ normal(0, 0.5) T[1e-6, 5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(logit(1.2 - 0.5) / (2.5 - 0.5), 0.5); // 0.5 + (2.5 - 0.5) * plogis(qlogis((1.2 - 0.5) / (2.5 - 0.5)) + c(-1, 1) * qnorm(0.975) * 0.5)
  // overall prior for the % of tests distributed being used
  phi_overall ~ normal(logit((0.85 - 0.5) / (1 - 0.5)), 1); // 0.5 + (1 - 0.5) * plogis(qlogis((0.85 - 0.5) / (1-0.5)) + c(-1, 1) * qnorm(0.975) * 1)
  beta_men_overall ~ normal(log(1), 0.5);
  
  beta_rt_raw ~ std_normal();
  beta_men_raw ~ std_normal();
  phi_raw ~ std_normal();
    
  // country-specific priors
  for (c in 1:n_cnt) {
    beta_t[c, 1] ~ normal(-10, 1); // exp(-10 + c(-1, 1) * 1.96 * 1)
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);


  // model predictions and likelihoods 
    real model_pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], pop[, c], 
    dt, to_vector(entry_m_dt[c, ]), to_vector(entry_f_dt[c, ]), to_vector(mort_m_dt[c, ]), to_vector(mort_f_dt[c, ]));

    // fitting to survey data (layer of country and surveys)
      num_svy[svy_idx_s[c]:svy_idx_e[c], 1] ~ binomial(den_svy[svy_idx_s[c]:svy_idx_e[c], 1], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 1]);
      num_svy[svy_idx_s[c]:svy_idx_e[c], 2] ~ binomial(den_svy[svy_idx_s[c]:svy_idx_e[c], 2], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 2]);

    // fitting to program data
    hts_mod[, c] = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / phi[c];
    hivst[hts_idx_s[c]:hts_idx_e[c]] ~ normal(hts_mod[ind_hts[hts_idx_s[c]:hts_idx_e[c]], c], 
                                              se_hts[hts_idx_s[c]:hts_idx_e[c]]);
  }
}

generated quantities {
    matrix[n_cnt, niter] hivst_prd;  // predicted HIVST rates per time step for all countries
    matrix[n_cnt, niter] svy_prd_m;  // predicted survey proportions (males) per country
    matrix[n_cnt, niter] svy_prd_f;  // predicted survey proportions (females) per country

    for (c in 1:n_cnt) {
    real pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], pop[, c], 
    dt, to_vector(entry_m_dt[c, ]), to_vector(entry_f_dt[c, ]), to_vector(mort_m_dt[c, ]), to_vector(mort_f_dt[c, ]));
        
        // survey prediction
            svy_prd_m[c, ] = to_row_vector(pred[, 4, 1]);  // males ever used HIVST
            svy_prd_f[c, ] = to_row_vector(pred[, 4, 2]);  // females ever used HIVST
        
        // hts predictions 
        hivst_prd[c, ] = to_row_vector(to_vector(pred[, 3, 1]) + to_vector(pred[, 3, 2])) / phi[c];
        }
}
'

# Compiling the model and exposing functions
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

#------ Survey & program data for multiple countries ------------
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
    yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
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
  ),
  zambia = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(3756,2869)),
    num_svy = round(cbind(104,82)),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(315348, 781175, 639225, 23750, 33153, 95559),
    se_hts = c(315348, 781175, 639225, 23750, 33153, 95559) * 0.1
  ),
  mali = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(3740,2990)),
    num_svy = round(cbind(15,31)),
    yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7763, 169962, 11375, 235729),
    se_hts = c(7763, 169962, 11375, 235729) * 0.1
  ),
  uganda = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy = round(cbind(3640,8640)),
    num_svy = round(cbind(203,389)),
    yr_hts = c(2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(42570, 306421, 750698, 681602),
    se_hts = c(42570, 306421, 750698, 681602) * 0.1
  ),
  lesotho  = list(
    yr_svy =  c(2020.5, 2023.5),
    ind_svy = (c(2020.5, 2023.5) - start) / dt,
    den_svy = round(cbind(c(4729, 2284), c(5038,5131))),
    num_svy = round(cbind(c(474, 837), c(435, 2529))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(58917, 42650, 164236, 281277, 301762, 262915),
    se_hts = c(58917, 42650, 164236, 281277, 301762, 262915) * 0.1
  ),
  mozambique = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy = round(cbind(1483,1590)),
    num_svy = round(cbind(128,174)),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(67883, 203966, 683345),
    se_hts = c(67883, 203966, 683345) * 0.1
  ),
  rwanda = list(
    yr_svy =  2019.5,
    ind_svy = (2019.5 - start) / dt,
    den_svy = round(cbind(3840,10700)),
    num_svy = round(cbind(62,140)),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 62683,
    se_hts = 62683 * 0.1
  ),
burkinafaso = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy = round(cbind(3570,11000)),
  num_svy = round(cbind(18,27)),
  yr_hts = c(2022, 2023) + 0.5,
  ind_hts = (c(2022, 2023) - start + 0.5) / dt,
  hts_dat = c(968, 3367),
  se_hts = c(968, 3367) * 0.1
),
burundi = list(
  yr_svy =  2016.5,
  ind_svy = (2016.5 - start) / dt,
  den_svy = round(cbind(6620,6340)),
  num_svy = round(cbind(25,16)),
  yr_hts = 2023 + 0.5,
  ind_hts = (2023 - start + 0.5) / dt,
  hts_dat = 78013,
  se_hts = 78013 * 0.1
),
cameroon = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy = round(cbind(2660,7090)),
  num_svy = round(cbind(90,147)),
  yr_hts = c(2021, 2022) + 0.5,
  ind_hts = (c(2021,2022) - start + 0.5) / dt,
  hts_dat = c(15000, 33073),
  se_hts = c(15000,33073) * 0.1
),
cotedivoire = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy = round(cbind(2490,3900)),
  num_svy = round(cbind(31,41)),
  yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(1159, 111184, 117556, 41774, 60154),
  se_hts = c(1159, 111184, 117556, 41774, 60154) * 0.1
),
guinea = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy = round(cbind(3580,3570)),
  num_svy = round(cbind(14,29)),
  yr_hts = c(2019, 2022) + 0.5,
  ind_hts = (c(2019, 2022) - start + 0.5) / dt,
  hts_dat = c(12, 152),
  se_hts = c(12, 152) * 0.1
),
liberia = list(
  yr_svy =  2019.5,
  ind_svy = (2019.5 - start) / dt,
  den_svy = round(cbind(2350,3630)),
  num_svy = round(cbind(29,55)),
  yr_hts = 2023 + 0.5,
  ind_hts = (2023 - start + 0.5) / dt,
  hts_dat = 12129,
  se_hts = 12129 * 0.1
),
senegal = list(
  yr_svy =  2017.5,
  ind_svy = (2017.5 - start) / dt,
  den_svy = round(cbind(2080,12000)),
  num_svy = round(cbind(3,21)),
  yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(7307, 18860, 5505, 4056, 11932),
  se_hts = c(7307, 18860, 5505, 4056, 11932) * 0.1
),
southafrica = list(
  yr_svy =  2016.5,
  ind_svy = (2016.5 - start) / dt,
  den_svy = round(cbind(1150,4290)),
  num_svy = round(cbind(30,127)),
  yr_hts = c(2018, 2019, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(1200000, 794034, 913418, 212000),
  se_hts = c(1200000, 794034, 913418, 212000) * 0.1
),
tanzania = list(
  yr_svy =  2022.5,
  ind_svy = (2022.5 - start) / dt,
  den_svy = round(cbind(3110,4980)),
  num_svy = round(cbind(141,160)),
  yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(25810, 14940, 19000, 38717, 809603, 1447029),
  se_hts = c(25810, 14940, 19000, 38717, 809603, 1447029) * 0.1
),
namibia = list(
  yr_svy =  2017.5,
  ind_svy = (2017.5 - start) / dt,
  den_svy = round(cbind(3651,3881)),
  num_svy = round(cbind(148,93)),
  yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(3910, 40075, 47258, 130000, 27974),
  se_hts = c(3910, 40075, 47258, 130000, 27974) * 0.1
),
botswana = list(
  yr_svy =  2021.5,
  ind_svy = (2021.5 - start) / dt,
  den_svy = round(cbind(3108,2911)),
  num_svy = round(cbind(59,68)),
  yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
  ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
  hts_dat = c(7000, 3848, 8403, 16405),
  se_hts = c(7000, 3848, 8403, 16405) * 0.1
),
guineabissau = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy = round(cbind(376,2518)),
  num_svy = round(cbind(15,63)),
  yr_hts = 2020 + 0.5,
  ind_hts = (2020 - start + 0.5) / dt,
  hts_dat = 37500,
  se_hts = 37500 * 0.1
),
drc = list(
  yr_svy =  2018.5,
  ind_svy = (2018.5 - start) / dt,
  den_svy = round(cbind(4891,17405)),
  num_svy = round(cbind(146,296)),
  yr_hts = c(2018, 2020, 2021) + 0.5,
  ind_hts = (c(2018, 2020, 2021) - start + 0.5) / dt,
  hts_dat = c(500, 7158, 7480),
  se_hts = c(500, 7158, 7480) * 0.1
),
eswatini = list(
  yr_svy = c(2021.5, 2022.5),
  ind_svy = (c(2021.5, 2022.5) - start) / dt,
  den_svy = round(cbind(c(2040, 1327), c(3394, 1606))),
  num_svy = round(cbind(c(399, 339), c(694, 494))),
  yr_hts = c(2018, 2019, 2020, 2021, 2022) + 0.5,
  ind_hts = (c(2018, 2019, 2020, 2021, 2022) - start + 0.5) / dt,
  hts_dat = c(33159, 32531, 191990, 78570, 111912),
  se_hts = c(33159, 32531, 191990, 78570, 111912) * 0.1
)
)


# list of survey years for each country
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

# adding svy_dat, lci,uci after list as the operations cant be performed inside list
cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat <- x$num_svy / x$den_svy
  x$lci_svy <- x$svy_dat - qnorm(0.975) * sqrt(x$svy_dat * (1 - x$svy_dat) / x$den_svy)
  x$uci_svy <- x$svy_dat + qnorm(0.975) * sqrt(x$svy_dat * (1 - x$svy_dat) / x$den_svy)
  return(x)
})

# combining svy and pgm data for all countries
num_svy <- do.call(rbind, lapply(cnt_data, function(x) x$num_svy)) #rows=countries, cols=surveys
den_svy <- do.call(rbind, lapply(cnt_data, function(x) x$den_svy))
ind_svy <- unlist(lapply(cnt_data, function(x) x$ind_svy))
hts_dat <- unlist(lapply(cnt_data, function(x) x$hts_dat)) #rows=countries, cols=time points
se_hts <- unlist(lapply(cnt_data, function(x) x$se_hts))
ind_hts <- unlist(lapply(cnt_data, function(x) x$ind_hts))

# fitting data for running in stan
data_stan <- list(
  n_cnt = length(cnt_data),            
  n_yr = n_yr,                         
  yr_ind = yr_ind,                     
  niter = niter,                       
  dt = dt,    
  pop = pop,                           
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
  num_svy = num_svy,                   
  den_svy = den_svy,
  # new rates for open pop 
  entry_m = entry_m_vec,
  entry_f = entry_f_vec,
  mort_m = mort_m_vec,
  mort_f = mort_f_vec
)
rstan_options(auto_write = TRUE)


# fitting the model
options(mc.cores = parallel::detectCores())
init_function <- function() {
  list(
    beta_t = matrix(rnorm(data_stan$n_cnt * data_stan$n_yr, -5, 0.1), 
                    ncol = data_stan$n_yr, nrow = data_stan$n_cnt),
    sd_rw = runif(1, min = 0.1, max = 1),
    sd_phi = runif(1, min = 0.1, max = 1),
    sd_rt = runif(1, min = 0.1, max = 1),
    sd_men = runif(1, min = 0.1, max = 1),
    beta_restest_overall = rnorm(1, qlogis((1.2 - 0.5) / (2.5 - 0.5)), 0.1),
    beta_rt_raw = rnorm(data_stan$n_cnt, 0, 0.5),
    beta_men_overall = rnorm(1, log(1), 0.1),
    beta_men_raw = rnorm(data_stan$n_cnt, 0, 0.1),
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
traceplot(fit, pars = "sd_men")
traceplot(fit, pars = "beta_retest_overall")
traceplot(fit, pars = "beta_rt_raw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_men_overall")
traceplot(fit, pars = "beta_men_raw")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi_overall")
traceplot(fit, pars = "phi_raw")
traceplot(fit, pars = "phi")

#summary(fit)

# parameters

# testing rate
pars_beta_t <- matrix(paste0("beta_t[", rep(1:data_stan$n_cnt, each = data_stan$n_yr), ",", rep(1:data_stan$n_yr, data_stan$n_cnt), "]"), 
              nrow = data_stan$n_cnt, ncol = data_stan$n_yr, byrow = TRUE)
r <- as.data.frame(rstan::summary(fit, pars = pars_beta_t, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r$`50%`
max(r$`50%`)
max(r$`97.5%`)
 

# ------ retesting rate ratio --------
rr_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_retest_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (2.5 - 0.5) * invlogit(rr_overall$`50%`)
0.5 + (2.5 - 0.5) * invlogit(rr_overall$`2.5%`)
0.5 + (2.5 - 0.5) * invlogit(rr_overall$`97.5%`)

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
                             median = 0.5 + (2.5 - 0.5) * invlogit(rr_overall$`50%`),
                             lci = 0.5 + (2.5 - 0.5) * invlogit(rr_overall$`2.5%`),
                             uci = 0.5 + (2.5 - 0.5) * invlogit(rr_overall$`97.5%`)))

# Renaming selected countries
df_rr_ov$country <- as.character(df_rr_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
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



#-------- rate ratio male ---------------
rr_m_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_men_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_m$country[df_rr_m$country == old_name] <- rename_map[[old_name]]
}

cap_first_letter <- function(x) {
  if (x == "overall") return("Overall")   
  if (nchar(x) == 0)   return(x)      
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
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


# ------- phi -----------------
phi_overall <- as.data.frame(rstan::summary(fit, pars = c("phi_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (1 - 0.5) * invlogit(phi_overall$`50%`)
0.5 + (1 - 0.5) * invlogit(phi_overall$`2.5%`)
0.5 + (1 - 0.5) * invlogit(phi_overall$`97.5%`)
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
                              median = 0.5 + (1 - 0.5) * invlogit(phi_overall$`50%`),
                              lci = 0.5 + (1 - 0.5) * invlogit(phi_overall$`2.5%`),
                              uci = 0.5 + (1 - 0.5) * invlogit(phi_overall$`97.5%`)))

# Renaming selected countries
df_phi_ov$country <- as.character(df_phi_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_phi_ov$country[df_phi_ov$country == old_name] <- rename_map[[old_name]]
}

cap_first_letter <- function(x) {
  if (x == "overall") return("Overall")   
  if (nchar(x) == 0)   return(x)      
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
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



#------- step to check each country's pred with program point-----------------
# function to plot individual output
svy_m_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts_all <- as.data.frame(rstan::summary(fit, pars = c("hivst_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe", 
                   "sierraleone", "zambia", "mali", "uganda",
                   "lesotho", "mozambique", "rwanda",
                   "burkinafaso", "burundi", "cameroon", "cotedivoire",
                   "guinea", "liberia", "senegal", "southafrica","tanzania",
                   "namibia", "botswana", "guineabissau","drc", "eswatini")

plot_country_fit <- function(c_idx, cnt_lowercase, time, 
                             svy_m_all, svy_f_all, hts_all,
                             cnt_data,
                             niter, using_layout = FALSE) {
  cn <- cnt_lowercase[c_idx] # to match cnt_data keys exactly
  
  start_row <- (c_idx - 1)  * niter + 1
  end_row <- c_idx * niter
   
  svy_m_country <- svy_m_all[start_row:end_row, ]
  svy_f_country <- svy_f_all[start_row:end_row, ]
  hts_country <- hts_all[start_row:end_row, ]
  
  svy_dat_c <- cnt_data[[cn]]$svy_dat
  lci_svy_c <- cnt_data[[cn]]$lci_svy
  uci_svy_c <- cnt_data[[cn]]$uci_svy
  ind_svy_c <- cnt_data[[cn]]$ind_svy
  
  hts_dat_c <- cnt_data[[cn]]$hts_dat
  ind_hts_c <- cnt_data[[cn]]$ind_hts
  
  if (using_layout == FALSE) { 
    par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1)) 
  }
  plot(svy_m_country$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, 
       ylab = "Ever used HIVST", ylim = c(0, max(svy_m_country$`97.5%`, svy_f_country$`97.5%`, max(uci_svy_c))),
       main = paste0(toupper(substring(cn, 1, 1)), substring(cn, 2)))
  
  polygon(x = c(time, rev(time)),
          y = c(svy_m_country$`2.5%`, rev(svy_m_country$`97.5%`)),
          col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
  
  polygon(x = c(time, rev(time)),
          y = c(svy_f_country$`2.5%`, rev(svy_f_country$`97.5%`)),
          col = yarrr::transparent("pink3", trans.val = 0.5), border = NA)
  
  lines(svy_f_country$`50%` ~ time, col = "pink3", lwd = 3)
  
  points(svy_dat_c[, 1] ~ time[ind_svy_c], pch = 16, col = "blue4")
  points(svy_dat_c[, 2] ~ time[ind_svy_c], pch = 16, col = "firebrick4")
  
  segments(x0 = time[ind_svy_c], y0 = lci_svy_c[, 1],
           x1 = time[ind_svy_c], y1 = uci_svy_c[, 1], col = "blue4")
  segments(x0 = time[ind_svy_c], y0 = lci_svy_c[, 2],
           x1 = time[ind_svy_c], y1 = uci_svy_c[, 2], col = "firebrick4")
  
  legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")
  
  plot(hts_country$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, 
       ylab = "Number of HIVST kits",
       ylim = c(0, max(hts_country$`97.5%`)))
  
  polygon(x = c(time, rev(time)),
          y = c(hts_country$`2.5%`, rev(hts_country$`97.5%`)),
          col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
  
  points(hts_dat_c ~ time[ind_hts_c], pch = 16, col = "goldenrod3", cex = 1.25)
  
  mtext(paste0(toupper(substring(cn, 1, 1)), substring(cn, 2)), 
        outer = TRUE, side = 3, line = 1, cex = 1.5)
}

png("./figs/plots.png", width = 8, height = 26, units = "in", res = 320)
nf <- layout(mat = matrix(1:52, nrow = 13, ncol = 4, byrow = TRUE),
       widths = rep(lcm(5), 4), heights = rep(lcm(5), 13), respect = TRUE)
par(oma = c(0, 0, 0, 0), mar = c(3, 4, 2, 1))
alphabetical_cnt <- order(cnt_lowercase)
for (c_idx in alphabetical_cnt) {
  plot_country_fit(c_idx, cnt_lowercase, time, svy_m_all, svy_f_all, hts_all, cnt_data, niter, using_layout = TRUE)
}
dev.off()


#----verification step: checking wpp pop with model predictions-----------

post <- rstan::extract(fit)
beta_t_median <- apply(post$beta_t, c(2,3), median)  # beta_t_median is now [n_cnt, n_yr]
beta_retest_median <- apply(post$beta_retest, 2, median) # length n_cnt
beta_male_median <- apply(post$beta_male, 2, median)     # length n_cnt
phi_median <- apply(post$phi, 2, median)                 # length n_cnt

model_results <- list()
for (c_idx in 1:n_cnt) {
    beta_t_dt <- beta_t_median[c_idx, yr_ind]
    entry_m_dt <- entry_m_vec[yr_ind, c_idx]
  entry_f_dt <- entry_f_vec[yr_ind, c_idx]
  mort_m_dt <- mort_m_vec[yr_ind, c_idx]
  mort_f_dt <- mort_f_vec[yr_ind, c_idx]
  pop_c <- pop[, c_idx] 
  beta_retest_c <- beta_retest_median[c_idx]
  beta_male_c <- beta_male_median[c_idx]
  phi_c <- phi_median[c_idx]
  
  # calling the function
  model_out_raw <- hivst_fun(
    niter = niter,
    beta_t_dt = beta_t_dt,
    beta_retest = beta_retest_c, 
    beta_male = beta_male_c,     
    pop = pop_c,
    dt = dt,
    entry_m_dt = entry_m_dt,
    entry_f_dt = entry_f_dt,
    mort_m_dt = mort_m_dt,
    mort_f_dt = mort_f_dt
  )
  
  model_array <- array(NA, dim = c(niter, 4, 2))
  for (i in 1:niter) {
    for (j in 1:4) {
      model_array[i, j, ] <- model_out_raw[[i]][[j]]
    }
  }
  
  model_results[[c_idx]] <- model_array
}

steps_per_year <- 1/dt
year_seq <- start:(end-1)

# male pop verification
comparison_list <- list()
for (c_idx in seq_along(countries)) {
  cn <- countries[c_idx]
  model_array <- model_results[[c_idx]]  
  total_pop_male <- model_array[, 1, 1] + model_array[, 2, 1]
  wpp_m_country <- popM1[popM1$name == cn, !(colnames(popM1) %in% as.character(c(1949:2009)))]
  
  df_compare <- data.frame(
    Year = year_seq,
    ModelMale = NA_real_,
    WPPMale = NA_real_
  )
  
  for (y_i in seq_along(year_seq)) {
    year <- year_seq[y_i]
    mid_i <- (y_i - 1)*steps_per_year + (steps_per_year / 2)
    model_male_pop_mid_year <- total_pop_male[mid_i]
    wpp_male_mid_year <- sum(wpp_m_country[16:101, as.character(year)], na.rm = TRUE)*1000
    df_compare$ModelMale[y_i] <- model_male_pop_mid_year
    df_compare$WPPMale[y_i] <- wpp_male_mid_year
  }
    comparison_list[[cn]] <- df_compare
}

comparison_list[["Kenya"]]
comparison_list[["Ghana"]]

# plotting for male
options(scipen=999)
for (cn in countries) {
  df <- comparison_list[[cn]]
  
  p <- ggplot(df, aes(x = Year)) +
    geom_line(aes(y = ModelMale, color = "Model"), linewidth = 1.2) +
    geom_line(aes(y = WPPMale, color = "WPP"), linewidth = 1.2) +
    ggtitle(paste("Male Population Comparison -", cn)) +
    ylab("Population") +
    scale_color_manual(values = c("Model" = "blue", "WPP" = "red")) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  print(p)  # explicitly print the plot object
}

#  female pop verification
comparison_list_female <- list()
for (c_idx in seq_along(countries)) {
  cn <- countries[c_idx]
  model_array <- model_results[[c_idx]] 
  total_pop_female <- model_array[, 1, 2] + model_array[, 2, 2]
  wpp_f_country <- popF1[popF1$name == cn, !(colnames(popF1) %in% as.character(c(1949:2009)))]
  
  df_compare_f <- data.frame(
    Year = year_seq,
    ModelFemale = NA_real_,
    WPPFemale = NA_real_
  )
  
  for (y_i in seq_along(year_seq)) {
    year <- year_seq[y_i]
    mid_i <- (y_i - 1)*steps_per_year + (steps_per_year / 2)
    model_female_pop_mid_year <- total_pop_female[mid_i]
    wpp_female_mid_year <- sum(wpp_f_country[16:101, as.character(year)], na.rm = TRUE)*1000
    
    df_compare_f$ModelFemale[y_i] <- model_female_pop_mid_year
    df_compare_f$WPPFemale[y_i] <- wpp_female_mid_year
  }
  
  comparison_list_female[[cn]] <- df_compare_f
}

# plotting for female
options(scipen=999)
for (cn in countries) {
  df_f <- comparison_list_female[[cn]]
  
  p_f <- ggplot(df_f, aes(x = Year)) +
    geom_line(aes(y = ModelFemale, color = "Model"), linewidth = 1.2) +
    geom_line(aes(y = WPPFemale, color = "WPP"), linewidth = 1.2) +
    ggtitle(paste("Female Population Comparison -", cn)) +
    ylab("Population") +
    scale_color_manual(values = c("Model" = "blue", "WPP" = "red")) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
  print(p_f)
}

#------------plotting overall sex stratified trend -------------
extracted_fit <- extract(fit)
ext_fit_m <- extracted_fit$svy_prd_m
ext_fit_f <- extracted_fit$svy_prd_f
n_sample <- dim(ext_fit_m)[1] 
n_cnt <- dim(ext_fit_m)[2]
niter <- dim(ext_fit_m)[3]

# matrix for storing results
male_prp <- matrix(NA, nrow = n_sample, ncol = niter)   
female_prp <- matrix(NA, nrow = n_sample, ncol = niter)

# total sum of m and f pop (deno)
total_male_pop <- sum(data_stan$pop[1, ])    
total_female_pop <- sum(data_stan$pop[2, ])  

for (i in 1:n_sample) {
  ext_fit_mi <- ext_fit_m[i, , ] * data_stan$pop[1, ]  
  ext_fit_fi <- ext_fit_f[i, , ] * data_stan$pop[2, ]
  male_numbers <- colSums(ext_fit_mi)   
  female_numbers <- colSums(ext_fit_fi)    
  male_prp[i, ] <- male_numbers / total_male_pop
  female_prp[i, ] <- female_numbers / total_female_pop
}

# apply quantile function over the columns (denoted by margin=2)
male_lci <- apply(male_prp, 2, quantile, probs = 0.025)
male_med <- apply(male_prp, 2, quantile, probs = 0.5)
male_uci <- apply(male_prp, 2, quantile, probs = 0.975)

female_lci <- apply(female_prp, 2, quantile, probs = 0.025)
female_med <- apply(female_prp, 2, quantile, probs = 0.5)
female_uci <- apply(female_prp, 2, quantile, probs = 0.975)

# plot for overall trend by sex (open population)
#  plotting x axis as %
male_med_perc <- male_med * 100
male_lci_perc <- male_lci * 100
male_uci_perc <- male_uci * 100

female_med_perc <- female_med * 100
female_lci_perc <- female_lci * 100
female_uci_perc <- female_uci * 100

y_lim <- c(0, max(male_uci_perc, female_uci_perc))

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
plot(time, male_med_perc, type = "n",
     xlab = "Year",
     ylab = "Proportion of people having ever used HIVST (%)",
     ylim = y_lim,
     las = 1,
     bty = "l"
)

polygon(x = c(time, rev(time)),
        y = c(male_lci_perc, rev(male_uci_perc)),
        col = adjustcolor("lightblue", alpha.f = 0.3), border = NA)
lines(time, male_med_perc, col = "deepskyblue4", lwd = 2)

polygon(x = c(time, rev(time)),
        y = c(female_lci_perc, rev(female_uci_perc)),
        col = adjustcolor("pink", alpha.f = 0.3), border = NA)
lines(time, female_med_perc, col = "deeppink1", lwd = 2)

title("Estimated trends in HIVST uptake in SSA by sex")

legend("top",
       legend = c("Men", "Women"),
       col = c("deepskyblue4", "deeppink1"),
       lwd = 2,
       lty = 1,
       pt.cex = 1.5,
       pt.bg = c("lightblue", "pink"),
       bty = "l")

# ------gg plot object for 4 panel figure--------
df_sextrend <- data.frame(
  time = time,
  male_med  = male_med_perc,
  male_lci  = male_lci_perc,
  male_uci  = male_uci_perc,
  female_med = female_med_perc,
  female_lci = female_lci_perc,
  female_uci = female_uci_perc
)

sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(
    aes(ymin = male_lci, ymax = male_uci),
    fill = "lightblue", alpha = 0.3, show.legend = FALSE
  ) +
  geom_line(
    aes(y = male_med, color = "Male"),
    size = 1.1
  ) +
  geom_ribbon(
    aes(ymin = female_lci, ymax = female_uci),
    fill = "pink", alpha = 0.3, show.legend = FALSE
  ) +
  geom_line(
    aes(y = female_med, color = "Female"),
    size = 1.1
  ) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion having ever used HIVST (%)"
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),  # no grid lines
    axis.line.x      = element_line(color = "black"),
    axis.line.y      = element_line(color = "black"),
    plot.margin      = margin(t = 1, r = 1, b = 3, l = 1, unit = "cm"),
    plot.title       = element_text(hjust = 0.5, vjust = -12, size = 14)
  ) + 
  coord_cartesian(clip = "off") +
theme(
  legend.position       = c(1, 0.5),   
  legend.justification  = c(1, 0.5),   
  plot.margin           = margin(t = 1, r = 1, b = 3, l = 1, unit = "cm")
)


# 2by2 plot (4 panel figure)
p1 <- sex_trend_plot + 
  labs(title = "(a) Trends in proportion of people who have ever used HIVST in Africa by sex") +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5, vjust = 1),
    legend.position = "right",  
    legend.justification = "center",
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  )

p2 <- rr_male_forest +
  labs(title = "(b) Overall and country-specific rate ratio estimates between males and females") +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    legend.position = "right",
    legend.justification = "center",
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm")
  )

p3 <- rr_retesting_forest +
  labs(title = "(c) Overall and country-specific rate ratio estimates between first time testing and re-testing using HIVST") +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5)
  )

p4 <- phi_forest +
  labs(title = "(d) Overall and country-specific estimates for proportion of distributed HIVST kits being used") +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5))

combined_plot <- (p1 | p2) /
  (p3 | p4) +
  plot_layout(nrow = 2, heights = c(1, 1))





# --getting the estimates for Lesotho----

# function to plot individual output
svy_m_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts_all <- as.data.frame(rstan::summary(fit, pars = c("hivst_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe", 
                   "sierraleone", "zambia", "mali", "uganda",
                   "lesotho", "mozambique", "rwanda",
                   "burkinafaso", "burundi", "cameroon", "cotedivoire",
                   "guinea", "liberia", "senegal", "southafrica","tanzania",
                   "namibia", "botswana", "guineabissau","drc", "eswatini")

plot_country_fit <- function(c_idx, cnt_lowercase, time, 
                             svy_m_all, svy_f_all, hts_all,
                             cnt_data,
                             niter, using_layout = FALSE) {
  cn <- cnt_lowercase[c_idx] # to match cnt_data keys exactly
  
  start_row <- (c_idx - 1)  * niter + 1
  end_row <- c_idx * niter
  
  svy_m_country <- svy_m_all[start_row:end_row, ]
  svy_f_country <- svy_f_all[start_row:end_row, ]
  hts_country <- hts_all[start_row:end_row, ]
  
  svy_dat_c <- cnt_data[[cn]]$svy_dat
  lci_svy_c <- cnt_data[[cn]]$lci_svy
  uci_svy_c <- cnt_data[[cn]]$uci_svy
  ind_svy_c <- cnt_data[[cn]]$ind_svy
  
  hts_dat_c <- cnt_data[[cn]]$hts_dat
  ind_hts_c <- cnt_data[[cn]]$ind_hts
  
  if (using_layout == FALSE) { par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1)) }
  plot(svy_m_country$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, 
       ylab = "Ever used HIVST", ylim = c(0, max(svy_m_country$`97.5%`, svy_f_country$`97.5%`, max(uci_svy_c))),
       main = paste0(toupper(substring(cn, 1, 1)), substring(cn, 2)))
  
  polygon(x = c(time, rev(time)),
          y = c(svy_m_country$`2.5%`, rev(svy_m_country$`97.5%`)),
          col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
  
  polygon(x = c(time, rev(time)),
          y = c(svy_f_country$`2.5%`, rev(svy_f_country$`97.5%`)),
          col = yarrr::transparent("pink3", trans.val = 0.5), border = NA)
  
  lines(svy_f_country$`50%` ~ time, col = "pink3", lwd = 3)
  
  points(svy_dat_c[, 1] ~ time[ind_svy_c], pch = 16, col = "blue4")
  points(svy_dat_c[, 2] ~ time[ind_svy_c], pch = 16, col = "firebrick4")
  
  segments(x0 = time[ind_svy_c], y0 = lci_svy_c[, 1],
           x1 = time[ind_svy_c], y1 = uci_svy_c[, 1], col = "blue4")
  segments(x0 = time[ind_svy_c], y0 = lci_svy_c[, 2],
           x1 = time[ind_svy_c], y1 = uci_svy_c[, 2], col = "firebrick4")
  
  legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")
  
  plot(hts_country$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, 
       ylab = "Number of HIVST kits",
       ylim = c(0, max(hts_country$`97.5%`)))
  
  polygon(x = c(time, rev(time)),
          y = c(hts_country$`2.5%`, rev(hts_country$`97.5%`)),
          col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
  
  points(hts_dat_c ~ time[ind_hts_c], pch = 16, col = "goldenrod3", cex = 1.25)
  
  mtext(paste0(toupper(substring(cn, 1, 1)), substring(cn, 2)), 
        outer = TRUE, side = 3, line = 1, cex = 1.5)
}

png("D:/Downloads/MSc Thesis/hivst/figs/all_plot.png", width = 8, height = 26, units = "in", res = 320)
nf <- layout(mat = matrix(1:52, nrow = 13, ncol = 4, byrow = TRUE),
             widths = rep(lcm(5), 4), heights = rep(lcm(5), 13), respect = TRUE)
for (c_idx in seq_along(cnt_lowercase)) {
  plot_country_fit(c_idx, cnt_lowercase, time, svy_m_all, svy_f_all, hts_all, cnt_data, niter, using_layout = TRUE)
}
dev.off()






# Specify Lesotho index
lesotho_index <- which(cnt_lowercase == "lesotho")

# Update the plotting function call for Lesotho
png("D:/Downloads/MSc Thesis/hivst/figs/leso_plot.png", width = 8, height = 4, units = "in", res = 320)

# Call the plotting function for Lesotho only
plot_country_fit(
  c_idx = lesotho_index,
  cnt_lowercase = cnt_lowercase,
  time = time,
  svy_m_all = svy_m_all,
  svy_f_all = svy_f_all,
  hts_all = hts_all,
  cnt_data = cnt_data,
  niter = niter,
  using_layout = FALSE # No layout needed for a single plot
)

dev.off()


# Extract the start and end rows for Lesotho
start_row <- (lesotho_index - 1) * niter + 1
end_row <- lesotho_index * niter

# Extract the values for svy_m_all and svy_f_all for Lesotho
svy_m_lesotho <- svy_m_all[start_row:end_row, ]
svy_f_lesotho <- svy_f_all[start_row:end_row, ]

data_stan$pop

# Extract male and female population for Lesotho from data_stan$pop
male_pop <- data_stan$pop[1, "Lesotho"]
female_pop <- data_stan$pop[2, "Lesotho"]

# Calculate total population
total_pop <- male_pop + female_pop

# Calculate overall proportions for all time steps
overall_proportion <- (svy_m_lesotho$`50%` * male_pop + svy_f_lesotho$`50%` * female_pop) / total_pop
# Calculate confidence intervals for the overall proportions
overall_lower <- (svy_m_lesotho$`2.5%` * male_pop + svy_f_lesotho$`2.5%` * female_pop) / total_pop
overall_upper <- (svy_m_lesotho$`97.5%` * male_pop + svy_f_lesotho$`97.5%` * female_pop) / total_pop







