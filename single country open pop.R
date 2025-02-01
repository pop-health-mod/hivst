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


#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)


# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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


# male mortality rate
mx_male_kenya <- mxM1[mxM1$name == "Kenya", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male_kenya[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male_kenya[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)


# female mortality rate
mx_female_kenya <- mxF1[mxF1$name == "Kenya", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female_kenya[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female_kenya[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]


# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}

yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) #for last year because it is 1 now


#-------stan block for open pop---------
hivst_mod <- '
functions {
  real[,,] hivst_fun(
    int niter,
    vector beta_t_dt,
    real beta_retest,
    real beta_male,
    vector pop,
    real dt,
    vector entry_m_dt,
    vector entry_f_dt,
    vector mort_m_dt,
    vector mort_f_dt
  ) {
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    
    real out[niter,4,2];
    out = rep_array(0.0, niter,4,2);
    
    // Initialization
    out[1,1,1] = pop[1];  // Males, never tested
    out[1,1,2] = pop[2];  // Females, never tested
    
    for (i in 2:niter) {
  // Males

  out[i,1,1] = out[i-1,1,1] + dt * (entry_m_dt[i]*(out[i-1,1,1] + out[i-1,2,1]) - rr_t[i] * rr_m*out[i-1,1,1] - mort_m_dt[i]*out[i-1,1,1]);
  out[i,2,1] = out[i-1,2,1] + dt * (rr_t[i] * rr_m*out[i-1,1,1] - mort_m_dt[i]*out[i-1,2,1]);
  out[i,3,1] = dt * rr_t[i] * rr_m*(out[i-1,1,1] + rr_r*out[i-1,2,1]);
  out[i,4,1] = out[i,2,1] / (out[i,1,1] + out[i,2,1]);
  
  // Females
  out[i,1,2] = out[i-1,1,2] + dt * (epentry_f_dt[i]s_f*(out[i-1,1,2] + out[i-1,2,2]) - rr_t[i]*out[i-1,1,2] - mort_f_dt[i]*out[i-1,1,2]);
  out[i,2,2] = out[i-1,2,2] + dt * (rr_t[i]*out[i-1,1,2] - mort_f_dt[i]*out[i-1,2,2]);
  out[i,3,2] = dt * rr_t[i]*(out[i-1,1,2] + rr_r*out[i-1,2,2]);
  out[i,4,2] = out[i,2,2] / (out[i,1,2] + out[i,2,2]);
    }
return out;
  }
}

data {
  int<lower = 1> n_yr;
  int<lower = 1> niter;
  int<lower = 1> yr_ind[niter];
  real dt;
  vector[2] pop;
  int<lower = 1> beta_ind[n_yr];
  int<lower = 0> n_hts;
  int<lower = 1> n_svy;
  int<lower = 0> ind_hts[n_hts];
  int<lower = 0> ind_svy[n_svy];
  int hivst[n_hts];
  real se_hts[n_hts];
  int<lower = 0> num_svy[n_svy, 2];
  int<lower = 1> den_svy[n_svy, 2];
  
  // entry and exit rates for open pop
  real entry_m[n_yr];   
  real entry_f[n_yr];   
  real mort_m[n_yr];    
  real mort_f[n_yr];    
}

parameters {
  real beta_t[n_yr];                 
  real<lower = 0, upper = 5> sd_rw;  
  real beta_retest;                  
  real beta_male;                    
  real<lower = 0, upper = 1> phi;    
}

 // mapping beta_t, entry rate and exit rate to yearly rate
transformed parameters {
  vector[niter] beta_t_dt;
  vector[niter] entry_m_dt;
  vector[niter] entry_f_dt;
  vector[niter] mort_m_dt;
  vector[niter] mort_f_dt;
  
  for (i in 1:niter) {
    beta_t_dt[i] = beta_t[yr_ind[i]];
    entry_m_dt[i] = entry_m[yr_ind[i]];
    entry_f_dt[i] = entry_f[yr_ind[i]];
    mort_m_dt[i] = mort_m[yr_ind[i]];
    mort_f_dt[i] = mort_f[yr_ind[i]];
  }
}

model {
  real model_pred[niter,4,2] = hivst_fun(
    niter, beta_t_dt, beta_retest, beta_male, pop, dt,
    entry_m_dt, entry_f_dt, mort_m_dt, mort_f_dt
  ); // added open pop
  
  // Priors
  beta_t[1] ~ normal(-10,1);
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr-1)], sd_rw);
  sd_rw ~ normal(0,0.5);
  beta_retest ~ normal(log(1.2),0.5);
  beta_male ~ normal(log(1),0.5);
  phi ~ beta(24,6);
  
  // Likelihood: survey
  num_svy[,1] ~ binomial(den_svy[,1], model_pred[ind_svy,4,1]);
  num_svy[,2] ~ binomial(den_svy[,2], model_pred[ind_svy,4,2]);
  
  // Likelihood: program data (HTS)
  vector[niter] hts_m = (to_vector(model_pred[,3,1]) + to_vector(model_pred[,3,2])) / phi;
  {
    real hts_m_pery[n_hts];
    for (i in 1:n_hts) {
      hts_m_pery[i] = sum(hts_m[ind_hts[i]:to_int(ind_hts[i] + (1/dt)-1)]);
    }
    hivst ~ normal(hts_m_pery, se_hts);
  }
}

generated quantities {
  vector[niter] hivst_prd;
  real svy_prd_m[niter];
  real svy_prd_f[niter];
  real pred[niter,4,2] = hivst_fun(
    niter, beta_t_dt, beta_retest, beta_male, pop, dt,
    entry_m_dt, entry_f_dt, mort_m_dt, mort_f_dt
  ); // added open pop
  
  svy_prd_m = pred[,4,1];
  svy_prd_f = pred[,4,2];
  
  hivst_prd = (to_vector(pred[,3,1]) + to_vector(pred[,3,2])) / phi;
  real hts_pred_pery[n_yr];
  for (i in 1:n_yr) {
    hts_pred_pery[i] = sum(hivst_prd[beta_ind[i]:to_int(beta_ind[i] + (1/dt)-1)]);
  }
}
'

# we compile the model and expose the function to invoke it directly in R
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

#survey and program data
ind_svy <- (c(2012.5, 2018.5, 2022.5) - start) / dt
den_svy <- round(cbind(c(5756, 20102, 14453), c(7938, 22350, 32156)) * 0.8)
num_svy <- round(cbind(c(185, 425, 1305), c(145, 545, 1552)) * 0.8)
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c( 2018,  2019,   2020,    2021,   2022,  2023) - start) / dt
yr_hts <- c( 2018,  2019,   2020,    2021,   2022,  2023)
hts_dat <- c(197200, 400000, 595953, 630000, 342610, 617317)
se_hts <- hts_dat * 0.1

# data for fitting and running
data_stan <- list(n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                  pop = pop,
                  beta_ind = beta_ind,
                  n_svy = nrow(num_svy),
                  n_hts = length(hts_dat),
                  ind_svy = ind_svy,
                  ind_hts = ind_hts,
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
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Kenya with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)




#---"Zimbabwe"----
wpp_m <- popM1[popM1$name == "Zimbabwe", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Zimbabwe", !(colnames(popF1) %in% as.character(c(1949:2009)))]


#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)


# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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


# male mortality rate
mx_male_zmb <- mxM1[mxM1$name == "Zimbabwe", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male_zmb[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male_zmb[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)


# female mortality rate
mx_female_zmb <- mxF1[mxM1$name == "Zimbabwe", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female_zmb[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female_zmb[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]


# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) 



#survey and program data
ind_svy <- (c(2015.5, 2019.5, 2020.5) - start) / dt
den_svy <- round(cbind(c(8396, 4179, 8220), c(9955, 10130, 12573)) * 0.8)
num_svy <- round(cbind(c(147, 214, 476), c(26, 559, 742)) * 0.8)
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
yr_hts <- c(2019,   2020,    2021,   2022,  2023)
ind_hts <- (c(2019, 2020, 2021, 2022, 2023) - start) / dt
hts_dat <- c(174566, 240434, 459517, 414499, 513090)
se_hts <- hts_dat * 0.1


#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Zimbabwe with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)



#-----------------checking pop fit from wpp and hivstfun()---------
post <- rstan::extract(fit)
beta_t_median <- apply(post$beta_t, 2, median)
beta_retest_median <- median(post$beta_retest)
beta_male_median <- median(post$beta_male)
phi_median <- median(post$phi)
beta_t_dt <- beta_t_median[yr_ind]
entry_m_dt <- entry_m_vec[yr_ind]
entry_f_dt <- entry_f_vec[yr_ind]
mort_m_dt <- mort_m_vec[yr_ind]
mort_f_dt <- mort_f_vec[yr_ind]

model_out_raw <- hivst_fun(
  niter = niter,
  beta_t_dt = beta_t_dt,
  beta_retest = beta_retest_median, 
  beta_male = beta_male_median,     
  pop = pop,
  dt = dt,
  entry_m_dt = entry_m_dt,
  entry_f_dt = entry_f_dt,
  mort_m_dt = mort_m_dt,
  mort_f_dt = mort_f_dt
)

str(model_out_raw)
model_array <- array(NA, dim = c(niter, 4, 2))
for (i in 1:niter) {
  for (j in 1:4) {
    model_array[i, j, ] <- model_out_raw[[i]][[j]]
  }
}

total_pop_male <- model_array[,1,1] + model_array[,2,1]
total_pop_female <- model_array[,1,2] + model_array[,2,2]

steps_per_year <- 1/dt
year_seq <- start:(end-1)

for (year in year_seq) {
  y_index <- year - start + 1
  i <- (y_index - 1)*steps_per_year + (steps_per_year / 2)
  model_male_pop_mid_year <- total_pop_male[i]
  wpp_male_mid_year <- sum(wpp_m[16:101, as.character(year)], na.rm = TRUE)*1000
  cat("Year:", year,
      "Model male pop:", model_male_pop_mid_year, 
      "WPP male pop:", wpp_male_mid_year, "\n\n")
}


# Ghana 

#---"Ghana"----
wpp_m <- popM1[popM1$name == "Ghana", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Ghana", !(colnames(popF1) %in% as.character(c(1949:2009)))]


#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)


# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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


# male mortality rate
mx_male_gha<- mxM1[mxM1$name == "Ghana", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male_gha[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male_gha[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)


# female mortality rate
mx_female_gha<- mxF1[mxM1$name == "Ghana", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female_gha[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female_gha[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]

# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) 




# survey data and hivst program data
yr_svy <- c(2017.5, 2022.5)
ind_svy <- (c(2017.5, 2022.5) - start) / dt
den_svy <- round(cbind(c(2553, 4558), c(5575, 6250)))
num_svy <- round(cbind(c(37, 83), c(132, 151)))
yr_hts <- c(2020,  2021,   2022,  2023)
ind_hts <- (c(2020, 2021, 2022, 2023) - start) / dt
hts_dat <- c(20000, 1323, 235000, 140500)
se_hts <- c(20000, 1323, 235000, 140500) * 0.1
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)

#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Ghana with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)



#---Sierra Leone------------------
wpp_m <- popM1[popM1$name == "Sierra Leone", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Sierra Leone", !(colnames(popF1) %in% as.character(c(1949:2009)))]


#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)

# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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

# male mortality rate
mx_male_sle<- mxM1[mxM1$name == "Sierra Leone", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male_sle[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male_sle[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)

# female mortality rate
mx_female_sle<- mxF1[mxM1$name == "Sierra Leone", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female_sle[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female_sle[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]

# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) 


# survey data and hivst program data
ind_svy <- (c(2017.5, 2019.5) - start) / dt
den_svy <- round(cbind(c(2465, 2907), c(5096, 2607)))
num_svy <- round(cbind(c(50, 62), c(165, 101)))
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
yr_hts <- c( 2020,    2021,   2022)
ind_hts <- (c(2020, 2021, 2022) - start) / dt
hts_dat <- c(2500, 270, 11050)
se_hts <- hts_dat * 0.1


#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Sierra Leone with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)



# ----------Madagascar---------------------
wpp_m <- popM1[popM1$name == "Madagascar", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Madagascar", !(colnames(popF1) %in% as.character(c(1949:2009)))]


#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)


# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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


# male mortality rate
mx_male<- mxM1[mxM1$name == "Madagascar", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)


# female mortality rate
mx_female<- mxF1[mxM1$name == "Madagascar", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]

# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) 

# survey data and hivst program data
ind_svy <- (c(2018.5, 2021.5) - start) / dt
den_svy <- round(cbind(c(3055, 6178), c(5039, 6825)))
num_svy <- round(cbind(c(35, 44), c(84, 20)))
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
yr_hts <- c(2022, 2023)
ind_hts <- (c(2022, 2023) - start) / dt
hts_dat <- c(2500, 2500)
se_hts <- hts_dat * 0.1


#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Madagascar with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)


#---Malawi-------
wpp_m <- popM1[popM1$name == "Malawi", !(colnames(popM1) %in% as.character(c(1949:2009)))]
wpp_f <- popF1[popF1$name == "Malawi", !(colnames(popF1) %in% as.character(c(1949:2009)))]

#linear interplation to get start of the year pop
pop_2010_5_m <- sum(wpp_m[16:101, "2010"]) * 1000
pop_2011_5_m <- sum(wpp_m[16:101, "2011"]) * 1000
pop_2010_5_f <- sum(wpp_f[16:101, "2010"]) * 1000
pop_2011_5_f <- sum(wpp_f[16:101, "2011"]) * 1000
pop_2011_0_m <- (pop_2010_5_m + pop_2011_5_m) / 2
pop_2011_0_f <- (pop_2010_5_f + pop_2011_5_f) / 2
pop <- c(pop_2011_0_m, pop_2011_0_f)


# time specification
start <- 2011
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt
n_yr <- end - start

# Entry rate for male (current year's 15y pop / total pop aged 15–100)
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


# male mortality rate
mx_male<- mxM1[mxM1$name == "Malawi", c("name", "age", as.character(2010:2024))]

wt_mort_rate_m <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_male <- wpp_m[(15 + 1):(100 + 1), "2023"] * 1000
    mort_male <- mx_male[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_male <- wpp_m[(15 + 1):(100 + 1), t] * 1000
    mort_male <- mx_male[(15 + 1):(100 + 1), t]
  }
  wt_deaths_m <- (pop_male) * (mort_male)
  tot_deaths_m <- sum(wt_deaths_m)
  tot_pop_m <- sum(pop_male)
  wt_mort_rate_m[as.numeric(t) - start + 1] <- tot_deaths_m / tot_pop_m
}
wt_mort_male <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_m)


# female mortality rate
mx_female<- mxF1[mxM1$name == "Malawi", c("name", "age", as.character(2010:2024))]

wt_mort_rate_f <- numeric(end - start + 1)
for (t in as.character(start:end)) {
  if (t == "2024") {
    pop_female <- wpp_f[(15 + 1):(100 + 1), "2023"] * 1000
    mort_female <- mx_female[(15 + 1):(100 + 1), "2024"]
  } else {
    pop_female <- wpp_f[(15 + 1):(100 + 1), t] * 1000
    mort_female <- mx_female[(15 + 1):(100 + 1), t]
  }
  wt_deaths_f <- (pop_female) * (mort_female)
  tot_deaths_f <- sum(wt_deaths_f)
  tot_pop_f <- sum(pop_female)
  wt_mort_rate_f[as.numeric(t) - start + 1] <- tot_deaths_f / tot_pop_f
}
wt_mort_female <- data.frame(Year = as.integer(start:end), MortalityRate = wt_mort_rate_f)

# preparing entry rates and mort rates for model
entry_m_vec <- entry_rate_male$EntryRate[2:14]
entry_f_vec <- entry_rate_female$EntryRate[2:14]

mort_m_vec <- wt_mort_male$MortalityRate[1:13] 
mort_f_vec <- wt_mort_female$MortalityRate[1:13]

# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind) 


# survey data and hivst program data (not using 2019 survey data for now)
#ind_svy <- (c(2015.5, 2019.5, 2020.5) - start) / dt
ind_svy <- (c(2015.5, 2020.5) - start) / dt

#den_svy <- round(cbind(c(2796, 2150, 5165), c(14792, 6669, 5920)))
#num_svy <- round(cbind(c(30, 214, 406), c(136, 443, 373)))

den_svy <- round(cbind(c(2796, 5165), c(14792, 5920)))
num_svy <- round(cbind(c(30, 406), c(136, 373)))
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
yr_hts <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
ind_hts <- (c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023) - start) / dt
hts_dat <- c(63460, 228021, 501889, 830402, 1780000, 1000000, 1488750, 2699100)
se_hts <- hts_dat * 0.1


#summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

#-----------------------------------------------
# we get the results
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hts_pred_pery"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# plot
options(scipen = 999)
par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "Ever used HIVST(%)", ylim = c(0, 0.2))
polygon(x = c(time, rev(time)),
        y = c(svy_m$`2.5%`, rev(svy_m$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
polygon(x = c(time, rev(time)),
        y = c(svy_f$`2.5%`, rev(svy_f$`97.5%`)),
        col = yarrr::transparent("pink3", trans.val = 0.5), border = NA) 
lines(svy_f$`50%` ~ time, col = "pink3", lwd = 3)

points(svy_dat[, 1] ~ time[ind_svy], pch = 16, col = "blue4")
points(svy_dat[, 2] ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 1],
         x1 = time[ind_svy], y1 = uci_svy[, 1], col = "blue4")
segments(x0 = time[ind_svy], y0 = lci_svy[, 2],
         x1 = time[ind_svy], y1 = uci_svy[, 2], col = "firebrick4")
legend("topleft", legend = c("men", "women"), col = c("steelblue4", "pink4"), lwd = 4, bty = "n")

# hts fit
year <- start:(end-1)
plot(hts$`50%` ~ year, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(year, rev(year)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ year[year %in% yr_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Malawi with open population", outer = TRUE, side = 3, line = 1, cex = 1.5)



