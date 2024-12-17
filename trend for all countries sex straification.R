
rm(list = ls())
gc()

library(wpp2024)
library(rstan)

#---preparing model inputs---

# male & female pop data from wpp
data(popM1)
data(popF1)

# function to retrieve pop data in 2020
get_popdata <- function(cnt_name, year="2020") {
  wpp_m <- popM1[popM1$name == cnt_name, !(colnames(popM1) %in% as.character(c(1949:2008)))]
  wpp_f <- popF1[popF1$name == cnt_name, !(colnames(popF1) %in% as.character(c(1949:2008)))]
  return(c(sum(wpp_m[(15 + 1):(100 + 1), year]) * 1000, 
           sum(wpp_f[(15 + 1):(100 + 1), year]) * 1000))
}

# pop for 6 countries
countries <- c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", "Sierra Leone") 
pop <- sapply(countries, get_popdata)#2by2 matrix of male & female pop in 6 countries

# time specification
start <- 2009 #same starting year for all countries
end <- 2024  
dt <- 0.1 
time <- seq(start, end - dt, by = dt) 
niter <- (end - start) / dt
n_yr <- end - start

# mapping HIVST rate to the appropriate yearly indices
beta_ind <- seq(1, niter, by = 1 / dt) 
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)

# stan code
hivst_mod <- '
functions {
  real[, , ] hivst_fun(int niter,
                       vector beta_t_dt,
                       real beta_retest,
                       real beta_male,
                       vector pop, // male&female 
                       real dt) {

    // converting rates from log scale
    vector[niter] rr_t = exp(beta_t_dt); 
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    
    // initializing the compartments
    real out[niter, 4, 2];
    out = rep_array(0.0, niter, 4, 2);
    out[1, 1, 1] = pop[1]; // male
    out[1, 1, 2] = pop[2]; // female

    for (i in 2:niter) {
   // males
        out[i, 1, 1] = out[i - 1, 1, 1] + dt * (-rr_t[i] * rr_m * out[i - 1, 1, 1]);
        out[i, 2, 1] = out[i - 1, 2, 1] + dt * (+rr_t[i] * rr_m * out[i - 1, 1, 1]);
        out[i, 3, 1] = rr_t[i] * rr_m * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
        out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);
        
    // females
        out[i, 1, 2] = out[i - 1, 1, 2] + dt * (-rr_t[i] * out[i - 1, 1, 2]);
        out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+rr_t[i] * out[i - 1, 1, 2]);
        out[i, 3, 2] = rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
        out[i, 4, 2] = out[i, 2, 2] / (out[i, 1, 2] + out[i, 2, 2]);
    }
    return out;
} 
}

data {
  int<lower = 1> n_cnt;             // nb countries
  int<lower = 1> n_yr;              // nb of years   
  int<lower = 1> niter;             // nb iterations (same for all countries)
  int<lower = 1> tot_svy;         // total nb of surveys accross all countries
  int<lower = 1> tot_hts;         // total nb of year with hivst data, accross all countries
  int<lower = 1> yr_ind[niter];     // mapping the yearly rate to the dt ones
  real dt;       
  matrix[2, n_cnt] pop;               // r = sex, c = cnt
  
  // for prgrm data
  int<lower = 1> n_hts_by_cnt[n_cnt];        // nb of years with observed HIVST data per cntry
  int<lower = 1> ind_hts[tot_hts]; // indices pgm data(max length?)
  int<lower = 1> hivst[tot_hts];   // nb of HIVST performed per year per cnty
  real se_hts[tot_hts];            // se for program data(max lenght required?)
  
  // for survey data
  int<lower = 1> n_svy_by_cnt[n_cnt];     // nb of surveys per country
  int<lower = 1> ind_svy[tot_svy]; // indices for survey data (max length across countries)
  int<lower = 1> num_svy[tot_svy, 2]; // numerator for survey proportions
  int<lower = 1> den_svy[tot_svy, 2]; // denominator for survey proportions

  // indices for the unlist data
  int<lower = 1> svy_idx_s[n_cnt];
  int<lower = 1> svy_idx_e[n_cnt]; 
  int<lower = 1> hts_idx_s[n_cnt];
  int<lower = 1> hts_idx_e[n_cnt];  
}

parameters {
  matrix[n_cnt, n_yr] beta_t;          // yearly HIVST rates (rw1) for each country
  real<lower = 1e-6, upper = 5> sd_rw;    // sd of the rw1 for beta_t
  real<lower = 1e-6, upper = 2.5> sd_phi;    // sd of the RE for phi
  real<lower = 1e-6, upper = 2.5> sd_rt;    // sd of the RE for the re-testing rate ratio
  real<lower = 1e-6, upper = 2.5> sd_men;    // sd of the RE of the male rate ratio
  real beta_retest_overall;            // overall shared re-testing rate
  vector[n_cnt] beta_rt_raw;            // country-specific re-testing rates
  real beta_men_overall;
  vector[n_cnt] beta_men_raw;             //overall male relative rate of HIVST
  real phi_overall; // overall proportion of HIVST kits used
  vector[n_cnt] phi_raw;      // country-specific proportions of HIVST kits used
}

transformed parameters {
  // new non-centered parameterization
  vector[n_cnt] beta_retest;
  vector[n_cnt] beta_male;
  vector[n_cnt] phi;
  
  beta_retest = beta_retest_overall + sd_rt * beta_rt_raw;
  beta_male = beta_men_overall + sd_men * beta_men_raw;
  phi = phi_overall + sd_phi * phi_raw;
  
  // mapping beta_t to beta_t_dt
  matrix[n_cnt, niter] beta_t_dt;  // yearly HIVST rates mapped to time steps
  for (c in 1:n_cnt) {
    for (i in 1:niter) {
      beta_t_dt[c, i] = beta_t[c, yr_ind[i]];
    }
  }
}

model {
    matrix[niter, n_cnt] hts_mod;

  // priors
  // overall prior for the SD of the RW1 for testing rate
  sd_rw ~ normal(0, 0.5) T[1e-6, 5];
  sd_phi ~ normal(0, 0.25) T[1e-6, 2.5];
  sd_rt ~ normal(0, 0.25) T[1e-6, 2.5];
  sd_men ~ normal(0, 0.25) T[1e-6, 2.5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(log(1.2), 0.5);
  // overall prior for the % of tests distributed being used
  phi_overall ~ normal(logit(0.85), 0.5);
  beta_men_overall ~ normal(log(1), 0.5);
  
  beta_rt_raw ~ std_normal();
  beta_men_raw ~ std_normal();
  phi_raw ~ std_normal();

  // country-specific priors
  for (c in 1:n_cnt) {
    beta_t[c, 1] ~ normal(-10, 1); // exp(-10 + c(-1, 1) * 1.96 * 1)
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);

  // model predictions and likelihoods 
    real model_pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], pop[, c], dt);

    // fitting to survey data (layer of country and surveys)
      num_svy[svy_idx_s[c]:svy_idx_e[c], 1] ~ binomial(den_svy[svy_idx_s[c]:svy_idx_e[c], 1], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 1]);
      num_svy[svy_idx_s[c]:svy_idx_e[c], 2] ~ binomial(den_svy[svy_idx_s[c]:svy_idx_e[c], 2], 
                                              model_pred[ind_svy[svy_idx_s[c]:svy_idx_e[c]], 4, 2]);

    // fitting to program data(layer of country & years we are modeling)
    hts_mod[, c] = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / inv_logit(phi[c]);
    hivst[hts_idx_s[c]:hts_idx_e[c]] ~ normal(hts_mod[ind_hts[hts_idx_s[c]:hts_idx_e[c]], c], 
                                              se_hts[hts_idx_s[c]:hts_idx_e[c]]);
  }
}

generated quantities {
    matrix[n_cnt, niter] hivst_prd;  // predicted HIVST rates per time step for all countries
    matrix[n_cnt, niter] svy_prd_m;  // predicted survey proportions (males) per country
    matrix[n_cnt, niter] svy_prd_f;  // predicted survey proportions (females) per country

    for (c in 1:n_cnt) {
    real pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male[c], pop[, c], dt);
        
        // survey prediction
            svy_prd_m[c, ] = to_row_vector(pred[, 4, 1]);  // males ever used HIVST
            svy_prd_f[c, ] = to_row_vector(pred[, 4, 2]);  // females ever used HIVST
        
        // hts predictions (slicing operator : takes all niter for each country)
        hivst_prd[c, ] = to_row_vector(to_vector(pred[, 3, 1]) + to_vector(pred[, 3, 2])) / inv_logit(phi[c]);
        }
}
'

# Compiling the model and exposing functions
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

#------ Survey & program data for each country ------------
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
    yr_svy = c(2015.5, 2020.5),
    ind_svy = (c(2015.5, 2020.5) - start) / dt,
    den_svy = round(cbind(c(2796, 5165), c(14792, 5920))),
    num_svy = round(cbind(c(30, 406), c(136, 373))),
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
    ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(174566, 240434, 459517, 414499, 513090),
    se_hts = c(174566, 240434, 459517, 414499, 513090) * 0.1
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

# list of survey years for each country
n_cnt <- length(cnt_data) # 6 cntries
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


#combining svy and pgm data for all countries
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
  den_svy = den_svy                    
)
rstan_options(auto_write = TRUE)

# ---- fitting the model ----
options(mc.cores = parallel::detectCores())
init_function <- function() {
  list(
    beta_t = matrix(rnorm(data_stan$n_cnt * data_stan$n_yr, 0, 0.1), ncol = data_stan$n_yr, nrow = data_stan$n_cnt),   # Random initial value for alpha
    sd_rw = runif(1, min = 1.25, max = 2),
    sd_phi = runif(1, min = 0.1, max = 1),
    sd_rt = runif(1, min = 0.1, max = 1),
    sd_men = runif(1, min = 0.1, max = 1),
    beta_restest_overall = rnorm(1, log(1), 0.5),
    beta_rt_raw = rnorm(data_stan$n_cnt, 0, 0.5),
    beta_men_overall = rnorm(1, log(1.2), 0.2),
    beta_men_raw = rnorm(data_stan$n_cnt, 0, 0.2),
    beta_phi_overall = rnorm(1, qlogis(0.8), 0.2),
    phi_raw = rnorm(data_stan$n_cnt, 0, 0.2)
  )
}
fit <- sampling(hivst_stan, data = data_stan, iter = 2000, chains = 4, init = init_function,
                warmup = 1000, thin = 1, control = list(adapt_delta = 0.9))

shinystan::launch_shinystan(fit)


# traceplots
traceplot(fit, pars = "beta_t")
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
r <- as.data.frame(rstan::summary(fit, pars = c("beta_t"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(r$`50%`)

rr_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_retest_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_overall$`50%`)
rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr$`50%`)
exp(rr$`2.5%`)
exp(rr$`97.5%`)

rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m$`50%`)
exp(rr_m$`2.5%`)
exp(rr_m$`97.5%`)

phi_overall <- as.data.frame(rstan::summary(fit, pars = c("phi_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi_overall$`50%`
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`


# function to plot individual output
svy_m_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f_all <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts_all <- as.data.frame(rstan::summary(fit, pars = c("hivst_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe", "sierraleone")
plot_country_fit <- function(c_idx, cnt_lowercase, time, 
                             svy_m_all, svy_f_all, hts_all,
                             cnt_data,
                             niter) {
  cn <- cnt_lowercase[c_idx] # cn now matches cnt_data keys exactly
  
  start_row <- (c_idx - 1)*niter + 1
  end_row <- c_idx*niter
  
  svy_m_country <- svy_m_all[start_row:end_row, ]
  svy_f_country <- svy_f_all[start_row:end_row, ]
  hts_country <- hts_all[start_row:end_row, ]
  
  svy_dat_c <- cnt_data[[cn]]$svy_dat
  lci_svy_c <- cnt_data[[cn]]$lci_svy
  uci_svy_c <- cnt_data[[cn]]$uci_svy
  ind_svy_c <- cnt_data[[cn]]$ind_svy
  
  hts_dat_c <- cnt_data[[cn]]$hts_dat
  ind_hts_c <- cnt_data[[cn]]$ind_hts
  
  par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
  plot(svy_m_country$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, 
       ylab = "Ever used HIVST", ylim = c(0, max(svy_m_country$`97.5%`, svy_f_country$`97.5%`)))
  
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
  
  mtext(cn, outer = TRUE, side = 3, line = 1, cex = 1.5)
}

for (c_idx in seq_along(cnt_lowercase)) {
  plot_country_fit(c_idx, cnt_lowercase, time, svy_m_all, svy_f_all, hts_all, cnt_data, niter)
}


#------ combined fit ------------
extracted_fit <- extract(fit)
ext_fit_m <- extracted_fit$svy_prd_m
ext_fit_f <- extracted_fit$svy_prd_f
n_sample <- dim(ext_fit_m)[1] # no of iterations (6000)

#ext_fit_m[i, , ]:fixing 1st dimension at i (selecting the i-th iteration).
#Using , (with nothing before/after) for 2nd & 3rd dim: take all elements along those dim
#6000 rows(one for each posterior sample or iteration),150 columns (one for each time step)

pool_prp <- matrix(NA, nrow = n_sample, ncol = niter) # every element starts w/ NA, otherwise error as data is missing
for (i in 1:n_sample) {
  ext_fit_mi <- ext_fit_m[i, , ] * data_stan$pop[1, ]
  ext_fit_fi <- ext_fit_f[i, , ] * data_stan$pop[2, ]
  pool_prp[i, ] <- (colSums(ext_fit_mi) + colSums(ext_fit_fi)) / sum(data_stan$pop)
}

#apply quantile function over the columns (denoted by margin=2)
pool_lci <- apply(pool_prp, MARGIN = 2, quantile, probs = 0.025)
pool_med <- apply(pool_prp, MARGIN = 2, quantile, probs = 0.5)
pool_uci <- apply(pool_prp, MARGIN = 2, quantile, probs = 0.975)


#plotting from matrix
plot(pool_med, type = "l", lwd = 2)
lines(pool_lci, col = "grey")
lines(pool_uci, col = "grey")

#plot for combined fit
par(mfrow = c(1,1), oma = c(2, 2, 2, 2), mar = c(4, 4, 2, 2))
median_col <- "purple4"                          
ci_shade_col <- adjustcolor("plum", alpha.f = 0.3)
custom_years <- c(2009, 2012, 2015, 2018, 2021, 2024)
plot(time, pool_med, type = "n",
     xlab = "Year",
     ylab = "Proportion of People Having Used HIVST",
     ylim = c(0, max(pool_uci)),
     las = 1,   
     bty = "l",
     xaxt = "n"
)
polygon(x = c(time, rev(time)),
        y = c(pool_lci, rev(pool_uci)),
        col = ci_shade_col, border = NA)

lines(time, pool_med, col = median_col, lwd = 2)
axis(1, at = custom_years, labels = custom_years)

legend("topright",
       legend = c("Combined trend fit for 6 countries with more than one survey", "95% CrI"),
       lty = c(1, NA),
       pch = c(NA, 15),
       pt.cex = c(1, 1.5),
       col = c(median_col, ci_shade_col),
       bty = "n")

title("Estimated Trend of HIV Self-Test Uptake")

