library(wpp2024)
library(rstan)
library(V8)

#first testing with 2 countries:moving the automated starting y+survey+prgm data functions for the time being
#keeping manual data inputs for simplicity

#---preparing model inputs---

#male & female pop data from wpp
data(popM1)
data(popF1)

# function to retrieve pop data in 2020
get_popdata <- function(cnt_name, year="2020") {
  wpp_m <- popM1[popM1$name == cnt_name, !(colnames(popM1) %in% as.character(c(1949:2010)))]
  wpp_f <- popF1[popF1$name == cnt_name, !(colnames(popF1) %in% as.character(c(1949:2010)))]
  return(c(sum(wpp_m[(15 + 1):(100 + 1), year]) * 1000, 
           sum(wpp_f[(15 + 1):(100 + 1), year]) * 1000))
}

# pop for 2 countries
countries <- c("Kenya", "Ghana") 
pop <- sapply(countries, get_popdata)#2by2 matrix of male & female pop in Ken&Gha

# time specification
start <- 2009 #same starting year for all countries
end <- 2024  #same for all
dt <- 0.1 #unchanged
time <- seq(start, end - dt, by = dt) 
niter <- (end - start) / dt
n_yr <- end - start

# mapping HIVST rate to the appropriate yearly indices
beta_ind <- seq(1, niter, by = 1 / dt) #15 indices of testing rate indicating beginning of each year
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
                       real dt,
                       int dt_yr) {

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
        out[i, 3, 1] = dt * rr_t[i] * rr_m * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
        out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);
        
    // females
        out[i, 1, 2] = out[i - 1, 1, 2] + dt * (-rr_t[i] * out[i - 1, 1, 2]);
        out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+rr_t[i] * out[i - 1, 1, 2]);
        out[i, 3, 2] = dt * rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
        out[i, 4, 2] = out[i, 2, 2] / (out[i, 1, 2] + out[i, 2, 2]);
    }
    
    // we compute rolling sums to get the annual number of tests
    int n_sum = niter - dt_yr;
    for (i in 1:n_sum) {
      out[i, 3, 1] = sum(out[i:(i + dt_yr - 1), 3, 1]); 
      out[i, 3, 2] = sum(out[i:(i + dt_yr - 1), 3, 2]);       
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
  int dt_yr; 
  matrix[2, n_cnt] pop;               // r = sex, c = cnt
  
  // for prgrm data;
  int<lower = 1> n_hts_by_cnt[n_cnt];        // nb of years with observed HIVST data per cntry
  int<lower = 1> ind_hts[tot_hts]; // indices pgm data(max length?)
  int<lower = 1> hivst[tot_hts];   // nb of HIVST performed per year per cnty
  real se_hts[tot_hts];            // se for program data(max lenght required?)
  
  // for survey data;
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
  real<lower = 0, upper = 5> sd_rw;    // sd of the rw1 for beta_t
  real<lower = 0, upper = 5> sd_phi;    // sd of the rw1 for beta_t
  real<lower = 0, upper = 5> sd_rt;    // sd of the rw1 for beta_t
  real beta_retest_overall;            // overall shared re-testing rate
  real beta_retest[n_cnt];            // country-specific re-testing rates
  real beta_male;                      //male relative rate of HIVST (same for all cntries)
  real phi_overall; // overall proportion of HIVST kits used
  real phi[n_cnt];      // country-specific proportions of HIVST kits used
}

transformed parameters {
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
  sd_rw ~ normal(0, 1) T[0, 5];
  sd_phi ~ normal(0, 0.5) T[0, 5];
  sd_rt ~ normal(0, 0.5) T[0, 5];
  // overall prior for retesting parameter
  beta_retest_overall ~ normal(log(1.2), 0.5);
  // overall prior for the % of tests distributed being used
  phi_overall ~ normal(logit(0.85), 0.5);
  // prior for male-to-female ratio - fixed accross countries
  beta_male ~ normal(log(1), 0.5);

  // country-specific priors
  for (c in 1:n_cnt) {
    beta_retest[c] ~ normal(beta_retest_overall, sd_rt);
    phi[c] ~ normal(phi_overall, sd_phi);
    beta_t[c, 1] ~ normal(-5, 2);
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);

  // model predictions and likelihoods 
    real model_pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male, pop[, c], dt, dt_yr);

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
    real pred[niter, 4, 2] = hivst_fun(niter, to_vector(beta_t_dt[c, ]), beta_retest[c], beta_male, pop[, c], dt, dt_yr);
        
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
# done manually now, but I have a separate script to extract automatically from combined df of survey & prgm data

cnt_data <- list(
  kenya = list(
    yr_svy = c(2012.5, 2018.5, 2022.5),
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(4605, 16082, 11562), c(6350, 17880, 25725))),
    num_svy = round(cbind(c(148, 340, 1044), c(116, 436, 1242))),
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  ghana = list(
    yr_svy = c(2017.5, 2022.5),
    ind_svy = (c(2017.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(2553, 4558), c(5575, 6250))),
    num_svy = round(cbind(c(37, 83), c(132, 151))),
    ind_hts = (c(2020, 2021, 2022, 2023) - start) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  )
)

#indices for countries: first testing with 2 countries w/ multiple surveys
# list of survey years for each country
n_cnt <- length(cnt_data) #number of countries: 2
n_svy_by_cnt <- unlist(lapply(cnt_data, function(x) length(x$yr_svy))) #number of surveys per country
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
  dt_yr = 1 / dt, 
  pop = pop,                           
  beta_ind = beta_ind,
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


#fitting the model
options(mc.cores = parallel::detectCores())
fit <- sampling(hivst_stan, data = data_stan, iter = 3000, chains = 4,
                warmup = 1500, thin = 1, control = list(adapt_delta = 0.9))


# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")


#----results with 95%CrI--------
svy_m <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_m"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
svy_f <- as.data.frame(rstan::summary(fit, pars = c("svy_prd_f"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
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

phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`


#------plot--------
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0), mar = c(4, 4, 1, 1))

# survey data
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


# program data 
options(scipen = 999)
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "Number of HIVST kits distributed",
     ylim = c(0, 900000)) #fixed ylim for better interpretability
polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Overall trend", outer = TRUE, side = 3, line = 1, cex = 1.5)





















