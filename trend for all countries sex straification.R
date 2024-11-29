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

#time specification
start <- 2009 #same starting year for all countries
end <- 2024  #same for all
dt <- 0.1 #unchanged
time <- seq(start, end - dt, by = dt) 
niter <- (end - start) / dt
n_yr <- end - start

#mapping HIVST rate to the appropriate yearly indices
beta_ind <- seq(1, niter, by = 1 / dt) #15 indices of testing rate indicating beginning of each year
yr_ind <- rep(1, niter) 
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}

yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)


#indices for countries: first testing with 2 countries w/ multiple surveys
ken <- c(2012, 2018, 2022)
gha <- c(2017, 2022) 

#list of survey years for each country
cnt <- list(ken, gha)
n_cnt <- length(cnt) #number of countries: 2
n_svy_by_cnt <- unlist(lapply(cnt, length)) #number of surveys per country

start_idx <- NULL
end_idx <- NULL
start_idx[1] <- 1  
end_idx[1] <- n_svy_by_cnt[1]

#remaining countries
for (c in 2:n_cnt) {
  start_idx[c] <- start_idx[c - 1] + n_svy_by_cnt[c - 1]
  end_idx[c] <- start_idx[c] + n_svy_by_cnt[c] - 1
}


#stan code
hivst_mod <- '
functions {
  real[, , ] hivst_fun(int n_cnt, // # of countries added 
                       int niter,
                       vector[niter] beta_t_dt[n_cnt],
                       real beta_retest,
                       real beta_male,
                       real pop[2, n_cnt], // male&female for n countries
                       real dt) {
     
     // intializing ouputs:adding layer of country
    real out[n_cnt, niter, 4, 2] = rep_array(0.0, n_cnt, niter, 4, 2);
    
    // now need to loop for each country
    for (c in 1:n_cnt) {
    
    // converting rates from log scale
    vector[niter] rr_t = exp(beta_t_dt[c]); // for each cnt testing&retesting rate
    real rr_r = exp(beta_retest[c]);
    real rr_m = exp(beta_male); // assuming fixed for all cntries
    
    // initializing male&female compartments for each country
    out[c, 1, 1, 1] = pop[1, c]; // male, cnt c
    out[c, 1, 1, 2] = pop[2, c]; // female, cnt c

   // odes (per country)
    for (i in 2:niter) {
   // males for c cntry, i iteration
        out[c, i, 1, 1] = out[c, i - 1, 1, 1] + dt * (-rr_t[i] * rr_m * out[c, i - 1, 1, 1]);
        out[c, i, 2, 1] = out[c, i - 1, 2, 1] + dt * (+rr_t[i] * rr_m * out[c, i - 1, 1, 1]);
        out[c, i, 3, 1] = dt * rr_t[i] * rr_m * (out[c, i - 1, 1, 1] + rr_r * out[c, i - 1, 2, 1]);
        out[c, i, 4, 1] = out[c, i, 2, 1] / (out[c, i, 1, 1] + out[c, i, 2, 1]);
        
    // females for c cntry, i iteration
        out[c, i, 1, 2] = out[c, i - 1, 1, 2] + dt * (-rr_t[i] * out[c, i - 1, 1, 2]);
        out[c, i, 2, 2] = out[c, i - 1, 2, 2] + dt * (+rr_t[i] * out[c, i - 1, 1, 2]);
        out[c, i, 3, 2] = dt * rr_t[i] * (out[c, i - 1, 1, 2] + rr_r * out[c, i - 1, 2, 2]);
        out[c, i, 4, 2] = out[c, i, 2, 2] / (out[c, i, 1, 2] + out[c, i, 2, 2]);
      }
    return out;
    }
} 
}

data {
  int<lower = 1> n_cnt;             // # of countries
  int<lower = 1> n_yr;              // # of years (same for all countries)
  int<lower = 1> niter;             // #of iterations (same for all countries)
  int<lower = 1> yr_ind[niter];     // mapping the yearly rate to the dt ones
  real dt;                          
  real pop[2, n_cnt];               // r = sex, c = cnt
  int<lower = 1> beta_ind[n_yr];    // beta indices (common across cntries)
  
  // for prgrm data
  int<lower = 0> n_hts[n_cnt];      // # of years with observed HIVST data per cntry
  int<lower = 0> ind_hts[n_cnt, max(n_hts)]; //indices pgm data(max length?)
  int<lower = 0> hivst[n_cnt, max(n_hts)];   //# of HIVST performed per year per cnty
  real se_hts[n_cnt, max(n_hts)];   // se for program data(max lenght required?)
  
  // survey data & proportions
  int<lower = 1> n_svy_by_cnt[n_cnt]; // # of surveys per country
  int<lower = 0> ind_svy[n_cnt, max(n_svy_by_cnt)]; // indices for survey data (max length across countries)
  int<lower = 0> num_svy[n_cnt, max(n_svy_by_cnt), 2]; // numerator for survey proportions
  int<lower = 1> den_svy[n_cnt, max(n_svy_by_cnt), 2]; // denominator for survey proportions
}

parameters {
  matrix[n_cnt, n_yr] beta_t;          // yearly HIVST rates (rw1) for each country
  real<lower = 0, upper = 5> sd_rw;    // sd of the rw1 for beta_t
  real beta_retest_overall;            // overall shared re-testing rate
  real beta_retest[n_cnt];            // country-specific re-testing rates
  real beta_male;                      //male relative rate of HIVST (same for all cntries)
  real<lower = 0, upper = 1> phi_overall; // overall proportion of HIVST kits used
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
  // priors
  sd_rw ~ normal(0, 1) T[0, 5];
  beta_retest_overall ~ normal(log(1.2), 0.5);
  beta_male ~ normal(log(1), 0.5);
  phi_overall ~ normal(logit(0.85), 0.5);

  for (c in 1:n_cnt) {
    beta_retest[c] ~ normal(beta_retest_overall, 0.5);
    phi[c] ~ normal(phi_overall, 0.5);
    beta_t[c, 1] ~ normal(-5, 2);
    beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw);
  }

  // model predictions and likelihoods 
  for (c in 1:n_cnt) {
    real model_pred[niter, 4, 2] = hivst_fun(niter, beta_t_dt[c, ], beta_retest[c], beta_male, pop[, c], dt);

    // fitting to survey data (layer of country and surveys)
    for (s in 1:svy_by_cnt[c]) {
      num_svy[c, s, 1] ~ binomial(den_svy[c, s, 1], model_pred[ind_svy[c, s], 4, 1]);
      num_svy[c, s, 2] ~ binomial(den_svy[c, s, 2], model_pred[ind_svy[c, s], 4, 2]);
    }

    // fitting to program data(layer of country & years we are modeling)
    vector[niter] hts_m = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / inv_logit(phi[c]);
    real hts_m_pery[n_yr]; // predicted # of HTS per y for each c
    for (h in 1:n_yr) {
      hts_m_pery[h] = sum(hts_m[ind_hts[c, h]:to_int(ind_hts[c, h] + (1 / dt) - 1)]);
      hivst[c, h] ~ normal(hts_m_pery[h], se_hts[c, h]);
    }
  }
}

generated quantities {
    matrix[n_cnt, niter] hivst_prd;  // predicted HIVST rates per time step for all countries
    matrix[n_cnt, niter] svy_prd_m;  // predicted survey proportions (males) per country
    matrix[n_cnt, niter] svy_prd_f;  // predicted survey proportions (females) per country
    array[n_cnt, niter, 4, 2] pred;  // predicted outputs of the compartments for each country
    matrix[n_cnt, n_yr] hts_pred_pery;  // per-year predicted self-tests for each country

    for (c in 1:n_cnt) {
        pred[c] = hivst_fun(niter, beta_t_dt[c, ], beta_retest[c], beta_male, pop[, c], dt);
        
        // survey prediction
        for (i in 1:niter) {
            svy_prd_m[c, i] = pred[c, i, 4, 1];  // males ever used HIVST
            svy_prd_f[c, i] = pred[c, i, 4, 2];  // females ever used HIVST
        }
        
        // hts predictions (slicing operator : takes all niter for each country)
        hivst_prd[c] = (to_vector(pred[c, :, 3, 1]) + to_vector(pred[c, :, 3, 2])) / inv_logit(phi[c]);
        
        // yearly predictions # of Hts
        for (i in 1:n_yr) {
            hts_pred_pery[c, i] = sum(hivst_prd[c, beta_ind[i]:to_int(beta_ind[i] + (1 / dt) - 1)]);
        }
    }
}
'

# Compiling the model and exposing functions
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

#------ Survey & program data for each country ------------
#done manually now, but I have a seperate script to extract automatically from combined df of survey & prgm data

cnt_data <- list(
  kenya = list(
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(4605, 16082, 11562), c(6350, 17880, 25725))),
    num_svy = round(cbind(c(148, 340, 1044), c(116, 436, 1242))),
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  ghana = list(
    ind_svy = (c(2017.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(2553, 4558), c(5575, 6250))),
    num_svy = round(cbind(c(37, 83), c(132, 151))),
    ind_hts = (c(2020, 2021, 2022, 2023) - start) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  )
)

#adding svy_dat, lci,uci after list as the operations cant be perfomed inside list
cnt_data <- lapply(cnt_data, function(x) {
  x$svy_dat <- x$num_svy / x$den_svy
  x$lci_svy <- x$svy_dat - qnorm(0.975) * sqrt(x$svy_dat * (1 - x$svy_dat) / x$den_svy)
  x$uci_svy <- x$svy_dat + qnorm(0.975) * sqrt(x$svy_dat * (1 - x$svy_dat) / x$den_svy)
  return(x)
})


#combining svy and pgm data for all countries
num_svy <- do.call(rbind, lapply(cnt_data, function(x) x$num_svy)) #rows=countries, cols=surveys
den_svy <- do.call(rbind, lapply(cnt_data, function(x) x$den_svy))
ind_svy <- do.call(rbind, lapply(cnt_data, function(x) x$ind_svy))
hts_dat <- do.call(rbind, lapply(cnt_data, function(x) x$hts_dat)) #rows=countries, cols=time points
se_hts <- do.call(rbind, lapply(cnt_data, function(x) x$se_hts))
ind_hts <- do.call(rbind, lapply(cnt_data, function(x) x$ind_hts))

# fitting data for running in stan
data_stan <- list(
  n_cnt = length(cnt_data),            
  n_yr = n_yr,                         
  yr_ind = yr_ind,                     
  niter = niter,                       
  dt = dt,                             
  pop = pop,                           
  beta_ind = beta_ind,
  n_svy = n_svy_by_cnt,                
  n_hts = sapply(cnt_data, function(x) length(x$hts_dat)), 
  ind_svy = ind_svy,                   
  ind_hts = ind_hts,                
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

summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
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





















