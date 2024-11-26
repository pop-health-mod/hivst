
library(wpp2024)
library(rstan)

# male and female pop data from wpp
data(popM1)
data(popF1)

# function to retrieve pop data
get_popdata <- function(cnt_name, year="2020") {
  wpp_m <- popM1[popM1$name == cnt_name, !(colnames(popM1) %in% as.character(c(1949:2010)))]
  wpp_f <- popF1[popF1$name == cnt_name, !(colnames(popF1) %in% as.character(c(1949:2010)))]
  return(c(sum(wpp_m[(15 + 1):(100 + 1), year]) * 1000, 
           sum(wpp_f[(15 + 1):(100 + 1), year]) * 1000))
}

# pop for all countries
countries <- c("Kenya", "Ghana", "Sierra Leone", "Malawi", "Madagascar", "Zimbabwe")
pop <- sapply(countries, get_popdata)#2by6 matrix of male & female

#model starting year: y-3
#indhts

#time specification for different countries(different starting years for different countries)
start_years <- c(2011, 2015, 2015, 2015, 2015, 2015) #country-specific start years
end_year <- 2024  #same for all
dt <- 0.1 #unchanged
time <- list()  #list of time for different countries
niter <- numeric(length(start_years)) #niter different for each country(130 kenya, 90 others)
n_yr <- numeric(length(start_years))  

for (i in 1:length(start_years)) {
  start <- start_years[i]
  time_all <- seq(start, end_year - dt, by = dt)  #time vector for country i
  niter_all <- (end_year - start) / dt  #number of iterations for country i
  n_yr_all <- end_year - start  #number of years for country i
  
  time[[i]] <- time_all  
  niter[i] <- niter_all  
  n_yr[i] <- n_yr_all  
}

#unlist(time) #is it required?

#mapping HIVST rate to the appropriate yearly one(different yr_ind for each country)
yr_ind <- list()

for (i in 1:length(start_years)) {
  niter_all <- niter[i] 
  beta_ind <- seq(1, niter_all, by = 1 / dt)
  yr_ind_all <- rep(1, niter_all) 
  
  for (j in 2:length(beta_ind)) {
    yr_ind_all[beta_ind[j - 1]:(beta_ind[j] - 1)] <- j - 1  
  }
  
  yr_ind_all[(niter_all - 1 / dt + 1):niter_all] <- length(beta_ind)  
  yr_ind[[i]] <- yr_ind_all
}

#unlist(yr_ind_list)

#indices for countries: first testing with 6 countries with multiple surveys
ken <- c(2012, 2018, 2022)             
gha <- c(2017, 2022)                   
sle <- c(2017, 2019)                  
mal <- c(2015, 2019, 2020)            
mdg <- c(2018, 2021)                   
zwe <- c(2015, 2019, 2020)             

#list of survey years for each country
cnt <- list(ken, gha, sle, mal, mdg, zwe)
n_cnt <- length(cnt) #number of countries: 6
n_svy_by_cnt <- unlist(lapply(cnt, length)) #number of surveys/country

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
  real[, , ] hivst_fun(int niter,
                       vector beta_t_dt,
                       real beta_retest,
                       real beta_male,
                       real pop_male,
                       real pop_female,
                       real dt) {
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    real out[niter, 4, 2] = rep_array(0.0, niter, 4, 2);
    
    out[1, 1, 1] = pop[1, c]; //male pop for country c
    out[1, 1, 2] = pop[2, c]; //female pop for country c

    for (i in 2:niter) {
    //males
      out[i, 1, 1] = out[i - 1, 1, 1] + dt * (- rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 2, 1] = out[i - 1, 2, 1] + dt * (+ rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 3, 1] = dt * rr_t[i] * rr_m * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
      out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);
    //females
      out[i, 1, 2] = out[i - 1, 1, 2] + dt * (- rr_t[i] * out[i - 1, 1, 2]);
      out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+ rr_t[i] * out[i - 1, 1, 2]);
      out[i, 3, 2] = dt * rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
      out[i, 4, 2] = out[i, 2, 2] / (out[i, 1, 2] + out[i, 2, 2]);
    }
    return out;
    }
                       
  // function 2
   vector sel_non_zero(vector x) {
   
   int count = 0;
   for (i in 1:size(x)){
    if (x[i] >= 0) {
      count += 1;
      }
   }
   vector[count] non_zero;
   int idx = 1;
   for (i in 1:size(x)){
    if (x[i] >= 0) {
    non_zero[idx] = x[i];
    idx += 1;
      }
   }
    return(non_zero);
   }
}

data {
  int<lower = 1> n_cnt;
  int<lower = 1> n_yr;
  int<lower = 1> niter;
  int<lower = 1> max_niter;  //max iterations across countries
  int<lower = 1> yr_ind[niter];
  real dt;
  real pop[2, n_cnt]; // pop matrix:rows=sex,col=countries
  int<lower = 0> ind_hts[n_cnt, niter];
  int<lower = 0> ind_svy[n_cnt, niter];
  int hivst[n_cnt, n_yr];
  real se_hts[n_cnt, n_yr];
  int<lower = 0> svy_by_cnt[n_cnt];
  int<lower = 0> hts_by_cnt[n_cnt];
  int<lower = 0> num_svy[n_cnt, n_yr];
  int<lower = 1> den_svy[n_cnt, n_yr];
}

parameters {
  real beta_t[n_cnt, n_yr];
  real<lower = 0, upper = 5> sd_rw[n_cnt];
  real beta_retest;
  real beta_male;
  real<lower = 0, upper = 1> phi;
}

transformed parameters {
  real beta_t_dt[n_cnt, niter];
  for (c in 1:n_cnt) {
    for (i in 1:niter) {
      beta_t_dt[c, i] = beta_t[c, yr_ind[i]];
    }
  }
}

model {
// prior for testing rate
  beta_t[, 1] ~ normal(-5, 2);  
  for (c in 1:n_cnt) {
  beta_t[c, 2:n_yr] ~ normal(beta_t[c, 1:(n_yr - 1)], sd_rw[c]);  
  }

 // priors for fixed parameters (unchanged)
  beta_retest ~ normal(log(1.2), 0.5);
  beta_male ~ normal(log(1), 0.5);
  phi ~ beta(24, 6);

  // loop for each country
  real[max_niter, 4, 2] model_pred;
  for (c in 1:n_cnt) {
    model_pred[i, ,] = hivst_fun(niter[c], beta_t_dt[c, ], beta_retest, beta_male, pop[1, c], pop[2, c], dt);
  
    int<lower = 0> num_svy_c[svy_by_c[c]] = sel_non_zero(num_svy[c, ]);
    int<lower = 0> den_svy_c[svy_by_c[c]] = sel_non_zero(den_svy[c, ]);
    int<lower = 0> hts_c[hts_by_c[c]] = sel_non_zero(hivst[c, ]);

    for (s in 1:svy_by_cnt[c]) {
    num_svy[c, s, 1] ~ binomial(den_svy[c, s, 1], model_pred[ind_svy[c, s], 4, 1]);
    num_svy[c, s, 2] ~ binomial(den_svy[c, s, 2], model_pred[ind_svy[c, s], 4, 2]);
  }

    // prgm data
    vector[niter[c]] hts_m = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / phi;
    real hts_m_pery[n_yr[c]];  // Predicted HTS per year for country c
    for (h in 1:n_yr[c]) {
    hts_m_pery[h] = sum(hts_m[ind_hts[c, h]:to_int(ind_hts[c, h] + (1 / dt) - 1)]);
    hivst[c, h] ~ normal(hts_m_pery[h], se_hts[c, h]);
    } 
  }
}

generated quantities {
  array[n_cnt] vector[niter] hivst_prd;       
  array[n_cnt] vector[niter] svy_prd_m;       
  array[n_cnt] vector[niter] svy_prd_f;       
  array[n_cnt, max_niter, 4, 2] real pred;  
  array[n_cnt] real hts_pred_pery[max(n_yr)]; //per year self-tests for each country

  // loop for each country
  for (c in 1:n_cnt) {
  for (i in 1:niter[c]) {
    pred[c, i, ] = hivst_fun(niter[c], beta_t_dt[c, ], beta_retest, beta_male, pop[1,c], pop[2,c], dt);
    
    svy_prd_m[c] = pred[c][, 4, 1];  // males ever HIVST
    svy_prd_f[c] = pred[c][, 4, 2];  // females ever HIVST
    
    hivst_prd[c] = (to_vector(pred[c][, 3, 1]) + to_vector(pred[c][, 3, 2])) / phi;
    for (i in 1:n_yr[c]) {
    hts_pred_pery[c][i] = sum(hivst_prd[c][beta_ind[c, i]:to_int(beta_ind[c, i] + (1 / dt) - 1)]);
    }
  }
}
'

# Compiling the model and exposing functions
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

#Survey and program data for each country
cnt_data <- list(
  kenya = list(
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start_years[1]) / dt,
    den_svy = round(cbind(c(4605, 16082, 11562), c(6350, 17880, 25725))),
    num_svy = round(cbind(c(148, 340, 1044), c(116, 436, 1242))),
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start_years[1]) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  ghana = list(
    ind_svy = (c(2017.5, 2022.5) - start_years[2]) / dt,
    den_svy = round(cbind(c(2553, 4558), c(5575, 6250))),
    num_svy = round(cbind(c(37, 83), c(132, 151))),
    ind_hts = (c(2020, 2021, 2022, 2023) - start_years[2]) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  ),
  sierraleone = list(
    ind_svy = (c(2017.5, 2019.5) - start_years[3]) / dt,
    den_svy = round(cbind(c(2465, 2907), c(5096, 2607))),
    num_svy = round(cbind(c(50, 62), c(165, 101))),
    ind_hts = (c(2020, 2021, 2022) - start_years[3]) / dt,
    hts_dat = c(2500, 270, 11050),
    se_hts = c(2500, 270, 11050) * 0.1
  ),
  malawi = list(
    ind_svy <- (c(2015.5, 2020.5) - start_years[4]) / dt,
    den_svy = round(cbind(c(2796, 5165), c(14792, 5920))),
    num_svy = round(cbind(c(30, 406), c(136, 373))),
    ind_hts = (c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023) - start_years[4]) / dt,
    hts_dat = c(63460, 228021, 501889, 830402, 1780000, 1000000, 1488750, 2699100),
    se_hts = c(63460, 228021, 501889, 830402, 1780000, 1000000, 1488750, 2699100) * 0.1
  ),
  madagascar = list(
    ind_svy = (c(2018.5, 2021.5) - start_years[5]) / dt,
    den_svy = round(cbind(c(3055, 6178), c(5039, 6825))),
    num_svy = round(cbind(c(35, 44), c(84, 20))),
    ind_hts = (c(2022, 2023) + 0.5 - start_years[5]) / dt,
    hts_dat = c(2500, 2500),
    se_hts = c(2500, 2500) * 0.1
  ),
  zimbabwe = list(
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start_years[6]) / dt,
    den_svy = round(cbind(c(6717, 3343, 6576), c(7964, 8104, 10058))),
    num_svy = round(cbind(c(118, 171, 381), c(21, 447, 594))),
    ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start_years[6]) / dt,
    hts_dat = c(174566, 240434, 459517, 414499, 513090),
    se_hts = c(174566, 240434, 459517, 414499, 513090) * 0.1
  )
)

#adding svy_dat, lci,uci after list
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
  n_cnt = length(cnt_data),            # Number of countries
  n_yr = n_yr,                         # Number of years
  yr_ind = yr_ind,                     # Year indices for time steps
  niter = niter,                       # Number of time steps
  dt = dt,                             # Time step size
  pop = pop,                           # Population matrix [2 x n_cnt]
  beta_ind = beta_ind,
  n_svy = n_svy_by_cnt,                # Vector: number of surveys per country
  n_hts = sapply(cnt_data, function(x) length(x$hts_dat)), # programm data points per country
  ind_svy = ind_svy,                   # Matrix: indices for surveys
  ind_hts = ind_hts,                   # Matrix: indices for program data
  hivst = hts_dat,                     # Matrix: HIVST program data
  se_hts = se_hts,                     # Matrix: standard errors for program data
  num_svy = num_svy,                   # Matrix: numerator of survey data
  den_svy = den_svy                    # Matrix: denominator of survey data
)
rstan_options(auto_write = TRUE)


#fitting the model
options(mc.cores = parallel::detectCores())
fit <- sampling(hivst_stan, data = data_stan, iter = 3000, chains = 4,
                warmup = 1500, thin = 1, control = list(adapt_delta = 0.8))

summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "beta_male")
traceplot(fit, pars = "phi")

# we get the results
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


#plot
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0), mar = c(4, 4, 1, 1))

# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "ever used HIVST", ylim = c(0, 0.2))
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
options(scipen = 999)
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number of HIVST kits",
     ylim = c(0, 900000)) #fixed ylim for better interpretability
polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)






















