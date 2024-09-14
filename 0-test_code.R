# we install the wpp 2024 data (https://github.com/PPgp/wpp2024)
library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2024")

# we load libraries
library(wpp2024)
library(rstan)

# we take Kenya as a test countries
data(popB1)
ken <- popB1[popF1$name == "Kenya", !(colnames(popB1) %in% as.character(c(1949:2010))) ]
#death rates
data(mxB1)
ken_dr <- mxB1[mxB1$name == "Kenya", !(colnames(mxB1) %in% as.character(c(1949:2010, 2023:2100)))]

# time specification
start <- 2015
end <- 2024
dt <- 0.1
time <- seq(start, end - dt, by = dt)
niter <- (end - start) / dt 
n_yr <- end - start

# we create a vector to map the hivst rate to the appropriate yearly one
beta_ind <- seq(1, niter, by = 1 / dt)
yr_ind <- rep(1, niter)
for (i in 2:length(beta_ind)) {
  yr_ind[beta_ind[i - 1]:(beta_ind[i] - 1)] <- i - 1
}
  yr_ind[(niter - 1 / dt + 1):niter] <- length(beta_ind)

#' ----------------
# ---- stan code ----
#' ----------------
library(rstan)
hivst_mod <- '
functions {
  // function to run the model for a given country
  matrix hivst_fun(int niter,
                   vector beta_t_dt,
                   real beta_retest,
                   real pop,
                   real dt) {

    // we convert the beta from the log scale
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    
    // we initialize the 2 compartments (nvr, evr) for hivst
    matrix[niter, 5] out;
    vector[niter] time;
    vector[niter] nvr;
    vector[niter] evr;
    vector[niter] prp;
    vector[niter] hivst_yr = rep_vector(0.0, niter);
    time[1] = 0.0;
    nvr[1] = pop;
    evr[1] = 0;
    prp[1] = 0;
    
    for (i in 2:niter) {
    // ode
      time[i] = time[i - 1] + dt;
      nvr[i] = nvr[i - 1] + dt * (- rr_t[i - 1] * nvr[i - 1]);
      evr[i] = evr[i - 1] + dt * (+ rr_t[i - 1] * nvr[i - 1]);
      hivst_yr[i] = (rr_t[i - 1] * (nvr[i - 1] + rr_r * evr[i - 1]));
      prp[i] = evr[i] / (evr[i] + nvr[i]);
    }

  // we assign the outputs
    out[, 1] = time;
    out[, 2] = nvr;
    out[, 3] = evr;
    out[, 4] = hivst_yr;
    out[, 5] = prp;
    return(out);
}
}

data {
  int<lower = 1> n_yr;        // number of years
  int<lower = 1> niter;       // number of iterations
  int<lower = 1> yr_ind[niter]; // to map the yearly rate to the dt ones
  real dt;                    // time step of ode
  real pop;                   // population from wpp
  int<lower = 0> n_hts;       // numbers of years with observed hivst data
  int<lower = 0> n_svy;       // numbers of years with observed survey data  
  int<lower = 0> ind_hts[n_hts]; // indices to get hts
  int<lower = 0> ind_svy[n_svy]; // indices to get svy
  int hivst[n_hts];      // number of hivst performed per year
  real se_hts[n_hts];    // standard error for program data
  int<lower = 0> num_svy[n_svy]; // numerator proportion survey
  int<lower = 1> den_svy[n_svy]; // denominator proportion survey
}

parameters {
  real beta_t[n_yr];
  real beta_retest;
  real<lower = 0, upper = 5> sd_rw;
  real<lower = 0, upper = 1> phi;
}

transformed parameters {
  vector[niter] beta_t_dt;
  for (i in 1:niter) {
    beta_t_dt[i] =  beta_t[yr_ind[i]];
  }
}

model {
  vector[niter] hts_m;
  // we use our custom function to get the expected proportion of HIVST users and tests
  matrix[niter, 5] model_pred = hivst_fun(niter, beta_t_dt, beta_retest, pop, dt);
  // priors
  beta_t[1] ~ normal(-5, 2);      // exp(-5 + c(-1, 1) * 2)
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr - 1)], sd_rw);
  sd_rw ~ normal(0, 1) T[0, 5];
  beta_retest ~ normal(log(1.25), 0.3);    // exp(log(1.25) + c(-1, 1) * 0.3)
  phi ~ beta(24, 6);            // plot(dbeta(x = seq(0, 1, 0.01), shape1 = 6, shape2 = 1.5), type = "l")

  // fitting to survey data
    num_svy ~ binomial(den_svy, model_pred[ind_svy, 5]);
  // fitting to hivst program data
    hts_m = model_pred[, 4] / phi;
    hivst ~ normal(hts_m[ind_hts], se_hts);
}

generated quantities {
    vector[niter] hivst_prd;
    vector[niter] svy_prd;
    matrix[niter, 5] pred = hivst_fun(niter, beta_t_dt, beta_retest, pop, dt);
      svy_prd = pred[, 5];
      hivst_prd = pred[, 4] / phi;
}
'
# we compile the model and expose the function to invoke it directly in R
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

# testing if it works
pop <- sum(ken[(15 + 1):(100 + 1), "2023"]) * 1000
beta_t <- log(c(0.001, 0.01, 0.03, 0.03, 0.04, 0.05, 0.07, 0.07, 0.06))
beta_t_dt <- rep(0, niter)
for (i in 1:niter) {
  beta_t_dt[i] <- beta_t[yr_ind[i]]
}

prd <- hivst_fun(niter, beta_t_dt, beta_retest = log(1.25), pop, dt)
par(mfrow = c(1, 2))
plot(prd[, 5] ~ I(prd[, 1] + start), type = "l", ylim = c(0, 1),
     xlab = "time", ylab = "ever used hivst")
plot(prd[, 4] ~ I(prd[, 1] + start), type = "l", 
     xlab = "time", ylab = "annual number hivst")

ind_svy <- (c(2018, 2020, 2023) - start) / dt
svy_dat <- prd[ind_svy, 5]
den_svy <- c(1000, 5000, 3000)
num_svy <- round(svy_dat * den_svy)
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c(2018:2023) - start) / dt
hts_dat <- round(prd[ind_hts, 4] / 0.85)
se_hts <- hts_dat * 0.1

# data for fitting and running
data_stan <- list(n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                  pop = pop,
                  n_svy = length(num_svy),
                  n_hts = length(hts_dat),
                  ind_svy = ind_svy,
                  ind_hts = ind_hts,
                  hivst = hts_dat,
                  se_hts = se_hts,
                  num_svy = num_svy,
                  den_svy = den_svy
                  )
rstan_options(auto_write = TRUE)

#' ----------------
# ---- Fitting ----
#' ----------------
# we fit the model (this step can take a few minutes)
options(mc.cores = parallel::detectCores())
fit <- sampling(hivst_stan, data = data_stan, iter = 1500, chains = 4,
                warmup = 500, thin = 1, control = list(adapt_delta = 0.9))
summary(fit)

# traceplots
traceplot(fit, pars = "beta_t")
traceplot(fit, pars = "sd_rw")
traceplot(fit, pars = "beta_retest")
traceplot(fit, pars = "phi")

# we get the results
svy <- as.data.frame(rstan::summary(fit, pars = c("svy_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
hts <- as.data.frame(rstan::summary(fit, pars = c("hivst_prd"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r <- as.data.frame(rstan::summary(fit, pars = c("beta_t"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(r$`50%`)
rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr$`50%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "ever hivst", ylim = c(0, 0.5))
  polygon(x = c(time, rev(time)),
        y = c(svy$`2.5%`, rev(svy$`97.5%`)),
        col = yarrr::transparent("steelblue4", trans.val = 0.5), border = NA)
points(svy_dat ~ time[ind_svy], pch = 16, col = "firebrick4")
segments(x0 = time[ind_svy], y0 = lci_svy,
         x1 = time[ind_svy], y1 = uci_svy, col = "firebrick4")

# hts fit
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number hivst")
  polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)
