
setwd("D:\\Downloads\\MSc Thesis\\hivst")

library(devtools)
options(timeout = 600)
library(wpp2024)
library(rstan)


# ---- Zimbabwe ----
data(popB1)
zimbabwe_pop <- popB1[popB1$name == "Zimbabwe", !(colnames(popB1) %in% as.character(c(1949:2010)))]

# indexing starts from age 0
pop_15_19 <- sum(zimbabwe_pop[(15+1):(19+1), "2020"]) * 1000
pop_20_49 <- sum(zimbabwe_pop[(20+1):(49+1), "2020"]) * 1000
pop_50_plus <- sum(zimbabwe_pop[(50+1):(100+1), "2020"]) * 1000

zimb_pop <- c(pop_15_19, pop_20_49, pop_50_plus)

# time specification
start <- 2013 #starting at 2013 (3 years before hivst policy)
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
hivst_mod <- '
functions {
  // function to run the model for a given country
  real[, , ] hivst_fun(int niter,
                   vector beta_t_dt,
                   real beta_retest,
                   vector pop,   // pop now has 3 values, one for each age group
                   real dt) {

    // we convert the beta from the log scale
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    
    // we initialize the 2 compartments (nvr, evr) for hivst
    real out[niter, 4, 3];  // 3 for the 3 age groups
    out = rep_array(0.0, niter, 4, 3);
    
    // Initialize the population for the 3 age groups
    out[1, 1, 1] = pop[1];  // Age group 15-19
    out[1, 1, 2] = pop[2];  // Age group 20-49
    out[1, 1, 3] = pop[3];  // Age group 50+

    for (i in 2:niter) {
    // ode for age group 15-19
      out[i, 1, 1] = out[i - 1, 1, 1] + dt * (- rr_t[i] * out[i - 1, 1, 1]);
      out[i, 2, 1] = out[i - 1, 2, 1] + dt * (+ rr_t[i] * out[i - 1, 1, 1]);
      out[i, 3, 1] = rr_t[i] * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
      out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);

    // ode for age group 20-49
      out[i, 1, 2] = out[i - 1, 1, 2] + dt * (- rr_t[i] * out[i - 1, 1, 2]);
      out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+ rr_t[i] * out[i - 1, 1, 2]);
      out[i, 3, 2] = rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
      out[i, 4, 2] = out[i, 2, 2] / (out[i, 1, 2] + out[i, 2, 2]);

    // ode for age group 50+
      out[i, 1, 3] = out[i - 1, 1, 3] + dt * (- rr_t[i] * out[i - 1, 1, 3]);
      out[i, 2, 3] = out[i - 1, 2, 3] + dt * (+ rr_t[i] * out[i - 1, 1, 3]);
      out[i, 3, 3] = rr_t[i] * (out[i - 1, 1, 3] + rr_r * out[i - 1, 2, 3]);
      out[i, 4, 3] = out[i, 2, 3] / (out[i, 1, 3] + out[i, 2, 3]);
    }

  // we assign the outputs
    return out;
  }
}

data {
  int<lower = 1> n_yr;        // number of years
  int<lower = 1> niter;       // number of iterations
  int<lower = 1> yr_ind[niter]; // to map the yearly rate to the dt ones
  real dt;                    // time step of ode
  vector[3] pop;              // population for 3 age groups from wpp
  int<lower = 0> n_hts;       // number of years with observed hivst data
  int<lower = 1> n_svy;       // number of years with observed survey data  
  int<lower = 0> ind_hts[n_hts]; // indices to get hts (program data)
  int<lower = 0> ind_svy[n_svy]; // indices to get survey data
  int hivst[n_hts];           // number of hivst performed per year (program data)
  real se_hts[n_hts];         // standard error for program data
  int<lower = 0> num_svy[n_svy, 3]; // numerator for survey (age-stratified)
  int<lower = 1> den_svy[n_svy, 3]; // denominator for survey (age-stratified)
}


parameters {
  real beta_t[n_yr];                  // yearly hivstesting rates (rw1)
  real<lower = 0, upper = 5> sd_rw;   // standard deviation of the rw1 for beta_t
  real beta_retest;                   // re-testing rate (same for all age groups)
  real<lower = 0, upper = 1> phi;     // proportion of hivst kits distributed that are used
}

transformed parameters {
  // here we assign the yearly hivst rates for each dt (niter)
  vector[niter] beta_t_dt;
  for (i in 1:niter) {
    beta_t_dt[i] =  beta_t[yr_ind[i]];
  }
}


model {
  vector[niter] hts_m;
  // we use our custom function to get the expected proportion of hivst users and tests
  real model_pred[niter, 4, 3] = hivst_fun(niter, beta_t_dt, beta_retest, pop, dt); // Adjusted to 3 age groups
  // priors
  beta_t[1] ~ normal(-5, 2);      // exp(-5 + c(-1, 1) * 2 * 1.96)
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr - 1)], sd_rw);
  sd_rw ~ normal(0, 1) T[0, 5];
  beta_retest ~ normal(log(1.2), 0.5); // exp(log(1.2) + c(-1, 1) * 1.96 * 0.5)
  phi ~ beta(24, 6);                   // plot(dbeta(x = seq(0, 1, 0.01), shape1 = 6, shape2 = 1.5), type = "l")

  // fitting to survey data for 3 age groups
  for (i in 1:3) {
    num_svy[, i] ~ binomial(den_svy[, i], model_pred[ind_svy, 4, i]);
  }

  // fitting to hivst program data
  hts_m = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2]) + to_vector(model_pred[, 3, 3])) / phi;
  hivst ~ normal(hts_m[ind_hts], se_hts);
}

generated quantities {
  vector[niter] hivst_prd;
  real svy_prd_15_19[niter];
  real svy_prd_20_49[niter];
  real svy_prd_50_plus[niter];   
  
  real pred[niter, 4, 3] = hivst_fun(niter, beta_t_dt, beta_retest, pop, dt); // Adjusted to 3 age groups
  
  // Survey predictions for 3 age groups
  svy_prd_15_19 = pred[, 4, 1];
  svy_prd_20_49 = pred[, 4, 2];
  svy_prd_50_plus = pred[, 4, 3];
  
  // Total HIVST predictions (summed across age groups)
  hivst_prd = (to_vector(pred[, 3, 1]) + to_vector(pred[, 3, 2]) + to_vector(pred[, 3, 3])) / phi;
}
'

# we compile the model and expose the function to invoke it directly in R
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)


# survey data and hivst program data
ind_svy <- (c(2015.5, 2019.5, 2020.5) - start) / dt
den_svy <- round(cbind(c(8396, 4179, 8220), c(9955, 10130, 12573)) * 0.8)
num_svy <- round(cbind(c(147, 214, 476), c(26, 559, 742)) * 0.8)
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c(2019, 2020, 2021, 2022, 2023) + 0.5 - start) / dt
hts_dat <- c(174566, 240434, 459517, 414499, 513090)
se_hts <- hts_dat * 0.1


# data for fitting and running
data_stan <- list(n_yr = n_yr,
                  yr_ind = yr_ind,
                  niter = niter,
                  dt = dt,
                  pop = pop,
                  n_svy = nrow(num_svy),
                  n_hts = length(hts_dat),
                  ind_svy = ind_svy,
                  ind_hts = ind_hts,
                  hivst = hts_dat,
                  se_hts = se_hts,
                  num_svy = num_svy,
                  den_svy = den_svy
)
rstan_options(auto_write = TRUE)


# Fitting 

# we fit the model (this step can take a few minutes)
options(mc.cores = parallel::detectCores())
fit <- sampling(hivst_stan, data = data_stan, iter = 3000, chains = 4,
                warmup = 1500, thin = 1, control = list(adapt_delta = 0.8))

#summary(fit)

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

par(mfrow = c(1, 2), oma = c(0, 0, 3, 0), mar = c(4, 4, 1, 1))
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
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number of HIVST kits",
     ylim = c(0, max(hts$`97.5%`)))
polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)

mtext("Zimbabwe", outer = TRUE, side = 3, line = 1, cex = 1.5)

