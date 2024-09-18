# we install the wpp 2024 data (https://github.com/PPgp/wpp2024)
library(devtools)
options(timeout = 600)
install_github("PPgp/wpp2024")

# we load libraries
library(wpp2024)
library(rstan)

# we take Kenya as a test countries
data(popB1)
wpp_b <- popB1[popB1$name == "Zimbabwe", !(colnames(popB1) %in% as.character(c(1949:2010))) ]
data(popM1)
wpp_m <- popM1[popM1$name == "Zimbabwe", !(colnames(popM1) %in% as.character(c(1949:2010))) ]
data(popF1)
wpp_f <- popF1[popF1$name == "Zimbabwe", !(colnames(popF1) %in% as.character(c(1949:2010))) ]

# death rates
data(mxB1)
ken_dr <- mxB1[mxB1$name == "Zimbabwe", !(colnames(mxB1) %in% as.character(c(1949:2010, 2023:2100)))]

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
  real[, , ] hivst_fun(int niter,
                   vector beta_t_dt,
                   real beta_retest,
                   real beta_male,
                   vector pop,
                   real dt) {

    // we convert the beta from the log scale
    vector[niter] rr_t = exp(beta_t_dt);
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    
    // we initialize the 2 compartments (nvr, evr) for hivst
    real out[niter, 4, 2];
    out = rep_array(0.0, niter, 4, 2);
    out[1, 1, 1] = pop[1];
    out[1, 1, 2] = pop[2];

    for (i in 2:niter) {
    // ode males
      out[i, 1, 1] = out[i - 1, 1, 1] + dt * (- rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 2, 1] = out[i - 1, 2, 1] + dt * (+ rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 3, 1] = rr_t[i] * rr_m * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
      out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);
    // ode females
      out[i, 1, 2] = out[i - 1, 1, 2] + dt * (- rr_t[i] * out[i - 1, 1, 2]);
      out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+ rr_t[i] * out[i - 1, 1, 2]);
      out[i, 3, 2] = rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
      out[i, 4, 2] = out[i, 2, 2] / (out[i, 1, 2] + out[i, 2, 2]);
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
  vector[2] pop;                   // population from wpp
  int<lower = 0> n_hts;       // numbers of years with observed hivst data
  int<lower = 1> n_svy;       // numbers of years with observed survey data  
  int<lower = 0> ind_hts[n_hts]; // indices to get hts
  int<lower = 0> ind_svy[n_svy]; // indices to get svy
  int hivst[n_hts];      // number of hivst performed per year
  real se_hts[n_hts];    // standard error for program data
  int<lower = 0> num_svy[n_svy, 2]; // numerator proportion survey (design-adjusted)
  int<lower = 1> den_svy[n_svy, 2]; // denominator proportion survey (design-adjusted)
}

parameters {
  real beta_t[n_yr];                  // yearly hivstesting rates (rw1)
  real<lower = 0, upper = 5> sd_rw;   // standard deviation of the rw1 for beta_t
  real beta_retest;                   // re-testing rate
  real beta_male;                     // male relative rate of hivst (referent = female)
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
  real model_pred[niter, 4, 2] = hivst_fun(niter, beta_t_dt, beta_retest, beta_male, pop, dt);
  // priors
  beta_t[1] ~ normal(-5, 2);      // exp(-5 + c(-1, 1) * 2 * 1.96)
  beta_t[2:n_yr] ~ normal(beta_t[1:(n_yr - 1)], sd_rw);
  sd_rw ~ normal(0, 1) T[0, 5];
  beta_retest ~ normal(log(1.2), 0.5); // exp(log(1.2) + c(-1, 1) * 1.96 * 0.5)
  beta_male ~ normal(log(1), 0.5);     // exp(log(1) + c(-1, 1) * 1.96 * 0.5)
  phi ~ beta(24, 6);                   // plot(dbeta(x = seq(0, 1, 0.01), shape1 = 6, shape2 = 1.5), type = "l")

  // fitting to survey data
    num_svy[, 1] ~ binomial(den_svy[, 1], model_pred[ind_svy, 4, 1]);
    num_svy[, 2] ~ binomial(den_svy[, 2], model_pred[ind_svy, 4, 2]);
  // fitting to hivst program data
    hts_m = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / phi;
    hivst ~ normal(hts_m[ind_hts], se_hts);
}

generated quantities {
    vector[niter] hivst_prd;
    real svy_prd_m[niter];
    real svy_prd_f[niter];   
    real pred[niter, 4, 2] = hivst_fun(niter, beta_t_dt, beta_retest, beta_male, 
                                        pop, dt);
      svy_prd_m = pred[, 4, 1];
      svy_prd_f = pred[, 4, 2];
      hivst_prd = (to_vector(pred[, 3, 1]) + to_vector(pred[, 3, 2])) / phi;
}
'
# we compile the model and expose the function to invoke it directly in R
expose_stan_functions(stanc(model_code = hivst_mod))
hivst_stan <- stan_model(model_code = hivst_mod)

# ---- simulated data -----
# testing if it works (we simulate using fake data)
pop <- c((sum(wpp_m[(15 + 1):(100 + 1), "2020"]) * 1000),
         (sum(wpp_f[(15 + 1):(100 + 1), "2020"]) * 1000))
beta_t <- log(c(0.001, 0.01, 0.03, 0.03, 0.04, 0.05, 0.07, 0.07, 0.06))
beta_t_dt <- rep(0, niter)
for (i in 1:niter) {
  beta_t_dt[i] <- beta_t[yr_ind[i]]
}

prd <- hivst_fun(niter, beta_t_dt, beta_retest = log(1.25), beta_male = log(1.5),
                 pop, dt)
prd <- unlist(prd)
prd <- array(data = prd, dim = c(2, 4, niter))

par(mfrow = c(1, 2))
plot(prd[1, 4, ] ~ I(time + start), type = "l", ylim = c(0, 1),
     xlab = "time", ylab = "ever used hivst", col = "steelblue3", lwd = 2)
lines(prd[2, 4, ] ~ I(time + start), col = "pink3", lwd = 2)
plot(I(prd[1, 3, ] + prd[2, 3, ]) ~ I(time + start), type = "l", 
     xlab = "time", ylab = "annual number hivst", col = "cyan4", lwd = 2)

ind_svy <- (c(2018.5, 2020.5) - start) / dt
svy_dat <- cbind(prd[1, 4, ind_svy], prd[2, 4, ind_svy])
den_svy <- cbind(c(800, 4000), c(1000, 50000))
num_svy <- round(svy_dat * den_svy)
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c(2018:2023) + 0.5 - start) / dt
hts_dat <- round((prd[1, 3, ind_hts] + prd[2, 3, ind_hts]) / 0.85)
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

#' ----------------
# ---- Fitting ----
#' ----------------
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
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "ever hivst", ylim = c(0, 0.5))
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
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number hivst",
     ylim = c(0, max(hts$`97.5%`)))
  polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)


# ---- Kenya real data ----
wpp_m <- popM1[popM1$name == "Kenya", !(colnames(popM1) %in% as.character(c(1949:2010))) ]
wpp_f <- popF1[popF1$name == "Kenya", !(colnames(popF1) %in% as.character(c(1949:2010))) ]

# time specification
start <- 2011
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

  
pop <- c((sum(wpp_m[(15 + 1):(100 + 1), "2020"]) * 1000),
         (sum(wpp_f[(15 + 1):(100 + 1), "2020"]) * 1000))
ind_svy <- (c(2012.5, 2018.5, 2022.5) - start) / dt
den_svy <- round(cbind(c(5756, 20102, 14453), c(7938, 22350, 32156)) * 0.8)
num_svy <- round(cbind(c(185, 425, 1305), c(145, 545, 1552)) * 0.8)
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c( 2018,  2019,   2020,    2021,   2022,  2023) + 0.5 - start) / dt
hts_dat <- c(108200, 50000, 735645, 2236897, 300000, 425000)
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
rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m$`50%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "ever hivst", ylim = c(0, 0.2))
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
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number hivst",
     ylim = c(0, max(hts$`97.5%`)))
  polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)

# ---- Zimbabwe real data ----
wpp_m <- popM1[popM1$name == "Zimbabwe", !(colnames(popM1) %in% as.character(c(1949:2010))) ]
wpp_f <- popF1[popF1$name == "Zimbabwe", !(colnames(popF1) %in% as.character(c(1949:2010))) ]
pop <- c((sum(wpp_m[(15 + 1):(100 + 1), "2020"]) * 1000),
         (sum(wpp_f[(15 + 1):(100 + 1), "2020"]) * 1000))
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

  
ind_svy <- (c(2015.5, 2019.5, 2020.5) - start) / dt
den_svy <- round(cbind(c(8396, 4179, 8220), c(9955, 10130, 12573)) * 0.8)
num_svy <- round(cbind(c(147, 214, 476), c(26, 559, 742)) * 0.8)
svy_dat <- num_svy / den_svy
lci_svy <- svy_dat - qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
uci_svy <- svy_dat + qnorm(0.975) * sqrt(svy_dat * (1 - svy_dat ) / den_svy)
ind_hts <- (c(2016,   2017,  2018,  2019,     2020,   2021,  2023) + 0.5 - start) / dt
hts_dat <- c(66592, 486815, 318694, 194644, 938500, 581250, 250000)
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
rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m$`50%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`

par(mfrow = c(1, 2), oma = c(0, 0, 0, 0), mar = c(4, 4, 1, 1))
# survey fit
plot(svy_m$`50%` ~ time, type = "l", col = "steelblue4", lwd = 3, ylab = "ever hivst", ylim = c(0, 0.2))
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
plot(hts$`50%` ~ time, type = "l", col = "cyan4", lwd = 3, ylab = "number hivst",
     ylim = c(0, max(hts$`97.5%`)))
  polygon(x = c(time, rev(time)),
        y = c(hts$`2.5%`, rev(hts$`97.5%`)),
        col = yarrr::transparent("cyan4", trans.val = 0.5), border = NA)
points(hts_dat ~ time[ind_hts], pch = 16, col = "goldenrod3", cex = 1.25)

