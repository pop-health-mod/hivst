

#indices for countries: first testing with 6 countries with multiple surveys
ken <- c(2012, 2018, 2022)             
gha <- c(2017, 2022)                   
sle <- c(2017, 2019)                  
mal <- c(2015, 2019, 2020)            
mdg <- c(2018, 2021)                   
zwe <- c(2015, 2019, 2020)             

#list of survey years for each country
cnt <- list(ken, gha, sle, mal, mdg, zwe)
n_cnt <- length(cnt)                        # 6 countries
n_svy_by_cnt <- unlist(lapply(cnt, length)) # Number of surveys per country

#initialize
start_idx <- NULL
end_idx <- NULL

#country 1
start_idx[1] <- 1                           # The first survey for the first country starts at index 1
end_idx[1] <- n_svy_by_cnt[1]               # The first country's last survey index

#remaining countries
for (c in 2:n_cnt) {
start_idx[c] <- start_idx[c - 1] + n_svy_by_cnt[c - 1]
end_idx[c] <- start_idx[c] + n_svy_by_cnt[c] - 1
}


#keeping mathieu's stan code
#stan code
hivst_mod <- '
functions {
  real[, , ] hivst_fun(int niter,
                       vector mu_beta_t_dt,
                       vector beta_t_raw_dt,
                       real beta_retest,
                       real beta_male,
                       vector pop,
                       real dt) {
    vector[niter] rr_t = exp(mu_beta_t_dt + beta_t_raw_dt);
    real rr_r = exp(beta_retest);
    real rr_m = exp(beta_male);
    real out[niter, 4, 2] = rep_array(0.0, niter, 4, 2);
    
    out[1, 1, 1] = pop[1];
    out[1, 1, 2] = pop[2];

    for (i in 2:niter) {
      out[i, 1, 1] = out[i - 1, 1, 1] + dt * (- rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 2, 1] = out[i - 1, 2, 1] + dt * (+ rr_t[i] * rr_m * out[i - 1, 1, 1]);
      out[i, 3, 1] = rr_t[i] * rr_m * (out[i - 1, 1, 1] + rr_r * out[i - 1, 2, 1]);
      out[i, 4, 1] = out[i, 2, 1] / (out[i, 1, 1] + out[i, 2, 1]);
      out[i, 1, 2] = out[i - 1, 1, 2] + dt * (- rr_t[i] * out[i - 1, 1, 2]);
      out[i, 2, 2] = out[i - 1, 2, 2] + dt * (+ rr_t[i] * out[i - 1, 1, 2]);
      out[i, 3, 2] = rr_t[i] * (out[i - 1, 1, 2] + rr_r * out[i - 1, 2, 2]);
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
    return(non_zero)
   }
}

data {
  int<lower = 1> n_cnt;
  int<lower = 1> n_yr;
  int<lower = 1> niter;
  int<lower = 1> yr_ind[niter];
  real dt;
  vector[2] pop[n_cnt];


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
      beta_t_dt[c, i] = mu_beta_t[yr_ind[i]];
    }
  }
}

model {
// prior for testing rate
  beta_t[, 1] ~ normal(-5, 2);  
  for (c in 1:n_cnt) {
    mu_beta_t[c, 2:n_yr] ~ normal(mu_beta_t[c, 1:(n_yr - 1)], sd_rw[c]);  
  }

 // priors for fixed parameters (unchanged)
  beta_retest ~ normal(log(1.2), 0.5);
  beta_male ~ normal(log(1), 0.5);
  phi ~ beta(24, 6);

  // loop for each country
  for (c in 1:n_cnt) {
    real[niter, 4, 2] model_pred = hivst_fun(niter, beta_t_dt[c, ],
                beta_retest, beta_male, pop[c], dt);
  
    int<lower = 0> num_svy_c[svy_by_c[c]] = sel_non_zero(num_svy[c, ]);
    int<lower = 0> den_svy_c[svy_by_c[c]] = sel_non_zero(den_svy[c, ]);
    int<lower = 0> hts_c[hts_by_c[c]] = sel_non_zero(hivst[c, ]);

    num_svy[c, s, 1] ~ binomial(den_svy[c, s, 1], model_pred[ind_svy[c, s], 4, 1]);
    num_svy[c, s, 2] ~ binomial(den_svy[c, s, 2], model_pred[ind_svy[c, s], 4, 2]);
    }

    // prgm data
    vector[niter] hts_m = (to_vector(model_pred[, 3, 1]) + to_vector(model_pred[, 3, 2])) / phi;
    for (h in 1:n_yr) {
      hivst[c, h] ~ normal(hts_m[ind_hts[c, h]], se_hts[c, h]);
    }
  }
}

generated quantities {
  array[n_cnt] vector[niter] hivst_prd;       
  array[n_cnt] vector[niter] svy_prd_m;       
  array[n_cnt] vector[niter] svy_prd_f;       
  array[n_cnt, niter, 4, 2] real pred;        

  // loop for each country
  for (c in 1:n_cnt) {
    pred[c] = hivst_fun(niter, beta_t_dt[c], beta_t_raw[c], beta_retest, beta_male, pop[c], dt);

    svy_prd_m[c] = pred[c][, 4, 1];  // males ever HIVST
    svy_prd_f[c] = pred[c][, 4, 2];  // females ever HIVST

    hivst_prd[c] = (to_vector(pred[c][, 3, 1]) + to_vector(pred[c][, 3, 2])) / phi;
  }
}
'

