// Poisson model example

data {
  // Define variables in data
  // Number of observations (an integer)
  int<lower=0> N;
  // Number of beta parameters
  int<lower=0> p;
  
  // Covariates
  real intercept[N];
  real cycle[N];
  real motor[N];
  
  // offset
  real offset[N];
  
  // Count outcome
  int<lower=0> y[N];
}

parameters {
  // Define parameters to estimate
  real beta[p];
}

transformed parameters  {
  //
  real beta_sum;
  real lp[N];
  real <lower=0> mu[N];
  
  for (i in 1:N) {
    // Linear predictor
    lp[i] = beta[1] + beta[2]*cycle[i] + beta[3]*motor[i] + offset[i];
    
    // Mean
    mu[i] = exp(lp[i]);
  }
  beta_sum = beta[3] + beta[2];
}

model {
  // Prior part of Bayesian inference
  // Flat prior for mu (no need to specify if non-informative)
  
  
  // Likelihood part of Bayesian inference
  y ~ poisson(mu);
}