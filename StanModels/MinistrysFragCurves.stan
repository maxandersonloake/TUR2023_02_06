data {
  int<lower=0> N_buildings;
  real<lower=0> psa[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  real<lower=0> build_dens[N_buildings];
  real prior_mu[2];
  real prior_beta[2];
}

parameters {
  real mu;
  real<lower=1e-3> beta;
}

transformed parameters {
  real<lower=1e-8, upper=1-1e-8> damage_probs[N_buildings];

  for (n in 1:N_buildings) {
    damage_probs[n] = fmin(fmax(lognormal_cdf(psa[n], mu, beta), 1e-8), 1 - 1e-8);
  }
}

model {
  
  mu ~ normal(prior_mu[1], prior_mu[2]);
  beta ~ normal(prior_beta[1], prior_beta[2]);

  for (n in 1:N_buildings) {
    damage_flag[n] ~ bernoulli(damage_probs[n]);
  }
}