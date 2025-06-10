data {
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  real<lower=0> build_dens[N_buildings];
}

parameters {
  real mu;
  real<lower=1e-3> beta;
}

transformed parameters {
  real<lower=1e-8, upper=1-1e-8> damage_probs[N_buildings];

  for (n in 1:N_buildings) {
    damage_probs[n] = fmin(fmax(lognormal_cdf(PGV[n], mu, beta), 1e-8), 1 - 1e-8);
  }
}

model {
  mu ~ normal(-0.5, 0.6);
  beta ~ normal(0.6, 0.05);

  for (n in 1:N_buildings) {
    damage_flag[n] ~ bernoulli(damage_probs[n]);
  }
}