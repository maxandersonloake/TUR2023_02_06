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
  //real<lower=0.8, upper=1> alph1;
  //real<lower=2, upper=15> alph2;
  real<lower=0.5, upper=1> alph1;
  real<lower=10, upper=300> alph2;
}

transformed parameters {
  real<lower=1e-8, upper=1-1e-8> damage_probs[N_buildings];
  real<lower=1e-8, upper=1-1e-8> p_ministrys_unobserved[N_buildings];

  for (n in 1:N_buildings) {
    damage_probs[n] = fmin(fmax(lognormal_cdf(psa[n], mu, beta), 1e-8), 1 - 1e-8);
    //p_ministrys_unobserved[n] =  fmin(fmax(alph1 * (1-exp(-alph2 * build_dens[n]/100)), 1e-8), 1 - 1e-8);
    p_ministrys_unobserved[n] = (1-fmin(fmax(alph1 *  sqrt(build_dens[n])/(sqrt(build_dens[n])+alph2), 1e-8), 1 - 1e-8));
  }
}

model {
  mu ~ normal(prior_mu[1], prior_mu[2]);
  beta ~ normal(prior_beta[1], prior_beta[2]);
  
  //alph2 ~ normal(8, 3);

  for (n in 1:N_buildings) {
    if (damage_flag[n] == 1) {
      target += log(damage_probs[n]) + log1m(p_ministrys_unobserved[n]);
    } else {
      target += log_sum_exp(
        log1m(damage_probs[n]),                         // true damage didn't happen
        log(damage_probs[n]) + log(p_ministrys_unobserved[n])  // damage happened but was missed
      );
    }
  }
}