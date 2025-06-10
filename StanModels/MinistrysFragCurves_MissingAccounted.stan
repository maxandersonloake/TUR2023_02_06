data {
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  real<lower=0> build_dens[N_buildings];
}

parameters {
  real mu;
  real<lower=1e-3> beta;
  //real<lower=0.8, upper=1> alph1;
  //real<lower=2, upper=15> alph2;
  real<lower=0.5, upper=1> alph1;
  real<lower=50, upper=250> alph2;
}

transformed parameters {
  real<lower=1e-8, upper=1-1e-8> damage_probs[N_buildings];
  real<lower=1e-8, upper=1-1e-8> p_ministrys_unobserved[N_buildings];

  for (n in 1:N_buildings) {
    damage_probs[n] = fmin(fmax(lognormal_cdf(PGV[n], mu, beta), 1e-8), 1 - 1e-8);
    //p_ministrys_unobserved[n] =  fmin(fmax(alph1 * (1-exp(-alph2 * build_dens[n]/100)), 1e-8), 1 - 1e-8);
    p_ministrys_unobserved[n] = fmin(fmax(alph1 *  sqrt(build_dens[n])/(sqrt(build_dens[n]+alph2)), 1e-8), 1 - 1e-8);
  }
}

model {
  mu ~ normal(-0.5, 0.6);
  beta ~ normal(0.6, 0.05);
  
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