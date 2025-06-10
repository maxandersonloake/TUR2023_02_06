//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
    
//data {
//  int<lower=0> N_buildings;
//  real<lower=0> MMI[N_buildings];
//  int<lower=0> damage_flag[N_buildings]; 
//}

//parameters {
//  real mu; 
//  real<lower=0> sigma; 
//}

//model {
//  mu ~ uniform(0.5,4);
//  sigma ~ uniform(0.01,1);
//  damage_flag ~ bernoulli(lognormal_cdf(MMI, mu, sigma));
//}

/*data {
  int<lower=0> N_buildingTypes;
  int<lower=0> N_cities;
  
  //field survey data
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  
  //ministrys data
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PGV_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys]; 
  
}

parameters {
  real mu[N_buildingTypes];
  real<lower=0> sigma[N_buildingTypes];
  real<lower=0> eps_city_error;
  real eps_city_errors[N_cities];
  
  simplex[N_buildingTypes] p_buildingtype[N_cities];
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  real<lower=0, upper=1> damage_probs_ministrys[N_buildings_ministrys];
  for (n in 1:N_buildings)
    damage_probs[n] = lognormal_cdf(PGV[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]], sigma[buildingTypes[n]]);
  for (n in 1:N_buildings_ministrys)
    damage_probs_ministrys[n] = lognormal_cdf(PGV_ministrys[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]], sigma[buildingTypes[n]]);
    //damage_probs[n] = lognormal_cdf(exp(log(PGA[n]) + eps_city_errors[cities[n]]), mu[buildingTypes[n]], sigma[buildingTypes[n]]);
}

model {
  mu ~ normal(-0.5,0.8);
  sigma ~ normal(0.6,0.05); //gamma(5,10);
  for (n in 1:N_cities)
    eps_city_errors[n] ~ normal(0, eps_city_error);
  for (n in 1:N_buildings)
    damage_flag[n] ~ bernoulli(damage_probs[n]);
  for (n in 1:N_buildings_ministrys){
    vector[N_buildingTypes] lps = to_vector(log(p_buildingtype[cities_ministrys[n]]));
    for (k in 1:N_buildingTypes){
      lps[k] += damage_flag_ministrys[n] * log(lognormal_cdf(exp(log(PGV_ministrys[n]) + 
                                                eps_city_errors[cities_ministrys[n]]), mu[k], sigma[k])) +
                (1-damage_flag_ministrys[n]) * log(1- lognormal_cdf(exp(log(PGV_ministrys[n]) + 
                                                eps_city_errors[cities_ministrys[n]]), mu[k], sigma[k]));
    }
    target += log_sum_exp(lps);
  }
}*/

data {
  int<lower=0> N_buildingTypes;
  int<lower=0> N_cities;
  
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PGV_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys]; 
}

parameters {
  real mu[N_buildingTypes];
  real<lower=0.001> sigma[N_buildingTypes];
  real<lower=0.001> eps_city_error;
  real eps_city_errors[N_cities];
  
  simplex[N_buildingTypes] p_buildingtype[N_cities];
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  for (n in 1:N_buildings) {
    real loc = mu[buildingTypes[n]] + eps_city_errors[cities[n]];
    damage_probs[n] = lognormal_cdf(PGV[n], loc, sigma[buildingTypes[n]]);
  }
}

model {
  // Priors
  mu ~ normal(-0.5, 0.8);
  sigma ~ normal(0.6, 0.05);
  //eps_city_error ~ normal(0, 1);
  eps_city_error ~ inv_gamma(2,2);
  eps_city_errors ~ normal(0, eps_city_error);
  
  // Field survey likelihood
  damage_flag ~ bernoulli(damage_probs);

  // Ministry data (mixture model across building types)
  for (n in 1:N_buildings_ministrys) {
    vector[N_buildingTypes] lps = log(p_buildingtype[cities_ministrys[n]]);
    for (k in 1:N_buildingTypes) {
      real loc = mu[k] + eps_city_errors[cities_ministrys[n]];
      real log_cdf = lognormal_lcdf(PGV_ministrys[n] | loc, sigma[k]);
      real log1m_cdf = log1m_exp(log_cdf);
      lps[k] += damage_flag_ministrys[n] * log_cdf +
                (1 - damage_flag_ministrys[n]) * log1m_cdf;
    }
    target += log_sum_exp(lps);
  }
}
