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

data {
  int<lower=0> N_buildingTypes;
  int<lower=0> N_cities;
  
  //field survey data
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  int<lower=0> buildingtype_counts[N_cities, N_buildingTypes]; 
  
  //ministrys data
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PGV_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys]; 
  
  //prior dists
  vector<lower=0>[N_buildingTypes] buildingtypes_priorprob;
  
}

parameters {
  real mu[N_buildingTypes];
  real<lower=1e-3> sigma[N_buildingTypes];
  real<lower=1e-3> eps_city_error;
  real eps_city_errors[N_cities];
  
  simplex[N_buildingTypes] p_buildingtype[N_cities];
  
  real<lower=0.1> obs_prob_free[N_buildingTypes - 1]; 
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  //real<lower=0, upper=1> damage_probs_ministrys[N_buildings_ministrys];
  real<lower=0.1> obs_prob[N_buildingTypes];
  simplex[N_buildingTypes] p_buildingtype_adj[N_cities];
  obs_prob[1] = 1;  // fix first element
  
  for (n in 1:N_buildings)
    damage_probs[n] = lognormal_cdf(PGV[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]], sigma[buildingTypes[n]]);
  //for (n in 1:N_buildings_ministrys)
  //  damage_probs_ministrys[n] = lognormal_cdf(PGV_ministrys[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]], sigma[buildingTypes[n]]);
    //damage_probs[n] = lognormal_cdf(exp(log(PGA[n]) + eps_city_errors[cities[n]]), mu[buildingTypes[n]], sigma[buildingTypes[n]]);

  for (k in 2:N_buildingTypes) {
    obs_prob[k] = obs_prob_free[k - 1];
  }

  for (n in 1:N_cities) {
    vector[N_buildingTypes] temp = to_vector(obs_prob) .* to_vector(p_buildingtype[n]) ;
    p_buildingtype_adj[n] = temp / sum(temp);
  }
}

model {
  obs_prob_free ~ normal(1, 0.05);
  mu ~ normal(-0.5,0.8);
  sigma ~ normal(0.6,0.05); //gamma(5,10);
  eps_city_error ~ inv_gamma(3,1);
  eps_city_errors ~ normal(0, eps_city_error);
  for (n in 1:N_cities){
    //p_buildingtype_adj[n] = p_buildingtype[n] * obs_prob / sum(p_buildingtype[n] * obs_prob);
    p_buildingtype[n] ~ dirichlet(buildingtypes_priorprob);
    //p_buildingtype[n] ~ dirichlet(p_buildingtype_province[provinces[n]] * province_weight);
    buildingtype_counts[n,] ~ multinomial(p_buildingtype_adj[n]);
  }

  for (n in 1:N_buildings)
    damage_flag[n] ~ bernoulli(damage_probs[n]);
  for (n in 1:N_buildings_ministrys){
    vector[N_buildingTypes] lps = to_vector(log(p_buildingtype[cities_ministrys[n]]));
    for (k in 1:N_buildingTypes){
      /*lps[k] += damage_flag_ministrys[n] * log(lognormal_cdf(exp(log(PGV_ministrys[n]) + 
                                                eps_city_errors[cities_ministrys[n]]), mu[k], sigma[k])) +
                (1-damage_flag_ministrys[n]) * log(1- lognormal_cdf(exp(log(PGV_ministrys[n]) + 
                                                eps_city_errors[cities_ministrys[n]]), mu[k], sigma[k]));*/
      real loc = mu[k] + eps_city_errors[cities_ministrys[n]];
      real log_cdf = lognormal_lcdf(PGV_ministrys[n] | loc, sigma[k]);
      real log1m_cdf = log1m_exp(log_cdf);
      lps[k] += damage_flag_ministrys[n] * log_cdf +
                (1 - damage_flag_ministrys[n]) * log1m_cdf;
    }
    target += log_sum_exp(lps);
  }
}

generated quantities {
  matrix[N_buildings_ministrys, N_buildingTypes] buildingtype_probs_ministrys;

  for (n in 1:N_buildings_ministrys) {
    vector[N_buildingTypes] log_ps;

    for (k in 1:N_buildingTypes) {
      real loc = mu[k] + eps_city_errors[cities_ministrys[n]];
      real log_cdf = lognormal_lcdf(PGV_ministrys[n] | loc, sigma[k]);
      real log1m_cdf = log1m_exp(log_cdf);
      
      log_ps[k] = log(p_buildingtype[cities_ministrys[n]][k]) + 
                  damage_flag_ministrys[n] * log_cdf + 
                  (1 - damage_flag_ministrys[n]) * log1m_cdf;
    }

    vector[N_buildingTypes] probs = softmax(log_ps);
    for (k in 1:N_buildingTypes) {
      buildingtype_probs_ministrys[n, k] = probs[k];
    }
  }
}
