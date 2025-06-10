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
  int<lower=0> N_buildings;
  int<lower=0> N_buildingTypes;
  real<lower=0> PGA[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
}

parameters {
  real<lower=-2, upper=2> mu[N_buildingTypes];
  real<lower=0.01, upper=3> sigma[N_buildingTypes];
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  for (n in 1:N_buildings)
    damage_probs[n] = lognormal_cdf(PGA[n], mu[buildingTypes[n]], sigma[buildingTypes[n]]);
}

model {
  //damage_flag ~ bernoulli_logit(alpha + beta * MMI);
  mu ~ uniform(-2,2);
  sigma ~ uniform(0.01,3);
  for (n in 1:N_buildings)
    damage_flag[n] ~ bernoulli(damage_probs[n]);
}
