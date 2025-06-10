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
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
}

parameters {
  real mu;
  real sigma;
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  for (n in 1:N_buildings)
    damage_probs[n] = lognormal_cdf(PGV[n], mu, sigma);
}

model {
  //damage_flag ~ bernoulli_logit(alpha + beta * MMI);
  mu ~ uniform(-3,1);
  sigma ~ normal(0.6,0.05);
  //sigma ~ uniform(0.01,3);
  for (n in 1:N_buildings)
    damage_flag[n] ~ bernoulli(damage_probs[n]);
}

