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
    
data {
  int<lower=0> N_buildingtypes;
  int<lower=0> N_cities;
  int<lower=0> buildingtype_counts[N_cities, N_buildingtypes]; 
  int<lower=0> N_provinces;
  int<lower=0> provinces[N_cities];
  //real<lower=0> province_weight; 
  simplex[N_buildingtypes] p_buildingtype_province[N_provinces];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //simplex[N_buildingtypes] phi; # expected value of p_buildingtype
  //real<lower=0> kappa; #strength of prior phi in determinng 
  
  //simplex[N_buildingtypes] p_buildingtype_baseline;
  //real<lower=0>[N_buildingtypes] sampling_error_std[N_cities];
  simplex[N_buildingtypes] p_buildingtype[N_cities];
  //simplex[N_buildingtypes] p_buildingtype_adj[N_cities];
  real<lower=0> province_weight;
  real<lower=0> obs_prob_free[N_buildingtypes - 1];  // free parameters for obs_prob[2:N]
}

transformed parameters {
  real<lower=0, upper=1> obs_prob[N_buildingtypes];
  simplex[N_buildingtypes] p_buildingtype_adj[N_cities];
  
  obs_prob[1] = 1;  // fix first element
  for (k in 2:N_buildingtypes) {
    obs_prob[k] = obs_prob_free[k - 1];
  }

  for (n in 1:N_cities) {
    vector[N_buildingtypes] temp = to_vector(obs_prob) .* to_vector(p_buildingtype[n]) ;
    p_buildingtype_adj[n] = temp / sum(temp);
  }
}

//transformed parameters {
//  vector[N_buildingtypes] alpha = kappa * phi;
//}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.

model {
  //phi ~ dirichlet(rep_vector(1, N_buildingtypes));
  //kappa ~ normal(300,50);
  //p_buildingtype ~ dirichlet(alpha);
  province_weight ~ uniform(0, 1000);
  obs_prob ~ normal(1, 0.2);
  
  //p_buildingtype_baseline ~ dirichlet(rep_vector(1, N_buildingtypes));
  
  //phi ~ dirichlet(rep_vector(0.05, N_buildingtypes));
  //kappa ~ normal(2000,100);
  //p_buildingtype_province ~ dirichlet(alpha);

  for (n in 1:N_cities){
    p_buildingtype[n] ~ dirichlet(p_buildingtype_province[provinces[n]] * province_weight);
    //p_buildingtype_adj[n] = p_buildingtype[n] * obs_prob / sum(p_buildingtype[n] * obs_prob);
    //p_buildingtype[n] ~ dirichlet(rep_vector(1, N_buildingtypes));
    buildingtype_counts[n,] ~ multinomial(p_buildingtype_adj[n]);
  }

  //for (n in 1:N_buildings) {
  //  buildingtypes[n] ~ bernoulli(p_buildingtype[buildingtypes[n]]);
  //}
  //y ~ normal(mu, sigma);
}
