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
  real<lower=0> province_weight; 
  simplex[N_buildingtypes] p_buildingtype_province[N_provinces];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //simplex[N_buildingtypes] phi; # expected value of p_buildingtype
  //real<lower=0> kappa; #strength of prior phi in determinng 
  
  //simplex[N_buildingtypes] p_buildingtype_baseline;
  simplex[N_buildingtypes] p_buildingtype[N_cities];
  //real<lower=0> province_weight;
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
  //province_weight ~ uniform(0, 1000);
  //p_buildingtype_baseline ~ dirichlet(rep_vector(1, N_buildingtypes));
  
  //phi ~ dirichlet(rep_vector(0.05, N_buildingtypes));
  //kappa ~ normal(2000,100);
  //p_buildingtype_province ~ dirichlet(alpha);

  for (n in 1:N_cities){
    p_buildingtype[n] ~ dirichlet(p_buildingtype_province[provinces[n]] * province_weight);
    //p_buildingtype[n] ~ dirichlet(rep_vector(1, N_buildingtypes));
    buildingtype_counts[n,] ~ multinomial(p_buildingtype[n]);
  }

  //for (n in 1:N_buildings) {
  //  buildingtypes[n] ~ bernoulli(p_buildingtype[buildingtypes[n]]);
  //}
  //y ~ normal(mu, sigma);
}
