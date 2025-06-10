
functions {
  real dirichlet_multinomial_lpmf(int[] y, vector alpha) {
    real sum_alpha = sum(alpha);
    return lgamma(sum_alpha) - lgamma(sum(y) + sum_alpha)
           + sum(lgamma(to_vector(y) + alpha)) - sum(lgamma(alpha));
  }
}

data {
  int<lower=0> N_buildingTypes;
  int<lower=0> N_cities;
  int<lower=0> N_zones;
  
  //field survey data
  int<lower=0> N_buildings;
  real<lower=0> PGV[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings]; 
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  int<lower=0> buildingtype_counts[N_cities, N_buildingTypes]; 
  real<lower=0> mean_PGV[N_cities];
  
  //ministrys data
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PGV_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys]; 
  real<lower=0> build_dens[N_buildings_ministrys];
  
  //prior dists
  vector<lower=0>[N_buildingTypes] buildingtypes_priorprob;
  real prior_frag_structure_type[N_buildingTypes,4];
}

parameters {
  //fragility Model:
  real mu[N_buildingTypes];
  real<lower=1e-3> beta[N_buildingTypes];
  real<lower=1e-3> tau_city_errors;
  real<lower=0> city_errors[N_cities];
  real tau_ministrys;
  real<lower=0> errors_ministrys[N_buildings_ministrys];
  
  //taxonomy Model:
  simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones]; 
  simplex[N_buildingTypes] zone_means[N_zones];        
  simplex[N_buildingTypes] city_means[N_cities];     
  simplex[N_zones] zone_probs;
  real<lower=0> dirmult_dispersion;
  
  //missingness Model:
  real<lower=0.05, upper=1> nu_0;
  real<lower=0.05, upper=1> nu_1;
  real<lower=50, upper=250> nu_2;
  real<lower=0.5, upper=1> nu_3;
  real<lower=0.5, upper=0.8> lambda;
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  real<lower=0> updated_beta[N_buildings];
  real<lower=0, upper=1> p_ministrys_unobserved[N_buildings_ministrys];
  
  for (n in 1:N_buildings){
    updated_beta[n] = sqrt(city_errors[cities[n]]^2 + beta[buildingTypes[n]]^2);
    damage_probs[n] = lognormal_cdf(PGV[n], mu[buildingTypes[n]], updated_beta[n]);
  }
  
  for (n in 1:N_buildings_ministrys){
    //p_ministrys_unobserved[n] =  fmin(fmax(alph1 * (1-exp(-alph2 * build_dens[n]/100)), 1e-8), 1 - 1e-8);
    p_ministrys_unobserved[n] =  fmin(fmax(nu_3 * (sqrt(build_dens[n])/(sqrt(build_dens[n])+nu_2)), 1e-8), 1 - 1e-8);
  }
  
}

model {
  
  //priors:
  tau_city_errors ~ inv_gamma(9,1);
  tau_ministrys ~ inv_gamma(5,1);
 
  nu_0 ~ beta(4, 2);
  nu_1 ~ beta(4, 2);
  
  //model:
  city_errors ~ normal(0, tau_city_errors);
  errors_ministrys ~ normal(0, tau_ministrys);
  dirmult_dispersion ~ inv_gamma(6, 50);
  
  for (n in 1:N_buildingTypes){
    mu[n] ~ normal(prior_frag_structure_type[n,1], prior_frag_structure_type[n,2]);
    beta[n] ~ normal(prior_frag_structure_type[n,3], prior_frag_structure_type[n,4]);
  }
  
  for (z in 1:N_zones){
    zone_means[z] ~ dirichlet(10 * buildingtypes_priorprob);
  }
  for (c in 1:N_cities){
    city_means[c] ~ dirichlet(10 * buildingtypes_priorprob); 
  }
  
  for (c in 1:N_cities){
    for (z in 1:N_zones) {
      vector[N_buildingTypes] soft_alpha = dirmult_dispersion * (
              lambda * zone_means[z] + (1 - lambda) * city_means[c]
      );
      
      soft_alpha = fmax(soft_alpha, rep_vector(0.1, N_buildingTypes));
      
      // Prior over simplex
      buildingTypeProbs[c,z] ~ dirichlet(soft_alpha);
      
      // Only zone 1 gets the data likelihood
      if (z == 1) {
        target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | buildingTypeProbs[c,z]); #buildingtype_counts[c] ~ multinomial(buildingTypeProbs[c,z]);
      }
    }
  }

  for (n in 1:N_buildings){
    if (buildingTypes[n] != N_buildingTypes) { // Not URM
      damage_flag[n] ~ bernoulli(damage_probs[n]);
    } else {
      if (damage_flag[n] == 1) {
        // Probability of being damaged * probability of being included
        target += bernoulli_lpmf(1 | damage_probs[n]) + log(nu_1);
      } else {
        // Probability of being undamaged * probability of being included
        target += bernoulli_lpmf(0 | damage_probs[n]) + log(nu_0);
      }
      target += - log(
           damage_probs[n] * nu_1 +
          (1 -  damage_probs[n]) * nu_0
      );
    }
  }
  
  for (n in 1:N_buildings_ministrys){
    vector[N_zones] log_lik_zone;

    for (z in 1:N_zones){
      vector[N_buildingTypes] lps = to_vector(log(buildingTypeProbs[cities_ministrys[n], z]));

      for (k in 1:N_buildingTypes){
        real loc = mu[k]; 
        real log_cdf = lognormal_lcdf(PGV_ministrys[n] | loc, sqrt(beta[k]^2 + city_errors[cities_ministrys[n]]^2 + errors_ministrys[n]^2));
        real log1m_cdf = log1m_exp(log_cdf);

        if (damage_flag_ministrys[n] == 1) {
          lps[k] += log_cdf + log1m(p_ministrys_unobserved[n]);
        } else {
          lps[k] += log_sum_exp(
            log1m_cdf,
            log_cdf + log(p_ministrys_unobserved[n])
          );
        }
      }

      log_lik_zone[z] = log(zone_probs[z]) + log_sum_exp(lps);
    }

    target += log_sum_exp(log_lik_zone);
  }
}

/*generated quantities {
  matrix[N_buildings_ministrys, N_buildingTypes] buildingtype_probs_ministrys;

  for (n in 1:N_buildings_ministrys) {
    vector[N_buildingTypes] log_ps;

    for (k in 1:N_buildingTypes) {
      real loc = mu[k] + city_errors[cities_ministrys[n]];
      real log_cdf = lognormal_lcdf(updated_PGV_ministrys[n] | loc, sigma[k]);
      real log1m_cdf = log1m_exp(log_cdf);
      
      log_ps[k] = log(buildingTypeProbs[cities_ministrys[n], 1][k]) + 
                  damage_flag_ministrys[n] * log_cdf + 
                  (1 - damage_flag_ministrys[n]) * log1m_cdf;
    }

    vector[N_buildingTypes] probs = softmax(log_ps);
    for (k in 1:N_buildingTypes) {
      buildingtype_probs_ministrys[n, k] = probs[k];
    }
  }
}*/
