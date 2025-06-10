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
  real<lower=0, upper=1> lambda;
  real<lower=0> concentration_zone;
  
  //real<lower=0, upper=1> p_include_undam_URM;
  //real<lower=0, upper=1> p_include_dam_URM;
  //real<lower=300, upper=2000> p_ministrys_unobserved_param;
}

parameters {
  real mu[N_buildingTypes];
  real<lower=1e-3> sigma[N_buildingTypes];
  real<lower=1e-3> eps_city_error;
  simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones]; //vector[N_buildingTypes] raw_logits[N_cities, N_zones];
  
  //vector[N_buildingTypes - 1] raw_buildingTypeProbs[N_cities, N_zones];
  
  //real<lower=1e-3> eps_build_error;
  
  real eps_city_errors[N_cities];
  
  real PGV_sigma_ministrys;
  real PGV_errors_ministrys[N_buildings_ministrys];
  
  //real eps_build_errors[N_buildings];
  //real eps_build_errors_ministrys[N_buildings_ministrys];
  
  //real<lower=0, upper=1> p_ministrys_unobserved;
  real<lower=0.04, upper=0.1> p_ministrys_unobserved_param;
  //simplex[N_buildingTypes] p_buildingtype[N_cities];
  
  //real<lower=0.1> obs_prob_free[N_cities, N_buildingTypes-1]; 
  //real<lower=0.1> obs_prob_global[N_buildingTypes-1];
  
  //real<lower=0, upper = 1> frag_adj_URM;
  
  real<lower=0.05, upper=1> p_include_undam_URM;
  real<lower=0.05, upper=1> p_include_dam_URM;
  
  simplex[N_buildingTypes] zone_means[N_zones];        
  simplex[N_buildingTypes] city_means[N_cities];     
  //simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones];  
  
  //real<lower=0.1> zone_obs_probs[N_zones, N_buildingTypes];
  simplex[N_zones] zone_probs;
}

transformed parameters {
  real<lower=0, upper=1> damage_probs[N_buildings];
  //simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones];
      
  //for (c in 1:N_cities)
  //  for (z in 1:N_zones)
  //    buildingTypeProbs[c, z] = softmax(raw_logits[c,z]);
  
  //real<lower=0, upper=1> damage_probs_ministrys[N_buildings_ministrys];
  //real<lower=0.1> obs_prob[N_cities, N_buildingTypes];
  //simplex[N_buildingTypes] p_buildingtype_adj[N_cities];
  //real<lower=0, upper=1> p_dam_URM[N_cities];
  real<lower=0, upper=1> p_ministrys_unobserved[N_buildings_ministrys];
  
  real<lower=0> updated_PGV[N_buildings];
  real<lower=0> updated_PGV_ministrys[N_buildings_ministrys];
  
  for (n in 1:N_buildings){
    updated_PGV[n] = exp(eps_city_errors[cities[n]] + log(PGV[n]));
  }
  
  for (n in 1:N_buildings_ministrys){
    updated_PGV_ministrys[n] = exp(PGV_errors_ministrys[n] + eps_city_errors[cities_ministrys[n]] + log(PGV_ministrys[n]));
  }
  
  for (n in 1:N_buildings_ministrys){
    p_ministrys_unobserved[n] = fmin(p_ministrys_unobserved_param * sqrt(build_dens[n]+ 1e-8), 1); //1- exp(-p_ministrys_unobserved_param * dist_nearest_dam[n]);
  }
  
  for (n in 1:N_buildings)
    damage_probs[n] = lognormal_cdf(updated_PGV[n], mu[buildingTypes[n]], sigma[buildingTypes[n]]);
    //damage_probs[n] = lognormal_cdf(PGV[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]] + eps_build_errors[n], sigma[buildingTypes[n]]);
  //for (n in 1:N_buildings_ministrys)
  //  damage_probs_ministrys[n] = lognormal_cdf(PGV_ministrys[n], mu[buildingTypes[n]] + eps_city_errors[cities[n]], sigma[buildingTypes[n]]);
    //damage_probs[n] = lognormal_cdf(exp(log(PGA[n]) + eps_city_errors[cities[n]]), mu[buildingTypes[n]], sigma[buildingTypes[n]]);
  
  //for (n in 1:N_cities) {
  //  obs_prob[n,1] = 1;
  //  for (k in 1:(N_buildingTypes - 1)) {
  //    obs_prob[n, k+1] = obs_prob_free[n, k];
  //  }
    //obs_prob[n, N_buildingTypes] = 0.1;
    
    // Fixed observation probability for URM (last type) per city
    //p_dam_URM[n] = lognormal_cdf(mean_PGV[n], mu[N_buildingTypes] + eps_city_errors[n], sigma[N_buildingTypes]);
    //obs_prob[n, N_buildingTypes] = obs_prob[n, N_buildingTypes] * (p_include_dam_URM * p_dam_URM[n] + p_include_undam_URM * (1-p_dam_URM[n])); #p_include_dam_URM[n] * p_dam_URM[n] + p_include_undam_URM[n] * (1-p_dam_URM[n]);
    
  //  vector[N_buildingTypes] temp = to_vector(obs_prob[n]) .* to_vector(zone_obs_probs[1,]) .* to_vector(p_buildingtype[n]);
  //  p_buildingtype_adj[n] = temp / sum(temp);
  //}
}

model {
  //concentration_zone ~ gamma(50, 1);
  PGV_errors_ministrys ~ normal(0, PGV_sigma_ministrys);
  
  //for (n in 1:N_cities){
  //   for (k in 1:(N_buildingTypes - 1)){
  //     obs_prob_free[n, k] ~ normal(obs_prob_global[k], 0.1);
  //     obs_prob_global[k] ~ normal(1, 0.1);
  //   }
  //   obs_prob_free[n, N_buildingTypes-1] ~ normal(obs_prob_global[N_buildingTypes-1], 0.1);
  //   obs_prob_global[N_buildingTypes-1] ~ normal(1, 0.1);
     //obs_prob[n, N_buildingTypes] ~ normal(0.2, 0.1);
  //}
  
  //for (n in 1:N_zones){
  //  for (k in 1:N_buildingTypes){
  //    zone_obs_probs[n,k] ~ normal(1.0, 0.3);
  //  }
  //}
  
  eps_city_error ~ inv_gamma(3,2);
  PGV_sigma_ministrys ~ inv_gamma(10,1);
  
  p_include_dam_URM ~ normal(0.5, 0.1);
  p_include_undam_URM ~ normal(0.5, 0.1);
  
  //eps_build_error ~ inv_gamma(3,4);
  
  eps_city_errors ~ normal(0, eps_city_error);
  //eps_build_errors ~ normal(0, eps_build_error);
  //eps_build_errors_ministrys ~ normal(0, eps_build_error);
  
  //mu ~ normal(-0.5,0.8);
  //sigma ~ normal(0.6,0.05); //gamma(5,10);
  for (n in 1:N_buildingTypes){
    mu[n] ~ normal(prior_frag_structure_type[n,1], prior_frag_structure_type[n,2]);
    sigma[n] ~ normal(prior_frag_structure_type[n,3], prior_frag_structure_type[n,4]);
  }
  
  for (z in 1:N_zones){
    zone_means[z] ~ dirichlet(buildingtypes_priorprob);
  }
  for (c in 1:N_cities){
    city_means[c] ~ dirichlet(buildingtypes_priorprob); 
  }
  
  /*for (c in 1:N_cities){
    //p_buildingtype_adj[n] = p_buildingtype[n] * obs_prob / sum(p_buildingtype[n] * obs_prob);
    for (z in 1:N_zones){
      buildingTypeProbs[c,z] ~ dirichlet(concentration_zone * (lambda * zone_means[z] + (1 - lambda) * city_means[c]));
      buildingtype_counts[c,] ~ multinomial(buildingTypeProbs[c,z]);
    }
    //p_buildingtype[n] ~ dirichlet(p_buildingtype_province[provinces[n]] * province_weight);

  }*/
  // for (c in 1:N_cities){
  //   for (z in 1:N_zones) {
  //     vector[N_buildingTypes] alpha = concentration_zone * (
  //       lambda * zone_means[z] + (1 - lambda) * city_means[c]
  //     );
  //     raw_logits[c,z] ~ normal(log(alpha), 0.5);
  //     if (z == 1){
  //       target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha);
  //     }
  
  for (c in 1:N_cities){
    for (z in 1:N_zones) {
      vector[N_buildingTypes] soft_alpha = concentration_zone * (
  lambda * zone_means[z] + (1 - lambda) * city_means[c]
      );
      
      soft_alpha = fmax(soft_alpha, rep_vector(0.5, N_buildingTypes));
      
      // Prior over simplex
      buildingTypeProbs[c,z] ~ dirichlet(soft_alpha);
      
      // Only zone 1 gets the data likelihood
      if (z == 1) {
        target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | buildingTypeProbs[c,z]); #buildingtype_counts[c] ~ multinomial(buildingTypeProbs[c,z]);
      }
    }
  }
  
  // regularize
  // for (c in 1:N_cities) {
  //   for (z in 1:N_zones) {
  //     vector[N_buildingTypes] alpha = concentration_zone * (
  //       lambda * zone_means[z] + (1 - lambda) * city_means[c]
  //     );
  //     buildingTypeProbs[c,z] ~ dirichlet(alpha);
  // 
  //     // Apply data likelihood only for zone 1
  //     //if (z == 1) {
  //     //  target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha);
  //     //}
  //   }
  // }

  for (n in 1:N_buildings){
    if (buildingTypes[n] != N_buildingTypes) { // Not URM
      damage_flag[n] ~ bernoulli(damage_probs[n]);
    } else { // URM
      //target += bernoulli_lpmf(0 | damage_probs[n]* frag_adj_URM);
      if (damage_flag[n] == 1) {
        // Probability of being damaged * probability of being included
        target += bernoulli_lpmf(1 | damage_probs[n]) + log(p_include_dam_URM);
      } else {
        // Probability of being undamaged * probability of being included
        target += bernoulli_lpmf(0 | damage_probs[n]) + log(p_include_undam_URM);
      }
      target += - log(
           damage_probs[n] * p_include_dam_URM +
          (1 -  damage_probs[n]) * p_include_undam_URM
      );
    }
  }
  
  for (n in 1:N_buildings_ministrys){
    vector[N_zones] log_lik_zone;

    for (z in 1:N_zones){
      vector[N_buildingTypes] lps = to_vector(log(buildingTypeProbs[cities_ministrys[n], z]));

      for (k in 1:N_buildingTypes){
        real loc = mu[k]; //+ eps_city_errors[cities_ministrys[n]];
        real log_cdf = lognormal_lcdf(updated_PGV_ministrys[n] | loc, sigma[k]);
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
      real loc = mu[k] + eps_city_errors[cities_ministrys[n]];
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
