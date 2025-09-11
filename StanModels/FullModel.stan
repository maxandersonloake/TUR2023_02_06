/*functions {
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

  // field survey data
  int<lower=0> N_buildings;
  real<lower=0> PSA[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings];
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  int<lower=0> buildingtype_counts[N_cities, N_buildingTypes];
  real<lower=0> mean_PSA[N_cities];

  // ministry data
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PSA_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys];
  real<lower=0> build_dens[N_buildings_ministrys];

  // priors
  simplex[N_buildingTypes] buildingtypes_priorprob[N_cities, N_zones];
  real prior_frag_structure_type[N_buildingTypes, 4]; // [mu_mean, mu_sd, beta_mean, beta_sd]
  simplex[N_zones] zone_priorprob[N_cities];
  real alpha_omega;
  real alpha_pi;
  real<lower=0> alpha_D;
}

parameters {
  // Fragility
  real mu[N_buildingTypes];
  real<lower=0> beta[N_buildingTypes];

  // Random-effect scales (non-centered)
  real log_sigma_city;
  real log_sigma_obs;
  //real<lower=0> sigma_city;
  //real<lower=0> sigma_obs;
  vector[N_cities] city_z;
  vector[N_buildings_ministrys] obs_z;

  // Taxonomy
  simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones];
  simplex[N_zones] zone_probs[N_cities]; // city-specific zone weights

  // Missingness
  real<lower=0, upper=1> nu_0;
  real<lower=0, upper=1> nu_1;

  // Density->missingness params on unconstrained scales
  real<lower=30, upper=300> kappa_0;
  real<lower=0.5, upper=1> kappa_1;
}

transformed parameters {
  // Derived positive params
  //vector<lower=0>[N_buildingTypes] beta = exp(log_beta);
  //real<lower=0> kappa_0 = exp(log_kappa0);
  //real<lower=0, upper=1> kappa_1 = inv_logit(logit_kappa1);

  real<lower=0> sigma_city = exp(log_sigma_city);
  real<lower=0> sigma_obs = exp(log_sigma_obs);
  
  // Random effects (can be +/-)
  vector[N_cities] city_errors = sigma_city * city_z;
  vector[N_buildings_ministrys] errors_ministrys = sigma_obs * obs_z;

  // Likelihood helpers
  real<lower=0, upper=1> damage_probs[N_buildings];
  //real<lower=0> updated_IM[N_buildings];
  real<lower=0, upper=1> p_ministrys_unobserved[N_buildings_ministrys];
  real<lower=0, upper=1> mean_URM_obs_probs[N_cities];

  // city-level URM observation probability (uses last building-type as URM)
  for (c in 1:N_cities) {
    real mean_dam_prob = lognormal_cdf(mean_PSA[c], mu[N_buildingTypes], beta[N_buildingTypes]);
    mean_URM_obs_probs[c] = nu_0 * (1 - mean_dam_prob) + nu_1 * mean_dam_prob;
  }

  // field survey damage probs with city scatter folded into beta
  for (n in 1:N_buildings) {
    //updated_IM[n] = exp(log(PSA[n]) + city_errors[cities[n]]);
    //damage_probs[n] = lognormal_cdf(updated_IM[n], mu[buildingTypes[n]], beta[buildingTypes[n]]);
    damage_probs[n] = lognormal_cdf(PSA[n], mu[buildingTypes[n]], beta[buildingTypes[n]]);
  }

  // ministry missingness function (clamped)
  for (n in 1:N_buildings_ministrys) {
    real sbd = sqrt(build_dens[n]);
    real inc = kappa_0 * ( sbd / (sbd + kappa_1) );
    real p = 1 - inc; // probability of being unobserved
    p_ministrys_unobserved[n] = fmin(fmax(p, 1e-8), 1 - 1e-8);
  }
}

model {
  // ---- Priors ----
  // Fragility
  for (k in 1:N_buildingTypes) {
    mu[k] ~ normal(prior_frag_structure_type[k,1], prior_frag_structure_type[k,2]);
    beta[k] ~ normal(prior_frag_structure_type[k,3], prior_frag_structure_type[k,4]);
    // lognormal prior for beta via log_beta
    //{
    //  real m  = prior_frag_structure_type[k,3];
    //  real sd = prior_frag_structure_type[k,4];
    //  log_beta[k] ~ normal(log(m), sd / fmax(m, 1e-6));
    //}
  }

  // Scales (half-t-ish)
  //sigma_city ~ student_t(3, 0, 0.01);
  //sigma_obs  ~ student_t(3, 0, 0.02);
  log_sigma_city ~ normal(log(0.1), 0.2);
  log_sigma_obs ~ normal(log(0.2), 0.2);
  city_z ~ normal(0, 1);
  obs_z  ~ normal(0, 1);

  // Missingness params
  nu_0 ~ beta(25, 5);
  nu_1 ~ beta(20, 5);
  //log_kappa0   ~ normal(log(100), 0.5);
  //logit_kappa1 ~ normal(0, 1);

  // Zone mixture per city
  for (c in 1:N_cities)
    zone_probs[c] ~ dirichlet(alpha_omega * zone_priorprob[c]);

  // Building-type simplexes per (city, zone)
  for (c in 1:N_cities)
    for (z in 1:N_zones)
      
      //soft_alpha = fmax(soft_alpha, rep_vector(0.1, N_buildingTypes));
      //buildingTypeProbs[c,z] ~ dirichlet(soft_alpha);
      buildingTypeProbs[c,z] ~ dirichlet(fmax(alpha_pi * buildingtypes_priorprob[c,z], rep_vector(0.1, N_buildingTypes)));

  // ---- Field survey likelihood (Dirichlet–multinomial for zone 1 only) ----
  for (c in 1:N_cities) {
    // adjust last category (URM) by observation probability
    //vector[N_buildingTypes] tmp = to_vector(buildingTypeProbs[c, 1]);
    //tmp[N_buildingTypes] *= mean_URM_obs_probs[c];
    //tmp = fmax(tmp, rep_vector(1e-9, N_buildingTypes));                 // avoid corners
    //vector[N_buildingTypes] buildingTypeProbs_adj = tmp / sum(tmp);
    vector[N_buildingTypes]buildingTypeProbs_adj = to_vector(buildingTypeProbs[c,1]);

    target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha_D * buildingTypeProbs_adj);
  }

  // ---- Per-building survey damage flags ----
  for (n in 1:N_buildings) {
    damage_flag[n] ~ bernoulli(damage_probs[n]);
    //if (buildingTypes[n] != N_buildingTypes) {
    //  damage_flag[n] ~ bernoulli(damage_probs[n]);
    //} //else {
      // URM: inclusion depends on damage status
      //real p = fmin(fmax(damage_probs[n], 1e-9), 1 - 1e-9);
      //if (damage_flag[n] == 1) {
      //  target += bernoulli_lpmf(1 | p) + log(nu_1);
      //} else {
      //  target += bernoulli_lpmf(0 | p) + log(nu_0);
      //}
      //target += -log( p * nu_1 + (1 - p) * nu_0 );
    //}
  }

  // ---- Ministry sample mixture over zones and building types ----
  for (n in 1:N_buildings_ministrys) {
    int cmin = cities_ministrys[n];
    vector[N_zones] log_lik_zone;

    for (z in 1:N_zones) {
      vector[N_buildingTypes] lps = to_vector(log(buildingTypeProbs[cmin, z]));

      for (k in 1:N_buildingTypes) {
        //real IM_updated = exp(log(PSA_ministrys[n]) + city_errors[cmin]); //+ errors_ministrys[n]); 
        //real log_cdf    = lognormal_lcdf(IM_updated | mu[k], beta[k]);
        real log_cdf    = lognormal_lcdf(PSA_ministrys[n] | mu[k], beta[k]);
        real log1m_cdf  = log1m_exp(log_cdf);
        
        if (damage_flag_ministrys[n] == 1) {
          lps[k] += log_cdf;
        } else {
          lps[k] += log1m_cdf;
        }
        
        // if (damage_flag_ministrys[n] == 1) {
        //   lps[k] += log_cdf + log1m(p_ministrys_unobserved[n]);
        // } else {
        //   lps[k] += log_sum_exp( log1m_cdf, log_cdf + log(p_ministrys_unobserved[n]) );
        // }
      }

      log_lik_zone[z] = log( zone_probs[cmin][z] ) + log_sum_exp(lps);
    }

    target += log_sum_exp(log_lik_zone);
  }
}
*/

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

  // field survey data
  int<lower=0> N_buildings;
  real<lower=0> PSA[N_buildings];
  int<lower=0, upper=1> damage_flag[N_buildings];
  int<lower=1, upper=N_cities> cities[N_buildings];
  int<lower=1, upper=N_buildingTypes> buildingTypes[N_buildings];
  int<lower=0> buildingtype_counts[N_cities, N_buildingTypes];
  real<lower=0> mean_PSA[N_cities];

  // ministry data
  int<lower=0> N_buildings_ministrys;
  real<lower=0> PSA_ministrys[N_buildings_ministrys];
  int<lower=1, upper=N_cities> cities_ministrys[N_buildings_ministrys];
  int<lower=0, upper=1> damage_flag_ministrys[N_buildings_ministrys];
  real<lower=0> build_dens[N_buildings_ministrys];

  // priors
  simplex[N_buildingTypes] buildingtypes_priorprob[N_cities, N_zones];
  real prior_frag_structure_type[N_buildingTypes, 4]; // [mu_mean, mu_sd, beta_mean, beta_sd]
  simplex[N_zones] zone_priorprob[N_cities];
  real alpha_omega;
  real alpha_pi;
  real<lower=0> alpha_D;

}

parameters {
  // Fragility
  real mu[N_buildingTypes];
  vector[N_buildingTypes] log_beta;
  
  // Taxonomy
  simplex[N_buildingTypes] buildingTypeProbs[N_cities, N_zones];
  simplex[N_zones] zone_probs[N_cities];     // city-specific zone weights
  
  // Missingness
  real<lower=0.5, upper=1> kappa_0;
  real<lower=10, upper=300> kappa_1;
  real<lower=0, upper=1> nu_0;
  real<lower=0, upper=1> nu_1;
  
  real log_sigma_city;
  vector[N_cities] city_z;
  real log_sigma_obs;
  vector[N_buildings_ministrys] obs_z;
}

transformed parameters {
  vector<lower=0>[N_buildingTypes] beta = exp(log_beta);
  real<lower=0> sigma_city = exp(log_sigma_city);
  real<lower=0> sigma_obs = exp(log_sigma_obs);
}

model {
  
  city_z ~ normal(0,1);
  log_sigma_city ~ normal(log(0.1), 0.2);
  
  obs_z ~ normal(0,1);
  log_sigma_obs  ~ normal(log(0.2),  0.2);
  
  nu_0 ~ beta(20,5);
  //nu_1 ~ beta(5, 25);
  nu_1 ~ beta(5,20);
  
  
  kappa_0 ~ uniform(0.5, 1);
  kappa_1 ~ uniform(10, 300);
  
  {
    vector[N_cities] city_errors = sigma_city * city_z;
    
    vector[N_buildings_ministrys] obs_errors = sigma_obs * obs_z;

    vector[N_cities] mean_URM_obs_probs;
    for (c in 1:N_cities) {
      real mean_dam_prob = lognormal_cdf(mean_PSA[c], mu[N_buildingTypes], beta[N_buildingTypes]);
      mean_URM_obs_probs[c] = nu_0 * (1 - mean_dam_prob) + nu_1 * mean_dam_prob;
    }

    vector[N_buildings_ministrys] p_unobs;
    for (n in 1:N_buildings_ministrys) {
      real sbd = sqrt(build_dens[n]);
      real inc = kappa_0 * (sbd / (sbd + kappa_1));
      real p = 1 - inc;
      p_unobs[n] = fmin(fmax(p, 1e-8), 1 - 1e-8);
    }

    // ---- Priors ----
    for (k in 1:N_buildingTypes) {
      mu[k] ~ normal(prior_frag_structure_type[k,1], prior_frag_structure_type[k,2]);
      //beta[k] ~ normal(prior_frag_structure_type[k,3], prior_frag_structure_type[k,4]);
      {
        real m  = fmax(prior_frag_structure_type[k,3], 1e-6);
        real sd = fmax(prior_frag_structure_type[k,4], 1e-6);
        log_beta[k] ~ normal(log(m), sd);
      }
    }
  
    // Zone mixture per city — make this informative to reduce label switching
    for (c in 1:N_cities){
      //vector[4] smooth_prior = (1 - eps_dirichlet) * to_vector(zone_priorprob[c]) + eps_dirichlet * rep_vector(1.0 / 4, 4);
      zone_probs[c] ~ dirichlet(alpha_omega * zone_priorprob[c]);
    }
  
    // Building-type simplexes per (city, zone) with smooth epsilon mix
    // concentration = alpha_pi * ((1-eps) * prior + eps * uniform)
    for (c in 1:N_cities)
      for (z in 1:N_zones) {
        //vector[N_buildingTypes] smooth_prior =
        //  (1 - eps_dirichlet) * to_vector(buildingtypes_priorprob[c,z])
        //  + eps_dirichlet * rep_vector(1.0 / N_buildingTypes, N_buildingTypes);
        
        // adjust last category (URM) by observation probability
        buildingTypeProbs[c, z]  ~ dirichlet(alpha_pi * buildingtypes_priorprob[c,z]);
        
        vector[N_buildingTypes] tmp = to_vector(buildingTypeProbs[c, z]);
        tmp[N_buildingTypes] *= mean_URM_obs_probs[c];
        tmp = fmax(tmp, rep_vector(1e-9, N_buildingTypes));                 // avoid corners
        vector[N_buildingTypes] buildingTypeProbs_adj = tmp / sum(tmp);
        
        if (z == 1 && c != 1){ # Residential zone, not Narli
          vector[N_buildingTypes] bt = to_vector(buildingTypeProbs_adj);
          bt = fmax(bt, rep_vector(1e-9, N_buildingTypes)); // numerical guard
          vector[N_buildingTypes] bt_adj = bt / sum(bt);
          target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha_D * bt_adj);
        }
        if (z == 4 && c == 1){ # Mixed zone, Narli
          vector[N_buildingTypes] bt = to_vector(buildingTypeProbs_adj);
          bt = fmax(bt, rep_vector(1e-9, N_buildingTypes)); // numerical guard
          vector[N_buildingTypes] bt_adj = bt / sum(bt);
          target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha_D * bt_adj);
        }
        //vector[N_buildingTypes]buildingTypeProbs_adj = to_vector(buildingTypeProbs[c,1]);
      
        //buildingTypeProbs[c,z] ~ dirichlet(alpha_pi * buildingtypes_priorprob[c,z]);
        //buildingTypeProbs[c,z] ~ dirichlet(alpha_pi * buildingTypeProbs_adj);
      }
  
    // ---- Field survey likelihood (Dirichlet–multinomial for zone 1 only) ----
    // for (c in 1:N_cities) {
    //   vector[N_buildingTypes] bt = to_vector(buildingTypeProbs_adj[c, 1]);
    //   bt = fmax(bt, rep_vector(1e-9, N_buildingTypes)); // numerical guard
    //   vector[N_buildingTypes] bt_adj = bt / sum(bt);
    //   target += dirichlet_multinomial_lpmf(buildingtype_counts[c] | alpha_D * bt_adj);
    // }
  
    // ---- Per-building survey damage flags ----
    // for (n in 1:N_buildings) {
    //   real p = lognormal_cdf(PSA[n], mu[buildingTypes[n]], beta[buildingTypes[n]]);
    //   p = fmin(fmax(p, 1e-9), 1 - 1e-9);
    //   damage_flag[n] ~ bernoulli(p);
    // }
    
    for (n in 1:N_buildings) {
      real p = lognormal_cdf(exp(log(PSA[n]) + city_errors[cities[n]]), mu[buildingTypes[n]], beta[buildingTypes[n]]);
      p = fmin(fmax(p, 1e-9), 1 - 1e-9);
      if (buildingTypes[n] != N_buildingTypes) {
        damage_flag[n] ~ bernoulli(p);
      } else {
        // URM: inclusion depends on damage status
        if (damage_flag[n] == 1) {
          target += bernoulli_lpmf(1 | p) + log(nu_1);
        } else {
          target += bernoulli_lpmf(0 | p) + log(nu_0);
        }
        target += -log( p * nu_1 + (1 - p) * nu_0 );
      }
    }
  
    //---- Ministry sample mixture over zones and building types ----
    for (n in 1:N_buildings_ministrys) {
      // City for this record and updated intensity measure (city amplification)
      int cmin = cities_ministrys[n];
      real IM_updated = exp(log(PSA_ministrys[n]) + city_errors[cmin] + obs_errors[n]);
    
      // Likelihood per zone (computed in probability space)
      vector[N_zones] lik_zone;
    
      for (z in 1:N_zones) {
        // Likelihood per building type for this zone
        vector[N_buildingTypes] like_k;
    
        for (k in 1:N_buildingTypes) {
          // Damage probability for this building type at updated IM
          real cdf_k = lognormal_cdf(IM_updated, mu[k], beta[k]);
          // Optional tiny clamp to avoid exact 0/1 due to floating point
          cdf_k = fmin(fmax(cdf_k, 1e-12), 1.0 - 1e-12);
    
          if (damage_flag_ministrys[n] == 1) {
            // Damaged AND observed: P(damage) * P(included | damage)
            like_k[k] = cdf_k * (1.0 - p_unobs[n]);
          } else {
            // Not observed as damaged:
            // either truly undamaged and included, OR damaged but unobserved
            like_k[k] = (1.0 - cdf_k) + cdf_k * p_unobs[n];
          }
        }
    
        // Mix over building types using the zone's composition
        real lz = dot_product(to_vector(buildingTypeProbs[cmin, z]), like_k);
        lik_zone[z] = fmin(fmax(lz, 1e-12), 1.0);  // guard against log(0)
      }
    
      // Mix over zones using the city's zone weights, then take a single log
      real lik = dot_product(to_vector(zone_probs[cmin]), lik_zone);
      target += log(fmin(fmax(lik, 1e-12), 1.0));
    }
  }
}