
dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

#----- Define Model Dimensions -----
N_cities <- 10
N_buildingTypes <- 11   # includes URM as last category
N_zones <- 4

N_buildings <- 527       # field (engineering) survey
N_buildings_ministrys <- 100  # ministry (MEUCC) sample

#----- Hyperparameter settings -----
alpha_omega <- 10      # concentration for zone prior
alpha_pi <- 10         # concentration for building type prior
alpha_D <- 5         # Dirichlet-multinomial concentration for counts

################################
# Read in priors
################################

Priors = readRDS(paste0(dir, 'Data/Priors'))
prior_frag_structure_type = Priors$prior_frag_structure_type
buildingtypes_priorprob = Priors$buildingtypes_priorprob
zone_priorprob = Priors$zone_priorprob 

################################
# Define the 'true' parameters
################################

# fragility: mu (meanlog) and beta (sdlog)
true_mu <- rnorm(N_buildingTypes, mean = prior_frag_structure_type[,1], sd = prior_frag_structure_type[,2])
true_beta <- rlnorm(N_buildingTypes, meanlog = log(prior_frag_structure_type[,3]), sdlog = prior_frag_structure_type[,4]) # positive

# taxonomy: actual zone weights per city (zone_probs) and building type probs per city/zone
true_zone_probs <- matrix(NA, nrow = N_cities, ncol = N_zones)
true_buildingTypeProbs <- array(NA, dim = c(N_cities, N_zones, N_buildingTypes))
for(c in 1:N_cities){
  true_zone_probs[c, ] <- as.vector(rdirichlet(1, as.numeric(alpha_omega * zone_priorprob[c, ])))
  for(z in 1:N_zones){
    true_buildingTypeProbs[c, z, ] <- as.vector(rdirichlet(1, alpha_pi * buildingtypes_priorprob[c, z, ]))
  }
}

# missingness / selection parameters (used in ministry and URM handling)
true_kappa_0 <- 0.8
true_kappa_1 <- 50
true_nu_0 <- 0.7    # inclusion prob for URM if undamaged 
true_nu_1 <- 0.3    # inclusion prob for URM if damaged 

# city and observation noise
true_sigma_city <- 0.12
true_city_errors <- rnorm(N_cities, 0, true_sigma_city)
true_sigma_obs <- 0.18
true_obs_errors <- rnorm(N_buildings_ministrys, 0, true_sigma_obs)

################################
# Simulate data
################################

#-- Building type counts:
# For each city produce a buildingtype_counts vector by drawing a multinomial of size n_counts[c]
buildingtype_counts <- matrix(0L, nrow = N_cities, ncol = N_buildingTypes)

for(c in 1:N_cities){
  # Logic check: City 1 (Narli) uses Zone 4, others use Zone 1
  target_zone <- ifelse(c == 1, 4, 1)
  
  # Base probabilities for the target zone
  bt_probs_base <- true_buildingTypeProbs[c, target_zone, ]
  
  # Adjust URM observation probability (matching Stan's mean_URM_obs_probs)
  mean_dam_prob_URM <- plnorm(mean_PSA[c], meanlog = true_mu[N_buildingTypes], sdlog = true_beta[N_buildingTypes])
  mean_URM_obs_prob <- true_nu_0 * (1 - mean_dam_prob_URM) + true_nu_1 * mean_dam_prob_URM
  
  # Apply adjustment to the last category (URM)
  bt_probs_adj <- bt_probs_base
  bt_probs_adj[N_buildingTypes] <- bt_probs_adj[N_buildingTypes] * mean_URM_obs_prob
  
  # Normalize and sample
  bt_probs_adj <- pmax(bt_probs_adj, 1e-9)
  bt_probs_adj <- bt_probs_adj / sum(bt_probs_adj)
  
  total_count_c <- sample(80:150, 1)
  buildingtype_counts[c, ] <- as.integer(rmultinom(1, size = total_count_c, prob = bt_probs_adj)[,1])
}

# -- Building damage:

cities <- sample(1:N_cities, N_buildings, replace = TRUE)
buildingTypes <- integer(N_buildings)
PSA <- rlnorm(N_buildings, meanlog = log(0.25), sdlog = 0.8)
damage_flag <- integer(N_buildings)

for(n in 1:N_buildings){
  keep <- FALSE
  while(!keep){
    cty <- cities[n]
    k <- sample(1:N_buildingTypes, 1, prob = true_buildingTypeProbs[cty, 1, ])
    IM_eff <- PSA[n] * exp(true_city_errors[cty])
    p <- plnorm(IM_eff, meanlog = true_mu[k], sdlog = true_beta[k])
    dmg <- rbinom(1, 1, p)
    
    if(k == N_buildingTypes){
      # URM Selection Bias
      inc_prob <- ifelse(dmg == 1, true_nu_1, true_nu_0)
      if(runif(1) < inc_prob){ 
        buildingTypes[n] <- k
        damage_flag[n] <- dmg
        keep <- TRUE
      }
    } else {
      buildingTypes[n] <- k
      damage_flag[n] <- dmg
      keep <- TRUE
    }
  }
}

# compute mean_PSA per city (as required by data block)
mean_PSA <- numeric(N_cities)
for(c in 1:N_cities){
  idx <- which(cities == c)
  mean_PSA[c] <- if(length(idx)>0) mean(PSA[idx]) else 0.2
}

#----- Simulate ministry (MEUCC) sample -----
PSA_ministrys <- rlnorm(N_buildings_ministrys, meanlog = log(0.25), sdlog = 0.8)
cities_ministrys <- sample(1:N_cities, N_buildings_ministrys, replace = TRUE)

build_dens <- runif(N_buildings_ministrys, 1, 4000) # building density (some positive measure)

damage_flag_ministrys <- integer(N_buildings_ministrys)

for(n in 1:N_buildings_ministrys){
  cmin <- cities_ministrys[n]
  # sample zone by city zone probs
  z <- sample(1:N_zones, 1, prob = true_zone_probs[cmin, ])
  # sample building type conditional on zone
  k <- sample(1:N_buildingTypes, 1, prob = true_buildingTypeProbs[cmin, z, ])
  # compute IM_updated = exp(log(PSA_min) + city_error + obs_error[n])
  IM_updated <- PSA_ministrys[n] * exp(true_city_errors[cmin] + true_obs_errors[n])
  cdf_k <- plnorm(IM_updated, meanlog = true_mu[k], sdlog = true_beta[k])
  cdf_k <- pmax(pmin(cdf_k, 1-1e-12), 1e-12)
  # p_unobs per model: p_unobs[n] = fmin(fmax(1 - kappa_0*(sbd/(sbd+kappa_1)), 1e-8), 1-1e-8)
  sbd <- sqrt(build_dens[n])
  inc <- true_kappa_0 * (sbd / (sbd + true_kappa_1))
  p_unobs <- 1 - inc
  p_unobs <- pmax(pmin(p_unobs, 1-1e-8), 1e-8)
  # simulate true damage
  damage_true <- rbinom(1, 1, cdf_k)
  if(damage_true == 1){
    # if damaged: observed damaged with prob (1 - p_unobs), otherwise unobserved (reported 0)
    damage_flag_ministrys[n] <- rbinom(1, 1, prob = (1 - p_unobs))
  } else {
    damage_flag_ministrys[n] <- 0
  }
}

#----- package into stan-style data list -----
stan_data <- list(
  N_buildingTypes = N_buildingTypes,
  N_cities = N_cities,
  N_zones = N_zones,
  # field survey data
  N_buildings = N_buildings,
  PSA = PSA,
  damage_flag = damage_flag,
  cities = cities,
  buildingTypes = buildingTypes,
  buildingtype_counts = buildingtype_counts,
  mean_PSA = mean_PSA,
  # ministry data
  N_buildings_ministrys = N_buildings_ministrys,
  PSA_ministrys = PSA_ministrys,
  cities_ministrys = cities_ministrys,
  damage_flag_ministrys = damage_flag_ministrys,
  build_dens = build_dens,
  # priors/inputs
  buildingtypes_priorprob = buildingtypes_priorprob,
  prior_frag_structure_type = prior_frag_structure_type,
  zone_priorprob = zone_priorprob,
  alpha_omega = alpha_omega,
  alpha_pi = alpha_pi,
  alpha_D = alpha_D
)

#----- true params for checking after fit -----
true_params <- list(
  mu = true_mu,
  beta = true_beta,
  zone_probs = true_zone_probs,
  buildingTypeProbs = true_buildingTypeProbs,
  kappa_0 = true_kappa_0,
  kappa_1 = true_kappa_1,
  nu_0 = true_nu_0,
  nu_1 = true_nu_1,
  sigma_city = true_sigma_city,
  city_errors = true_city_errors,
  sigma_obs = true_sigma_obs,
  obs_errors = true_obs_errors
)

stan_model_compiled <- stan_model(paste0(dir, "StanModels/FullModel.stan"))

fit <- sampling(
  stan_model_compiled,  # compiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  pars = c("mu","beta","nu_0","nu_1","kappa_0","kappa_1",
           "sigma_city","sigma_obs","zone_probs","buildingTypeProbs"),
  seed = 123
)

#----- quick summary print -----
cat("Simulated data summary\n")
cat("----------------------\n")
cat("Cities:", N_cities, "\n")
cat("Building types:", N_buildingTypes, "\n")
cat("Zones:", N_zones, "\n")
cat("Field buildings:", N_buildings, "\n")
cat("Ministry records:", N_buildings_ministrys, "\n\n")

cat("Example true mu (first 5):", round(true_mu[1:5], 3), "\n")
cat("Example true beta (first 5):", round(true_beta[1:5], 3), "\n")
cat("True sigma_city:", true_sigma_city, "true sigma_obs:", true_sigma_obs, "\n")
cat("True kappa_0, kappa_1:", true_kappa_0, true_kappa_1, "\n")
cat("True nu_0, nu_1:", true_nu_0, true_nu_1, "\n\n")

# optional: save for Stan (uncomment to save)
# saveRDS(stan_data, file = "simulated_stan_data.rds")
# saveRDS(true_params, file = "simulated_true_params.rds")

#------------------------------------------------
#------- PLOT 1: COMPARE FRAGILITY PARAMETERS----
#------------------------------------------------

posterior_fragility <- fit %>%
  gather_draws(mu[k], beta[k]) %>%
  mutate(type = ifelse(.variable == "mu", "Meanlog (mu)", "Sdlog (beta)"))

set.seed(123)
prior_samples <- data.frame()

for(k in 1:N_buildingTypes) {
  # Mu priors
  mu_samps <- rnorm(1000, prior_frag_structure_type[k, 1], prior_frag_structure_type[k, 2])
  # Beta priors (Log-normal logic from the Stan model)
  beta_samps <- rlnorm(1000, log(prior_frag_structure_type[k, 3]), prior_frag_structure_type[k, 4])
  
  prior_samples <- bind_rows(prior_samples, 
                             data.frame(k = k, .value = mu_samps, type = "Meanlog (mu)"),
                             data.frame(k = k, .value = beta_samps, type = "Sdlog (beta)")
  )
}

true_fragility <- data.frame(
  k = 1:N_buildingTypes,
  mu = true_params$mu,
  beta = true_params$beta
) %>%
  pivot_longer(cols = c(mu, beta), names_to = "param", values_to = "true_value") %>%
  mutate(type = ifelse(param == "mu", "Meanlog (mu)", "Sdlog (beta)"))


structure_type_ordered = c(
  'RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
  'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
  'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
  'Reinforced masonry', 'Unreinforced masonry'
)

# 1. Update data with descriptive factors
posterior_fragility$structure <- factor(structure_type_ordered[posterior_fragility$k], levels = structure_type_ordered)
prior_samples$structure <- factor(structure_type_ordered[prior_samples$k], levels = structure_type_ordered)
true_fragility$structure <- factor(structure_type_ordered[true_fragility$k], levels = structure_type_ordered)

# 2. Plotting
ggplot() +
  # Layer 1: Prior (Nudged Left)
  stat_pointinterval(data = prior_samples, aes(x = structure, y = .value), 
                     .width = c(0.66, .95), color = "#1FA187", 
                     position = position_nudge(x = -0.05)) +
  
  # Layer 2: Posterior (Nudged Right)
  stat_pointinterval(data = posterior_fragility, aes(x = structure, y = .value),
                     .width = c(.66, .95), color = '#440154', 
                     position = position_nudge(x = 0.05)) +
  
  # Layer 3: True Values (Centered)
  geom_point(data = true_fragility, aes(x = structure, y = true_value), 
             color = "red", size = 3, shape = 18) +
  
  facet_wrap(~type, scales = "free_y", ncol = 1) +
  
  # Formatting
  labs(title = "Fragility Parameter Recovery vs. Priors",
       subtitle = "Green = Prior | Purple = Posterior | Red Diamond = Ground Truth",
       x = "Structure Type", y = "Value") +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman"), # Set font
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate labels for readability
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )


###############################################
# Plot prior vs posterior fragility
###############################################

# 0. Setup and Parameters
psa_grid <- seq(0, 1.7, 0.02)
n_curve_samples <- 1000 # Number of draws to represent uncertainty

structure_type_ordered = c(
  'RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
  'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
  'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
  'Reinforced masonry', 'Unreinforced masonry'
)

# 1. Generate Prior Curves
# ---------------------------------------------------------
prior_curves <- map_dfr(1:N_buildingTypes, function(i) {
  # Sample mu and beta from the prior distributions
  mu_priors <- rnorm(n_curve_samples, prior_frag_structure_type[i, 1], prior_frag_structure_type[i, 2])
  beta_priors <- rlnorm(n_curve_samples, log(prior_frag_structure_type[i, 3]), prior_frag_structure_type[i, 4])
  
  map_dfr(1:n_curve_samples, function(j) {
    tibble(
      structure_type = structure_type_ordered[i],
      PSA = psa_grid,
      prob = plnorm(psa_grid, mu_priors[j], beta_priors[j]),
      type = "Prior"
    )
  })
})

# 2. Generate Posterior Curves
# ---------------------------------------------------------
post_draws <- fit %>% spread_draws(mu[k], beta[k], n = n_curve_samples)

posterior_curves <- post_draws %>%
  group_by(k, .draw) %>%
  do(tibble(
    structure_type = structure_type_ordered[.$k],
    PSA = psa_grid,
    prob = plnorm(psa_grid, .$mu, .$beta),
    type = "Posterior"
  )) %>%
  ungroup()

# 3. Prepare Synthetic Survey Points (from field data)
# ---------------------------------------------------------
survey_points <- tibble(
  structure_type = factor(structure_type_ordered[stan_data$buildingTypes], levels = structure_type_ordered),
  PSA = stan_data$PSA,
  damage = stan_data$damage_flag
) %>%
  mutate(rounded_PSA = round(PSA * 10) / 10) %>% # Binning for visibility
  group_by(structure_type, rounded_PSA) %>%
  summarise(
    prop_dam = mean(damage), 
    Build = n(), 
    .groups = "drop"
  ) %>%
  rename(PSA = rounded_PSA)

# 4. Summarize Curves (Ribbons and Medians)
# ---------------------------------------------------------
curve_summary <- bind_rows(prior_curves, posterior_curves) %>%
  mutate(
    type = factor(type, levels = c("Prior", "Posterior")),
    structure_type = factor(structure_type, levels = structure_type_ordered)
  ) %>%
  group_by(structure_type, type, PSA) %>%
  summarise(
    lower = quantile(prob, 0.025),
    median = median(prob),
    upper = quantile(prob, 0.975),
    .groups = "drop"
  )

# 5. Final Plot
# ---------------------------------------------------------
fill_colors <- c("Prior" = "#1FA187", "Posterior" = "#440154")

true_curves <- map_dfr(1:N_buildingTypes, function(i) {
  tibble(
    structure_type = factor(structure_type_ordered[i], levels = structure_type_ordered),
    PSA = psa_grid,
    # Using the true parameters from your simulation
    prob = plnorm(psa_grid, true_params$mu[i], true_params$beta[i]),
    type = "Ground Truth"
  )
})

ggplot() +
  # Layer 1: Prior Ribbon (Green)
  geom_ribbon(data = curve_summary %>% filter(type == "Prior"), 
              aes(x = PSA, ymin = lower, ymax = upper, fill = type), 
              alpha = 0.2, linewidth = 0.1) +
  
  # Layer 2: Posterior Ribbon (Purple)
  geom_ribbon(data = curve_summary %>% filter(type == "Posterior"), 
              aes(x = PSA, ymin = lower, ymax = upper, fill = type), 
              alpha = 0.4, linewidth = 0.1) +
  
  # Layer 3: Median Lines (Dashed)
  geom_line(data = curve_summary, aes(x = PSA, y = median, color = type), 
            linewidth = 0.7, linetype = "dashed") +
  
  # Layer 4: THE TRUE CURVE (Solid Red)
  geom_line(data = true_curves, aes(x = PSA, y = prob), 
            color = "#FF0000", linewidth = 1.1, alpha = 0.9) +
  
  facet_wrap(~ structure_type, scales = "free_x") +
  
  # Styling and Legends
  scale_fill_manual(values = fill_colors, name = "Uncertainty (95% CI)") +
  scale_color_manual(values = fill_colors, guide = "none") +
  scale_size_continuous(range = c(0.8, 5), name = "Binned Sample Size") +
  
  labs(
    x = "PSA [T=0.3s] (g)", 
    y = "Probability of Damage",
    caption = "Solid Red Line = Ground Truth | Dashed Lines = Medians"
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  )



################################


# 1. Extract Posterior for City 3, Zone 1
# ---------------------------------------------------------
# Note: Stan indices are 1-based. City 3, Zone 1.
posterior_samples <- as_draws_df(fit)

post_city3_zone1 <- lapply(1:N_buildingTypes, function(k) {
  col_name <- paste0("buildingTypeProbs[3,1,", k, "]")
  tibble(
    structure_type = structure_type_ordered[k],
    value = posterior_samples[[col_name]],
    source = "Posterior (City 3, Zone 1)"
  )
}) %>% bind_rows()

# 2. Extract Prior for City 3, Zone 1
# ---------------------------------------------------------
# Using the prior concentration alpha_pi and the prior simplex
prior_dist_city3 <- rdirichlet(4000, stan_data$buildingtypes_priorprob[3, 1, ] * stan_data$alpha_pi)
colnames(prior_dist_city3) <- structure_type_ordered

prior_df_city3 <- as_tibble(prior_dist_city3) %>%
  pivot_longer(cols = everything(), names_to = "structure_type", values_to = "value") %>%
  mutate(source = "Prior (City 3, Zone 1)")

# 3. True Values for City 3, Zone 1 (Ground Truth)
# ---------------------------------------------------------
true_vals_city3 <- tibble(
  structure_type = structure_type_ordered,
  value = true_params$buildingTypeProbs[3, 1, ],
  source = "Ground Truth"
)

# 4. Combine and Plot
# ---------------------------------------------------------
plot_df_sim <- bind_rows(prior_df_city3, post_city3_zone1) %>%
  mutate(structure_type = factor(structure_type, levels = structure_type_ordered))

fill_colors_sim <- c(
  "Prior (City 3, Zone 1)" = "#1FA187", 
  "Posterior (City 3, Zone 1)" = "#440154"
)

plot_list_sim <- lapply(structure_type_ordered, function(st) {
  ggplot() +
    # Histograms for Prior and Posterior
    geom_histogram(data = filter(plot_df_sim, structure_type == st), 
                   aes(x = value, fill = source, y = ..density..), 
                   position = "identity", alpha = 0.5, bins = 40) +
    # Ground Truth Vertical Line
    geom_vline(data = filter(true_vals_city1, structure_type == st), 
               aes(xintercept = value), color = "red", linetype = "dashed", linewidth = 0.8) +
    labs(title = st, x = NULL, y = NULL) +
    scale_fill_manual(values = fill_colors_sim) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.title = element_text(size = 8, face = "bold"),
      axis.text = element_text(size = 7),
      legend.position = "none"
    )
})

# Combine with patchwork
combined_taxonomy_plot <- wrap_plots(plot_list_sim, ncol = 4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Building Type Proportions: City 3 (Kahramanmaras), Zone 1",
    subtitle = "Red dashed line indicates the true simulated proportion",
    theme = theme(text = element_text(family = "Times New Roman"))
  ) #& 
  #theme(legend.position = "bottom")

combined_taxonomy_plot
