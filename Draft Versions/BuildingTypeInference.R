library('rstan')
library('openxlsx')
library('tidyverse')
library('dplyr')
library('magrittr')

# Set options for faster compilation
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())  # Use multiple CPU cores

# Specify the Stan model file
stan_model_file <- "StanBuildingTypes.stan"

field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data_and_shaking.xlsx') #read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data_kj.xlsx')
names(field_data)[which(names(field_data)=='City/Town')] = 'City'

field_data$structure_type %<>% as.factor()
#field_data_kahramanmaras <- field_data %>% filter(City=='Kahramanmaras')

#buildingtype_counts = as.numeric(table(field_data_kahramanmaras$structure_type))

buildingtype_counts = table(field_data$City, field_data$structure_type)
# Load data for Stan (replace with your actual data)
stan_data <- list(
  N_buildingtypes = ncol(buildingtype_counts),
  N_cities = nrow(buildingtype_counts),
  buildingtype_counts = buildingtype_counts, 
  pooling_weight=5
)

# Compile and fit the model
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanBuildingTypes.stan")

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 4000,
  chains = 4,
  warmup = 2000,
  seed = 1234
)

# Print summary of results
print(fit)

# Plot parameter estimates
plot(fit, pars=apply(expand.grid(1:10, 4:6), 1, function(x) paste0("p_buildingtype[", x[1], ",", x[2], "]")))

traceplot(fit, pars=apply(expand.grid(4:6, 4:6), 1, function(x) paste0("p_buildingtype[", x[1], ",", x[2], "]")))

# Extract samples for further analysis
samples <- extract(fit)

# library(MCMCprecision)
# hist(rdirichlet(1000, rep(1, 12)))

#============================================================
#============================================================
#============ Only look at three structure materials ========
#============================================================
#============================================================

field_data = field_data %>%
  mutate(structure_category = case_when(
    structure_type %in% c("RC MRF (4-7 Storeys)", "RC Wall (4-7 Storeys)", "RC MRF (1-3 Storeys)",
                          "RC MRF (8+ Storeys)", "RC Dual system (8+ Storeys)", "RC Dual system (4-7 Storeys)",
                          "RC Wall (8+ Storeys)", "RC Wall (1-3 Storeys)") ~ "RC",
    structure_type == "Unreinforced masonry" ~ "Masonry",
    structure_type == "Reinforced masonry" ~ "Composite",
    TRUE ~ "Other"
  ))

buildingcat_counts = table(field_data$City, field_data$structure_category)
# Load data for Stan (replace with your actual data)
stan_data <- list(
  N_buildingtypes = ncol(buildingcat_counts),
  N_cities = nrow(buildingcat_counts),
  buildingtype_counts = buildingcat_counts, 
  pooling_weight=5
)

# Compile and fit the model
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanBuildingTypes.stan")

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 4000,
  chains = 4,
  warmup = 2000,
  seed = 1234
)

# Print summary of results
print(fit)

structure_names = colnames(buildingcat_counts)
city_names = rownames(buildingcat_counts)

building_type_probs = data.frame(
  city = character(),
  structure_name = character(),
  obs_prob = numeric()
)
for (i in 1:ncol(buildingcat_counts)){
  for (j in 1:nrow(buildingcat_counts)){
    par = paste0('p_buildingtype[',j,',',i,']')
    chain_means = get_posterior_mean(fit, pars=par)
    building_type_probs %<>% add_row(
      city = city_names[j], 
      structure_name = structure_names[i],
      obs_prob = chain_means[length(chain_means)])
  }
}

building_type_probs <- building_type_probs %>%
  mutate(structure_name = factor(structure_name, levels = c( "Other", "Composite", "Masonry","RC" )))

# Define custom colors
custom_colors <- c("RC" = "blue", "Masonry" = "red", "Composite" = "purple", "Other" = "white")

city_province_mapping <- data.frame(
  city = c("Antakya", "Hassa", "Iskendurun", "Islahiye", "Kahramanmaras", 
           "Kirikhan", "Narli", "Nurdagi", "Pazarcik", "Turkoglu"),
  province = c("Hatay", "Hatay", "Hatay", "Gaziantep", "Kahramanmaras",
               "Hatay", "Kahramanmaras", "Gaziantep", "Kahramanmaras", "Kahramanmaras")
)

# Join with building_type_probs data
building_type_probs <- building_type_probs %>%
  left_join(city_province_mapping, by = "city")

building_type_probs$dat_source = 'Estimated'

building_type_district_probs = data.frame(
  city = '',
  province = rep(c('Gaziantep', 'Hatay', 'Kahramanmaras'), each=3),
  structure_name = rep(c('RC', 'Masonry', 'Composite'),3),
  obs_prob = c(.3376, .4313, .2139, .6606, .2049, .1121, .529, .3143, .1391),
  dat_source = 'District'
)

building_type_probs_combined = rbind(building_type_probs, building_type_district_probs)
# Create stacked bar plot
building_type_probs_combined <- building_type_probs_combined %>%
  mutate(province = factor(province, levels = unique(province)))

library(ggpattern)
# Create grouped bar chart with faceting by province
ggplot(building_type_probs_combined, 
       aes(x = city, y = obs_prob, fill = structure_name, pattern = ifelse(dat_source == "District", "stripe", "none"))) +
  geom_bar_pattern(stat = "identity", 
                   position = "stack",
                   pattern_fill = "black",   # Color of the pattern (stripes)
                   pattern_density = 0.1,    # Density of the pattern
                   pattern_spacing = 0.05) + # Spacing between stripes
  scale_fill_manual(values = custom_colors) +  
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +  # Define pattern mapping
  labs(title = "Building Type Probabilities by City",
       x = "City",
       y = "Observed Probability",
       fill = "Structure Type") +
  theme_minimal() +
  facet_wrap(~province, scales = "free_x", nrow = 1) +  # Cluster cities by province
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for clarity


#============================================================
#=============== Use Ministry's Data as Prior ===============
#============================================================
building_type_district_probs = data.frame(
  city = '',
  province = rep(c('Gaziantep', 'Hatay', 'Kahramanmaras'), each=3),
  structure_name = rep(c('RC', 'Masonry', 'Composite'),3),
  obs_prob = c(.3376, .4313, .2139, .6606, .2049, .1121, .529, .3143, .1391),
  dat_source = 'District'
)

field_data  = field_data %>%
  mutate(structure_category = case_when(
    structure_type %in% c("RC MRF (4-7 Storeys)", "RC Wall (4-7 Storeys)", "RC MRF (1-3 Storeys)",
                          "RC MRF (8+ Storeys)", "RC Dual system (8+ Storeys)", "RC Dual system (4-7 Storeys)",
                          "RC Wall (8+ Storeys)", "RC Wall (1-3 Storeys)") ~ "RC",
    structure_type == "Unreinforced masonry" ~ "Masonry",
    structure_type == "Reinforced masonry" ~ "Composite",
    TRUE ~ "Other"
  ))

field_data = field_data %>%
  mutate(province = case_when(
    City %in% c("Antakya", "Hassa", "Iskendurun", "Kirikhan") ~ "Gaziantep",
    City %in% c("Islahiye", "Nurdagi") ~ "Hatay",
    City %in% c("Kahramanmaras", "Narli", "Pazarcik", "Turkoglu") ~ "Kahramanmaras",
    TRUE ~ "Other"
  ))

province_levels <- c("Gaziantep", "Hatay", "Kahramanmaras")
structure_levels <- c("RC", "Masonry", "Composite", 'Other')

field_data$province = factor(field_data$province, levels=province_levels)
field_data$structure_category = factor(field_data$structure_category, levels=structure_levels)

buildingcat_counts = table(field_data$City, field_data$structure_category)

p_buildingtype_province <- (building_type_district_probs %>%
  dplyr::select(province, structure_name, obs_prob) %>%
  pivot_wider(names_from = province, values_from = obs_prob))
p_buildingtype_province = as.matrix(column_to_rownames(p_buildingtype_province, var = "structure_name"))
p_buildingtype_province %<>% rbind('Other' = as.numeric(1- colSums(p_buildingtype_province)))
p_buildingtype_province <- p_buildingtype_province[match( colnames(table(field_data$City, field_data$structure_category)), colnames(buildingcat_counts)), ]
p_buildingtype_province <- p_buildingtype_province[,match( province_levels, colnames(p_buildingtype_province))]

city_province = unique(field_data[,c('City', 'province')])


city_province <- city_province[match( rownames(table(field_data$City, field_data$structure_category)), city_province$City), ]

# Compile and fit the model
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanBuildingTypes_ProvincePrior.stan")

# Load data for Stan (replace with your actual data)
stan_data <- list(
  N_buildingtypes = ncol(buildingcat_counts),
  N_cities = nrow(buildingcat_counts),
  buildingtype_counts = buildingcat_counts, 
  provinces = as.numeric(factor(city_province$province, levels=province_levels)),# as.numeric(field_data$province), 
  p_buildingtype_province= t(p_buildingtype_province),  # need to double check ordering matches provinces and buildingtype_counts
  province_weight=5,
  N_provinces = 3
)

fit1 <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 4000,
  chains = 4,
  warmup = 2000,
  seed = 1234
)

stan_data$province_weight = 500

fit2 = sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 4000,
  chains = 4,
  warmup = 2000,
  seed = 1234
)

rename_stan_fit <- function(fit_named) {
  # Get current names
  old_names <- names(fit_named)
  for (k in 1:length(old_names)){
    name = old_names[k]
    matches <- regmatches(name, regexec("p_buildingtype\\[([0-9]+),([0-9]+)\\]", name))
    if (length(matches[[1]]) == 3) {
      i <- as.numeric(matches[[1]][2])
      j <- as.numeric(matches[[1]][3])
      new_name = paste0('p[',structure_levels[j],',',city_province$City[i],']')
      names(fit_named)[k] = new_name
    }
  }
  
  return(fit_named)
}

fit1 = rename_stan_fit(fit1)
fit2 = rename_stan_fit(fit2)

library(bayesplot)
combined <- rbind(mcmc_intervals_data(fit1), mcmc_intervals_data(fit2))
combined$model <- rep(c("Weak Prior", "Strong Prior"), each = nrow(combined)/2)

# make the plot using ggplot 
library(ggplot2)
theme_set(bayesplot::theme_default())
pos <- position_nudge(y = ifelse(combined$model == "Model 2", 0, 0.1))
ggplot(combined, aes(x = m, y = parameter, color = model)) + 
  geom_linerange(aes(xmin = l, xmax = h), position = pos, size=2)+
  geom_linerange(aes(xmin = ll, xmax = hh), position = pos)+
  geom_point(position = pos, color="black")

stan_data <- list(
  N_buildingtypes = ncol(buildingcat_counts),
  N_cities = nrow(buildingcat_counts),
  buildingtype_counts = buildingcat_counts, 
  provinces = as.numeric(factor(city_province$province, levels=province_levels)),# as.numeric(field_data$province), 
  p_buildingtype_province= t(p_buildingtype_province),  # need to double check ordering matches provinces and buildingtype_counts
  province_weight=5,
  N_provinces = 3
)
stan_data$province_weight = NULL
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanBuildingTypes_ProvincePriorBetween.stan")
fit3 = sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 8000,
  chains = 4,
  warmup = 6000,
  seed = 1234
)

fit3 = rename_stan_fit(fit3)

names(fit3)[grep("^obs_prob\\[[1-4]\\]$", names(fit3))] = paste0('obs_prob_', structure_levels)
plot(fit3, pars=c(names(fit3)[1:40], 'obs_prob'))


structure_names = colnames(buildingcat_counts)
city_names = rownames(buildingcat_counts)

building_type_probs = data.frame(
  city = character(),
  structure_name = character(),
  obs_prob = numeric()
)
for (i in 1:ncol(buildingcat_counts)){
  for (j in 1:nrow(buildingcat_counts)){
    par = paste0('p_buildingtype[',j,',',i,']')
    chain_means = get_posterior_mean(fit, pars=par)
    building_type_probs %<>% add_row(
      city = city_names[j], 
      structure_name = structure_names[i],
      obs_prob = chain_means[length(chain_means)])
  }
}

building_type_probs <- building_type_probs %>%
  mutate(structure_name = factor(structure_name, levels = c( "Other", "Composite", "Masonry","RC" )))

# Define custom colors
custom_colors <- c("RC" = "blue", "Masonry" = "red", "Composite" = "purple", "Other" = "white")

city_province_mapping <- data.frame(
  city = c("Antakya", "Hassa", "Iskendurun", "Islahiye", "Kahramanmaras", 
           "Kirikhan", "Narli", "Nurdagi", "Pazarcik", "Turkoglu"),
  province = c("Hatay", "Hatay", "Hatay", "Gaziantep", "Kahramanmaras",
               "Hatay", "Kahramanmaras", "Gaziantep", "Kahramanmaras", "Kahramanmaras")
)

# Join with building_type_probs data
building_type_probs <- building_type_probs %>%
  left_join(city_province_mapping, by = "city")

building_type_probs$dat_source = 'Estimated'

building_type_district_probs = data.frame(
  city = '',
  province = rep(c('Gaziantep', 'Hatay', 'Kahramanmaras'), each=3),
  structure_name = rep(c('RC', 'Masonry', 'Composite'),3),
  obs_prob = c(.3376, .4313, .2139, .6606, .2049, .1121, .529, .3143, .1391),
  dat_source = 'District'
)

building_type_probs_combined = rbind(building_type_probs, building_type_district_probs)
# Create stacked bar plot
building_type_probs_combined <- building_type_probs_combined %>%
  mutate(province = factor(province, levels = unique(province)))

library(ggpattern)
# Create grouped bar chart with faceting by province
ggplot(building_type_probs_combined, 
       aes(x = city, y = obs_prob, fill = structure_name, pattern = ifelse(dat_source == "District", "stripe", "none"))) +
  geom_bar_pattern(stat = "identity", 
                   position = "stack",
                   pattern_fill = "black",   # Color of the pattern (stripes)
                   pattern_density = 0.1,    # Density of the pattern
                   pattern_spacing = 0.05) + # Spacing between stripes
  scale_fill_manual(values = custom_colors) +  
  scale_pattern_manual(values = c("none" = "none", "stripe" = "stripe")) +  # Define pattern mapping
  labs(title = "Building Type Probabilities by City",
       x = "City",
       y = "Observed Probability",
       fill = "Structure Type") +
  theme_minimal() +
  facet_wrap(~province, scales = "free_x", nrow = 1) +  # Cluster cities by province
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for clarity

#------------------------------------------------------------------------------
#--------------------------- Fragiliy Curve Inference -------------------------
#------------------------------------------------------------------------------

# source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetUSGS.R')
# shake_xml_loc <- '/home/manderso/Documents/GitHub/TUR2023_02_06/ShakeMapUpd.xml.gz'
# haz <- ExtractUSGS_xml(shake_xml_loc)
# field_data$mmi_mean = pull(haz$mmi_mean %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))
#data_incomplete$mmi_mean = pull(haz$mmi_mean %>% raster::extract(cbind(data_incomplete$longitude, data_incomplete$latitude)))

field_data %<>% filter(!is.na(damage_condition))
field_data$PGV_mean = field_data$PGV_mean / 100

stan_data <- list(
  N_buildings = NROW(field_data),
  PGV = field_data$PGV_mean,
  damage_flag = ifelse(field_data$damage_condition=='None', 0, 1)
)

# Compile and fit the model
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves.stan")

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000,
  seed = 1234
)

stan_model_logit <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_Logit.stan")

fit_logit <- sampling(
  stan_model_logit,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000,
  seed = 1234
)

plot(fit)
traceplot(fit)

generate_plot_df <- function(stan_fit, pars=c('mu', 'sigma'), model_func='plnorm'){
  x_vals <- seq(-1, 2, length.out = 100)
  posterior_samples <- as.data.frame(rstan::extract(stan_fit, pars = pars))
  posterior_samples <- posterior_samples[sample(1:nrow(posterior_samples), 100), ]
  
  pars[1] = gsub("\\[|\\]", ".", pars[1])
  pars[2] = gsub("\\[|\\]", ".", pars[2])
  
  regr_function = get(model_func)
  cdf_samples <- posterior_samples %>%
    expand_grid(x = x_vals) %>%
    mutate(prob = regr_function(x, get(pars[1]), get(pars[2])))
  
  par1_mean <- mean(pull(posterior_samples[pars[1]]))
  par2_mean <- mean(pull(posterior_samples[pars[2]]))
  
  cdf_mean <- data.frame(
    x = x_vals,
    prob = regr_function(x_vals, par1_mean, par2_mean)
  )
  
  return(list(cdf_samples=cdf_samples, cdf_mean=cdf_mean))
} 

generate_plot_df_single_par <- function(stan_fit, sigma, pars='mu', model_func='plnorm'){
  x_vals <- seq(-1, 2, length.out = 100)
  posterior_samples <- as.data.frame(rstan::extract(stan_fit, pars = pars))
  posterior_samples <- posterior_samples[sample(1:nrow(posterior_samples), 100), , drop=F]
  
  pars[1] = gsub("\\[|\\]", ".", pars[1])
  
  regr_function = get(model_func)
  cdf_samples <- posterior_samples %>%
    expand_grid(x = x_vals) %>%
    mutate(prob = regr_function(x, get(pars), sigma))
  
  par1_mean <- mean(pull(posterior_samples[pars[1]]))
  
  cdf_mean <- data.frame(
    x = x_vals,
    prob = regr_function(x_vals, par1_mean, sigma)
  )
  
  return(list(cdf_samples=cdf_samples, cdf_mean=cdf_mean))
} 

logistic_regr <- function(x, alpha, beta){
  return(1 / (1 + exp(-(alpha + beta * x))))
}

plot_df_plnorm = generate_plot_df(fit)
plot_df_logit = generate_plot_df(fit_logit, pars=c('alpha', 'beta'), model_func='logistic_regr')

# Plot the results
ggplot() +
  # Plot 1000 sampled probabilities as thin lines
  geom_line(data = plot_df_plnorm$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "cyan", alpha = 0.1) +  # Semi-transparent for uncertainty
  # Plot the probability curve from mean posterior values as a thick line
  geom_line(data = plot_df_plnorm$cdf_mean, aes(x = x, y = prob), color = "blue", size = 1.5) +
  #geom_line(data = plot_df_logit$cdf_samples, aes(x = x, y = prob, group = interaction(alpha, beta)), 
  #          color = "green", alpha = 0.1) +  # Semi-transparent for uncertainty
  # Plot the probability curve from mean posterior values as a thick line
  #geom_line(data = plot_df_logit$cdf_mean, aes(x = x, y = prob), color = "forestgreen", size = 1.5) +
  labs(x = "Peak Ground Acceleration (PGV)", y = "Probability of Damage", 
       title = "Posterior Damage Probability with Mean Estimate") +
  geom_point(aes(x=field_data$PGV_mean, y=ifelse(field_data$damage_condition=='None', 0, 1))) + 
  theme_minimal() + xlim(0,1.3) + 
  geom_line(data= data.frame(x = seq(0,1.3, 0.01), y=plnorm(seq(0,1.3,0.01), log(0.55), 0.4)), aes(x=x, y=y)) 


field_data_filtered <- field_data %>%
  mutate(y_value = ifelse(damage_condition == "None", 0, 1))

num_zeros <- sum(field_data_filtered$y_value == 0, na.rm=T)
num_ones <- sum(field_data_filtered$y_value == 1, na.rm=T)

ggplot() +
  # Plot 1000 sampled probabilities as thin lines
  geom_line(data = plot_df_plnorm$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "cyan", alpha = 0.1) +
  
  # Mean probability curve
  geom_line(data = plot_df_plnorm$cdf_mean, aes(x = x, y = prob), color = "blue", size = 1.5) +
  
  # Density for y=0 (scaled to total count)
  geom_density(data = field_data_filtered %>% filter(y_value == 0),
               aes(x = PGV_mean, y = ..count.. * num_zeros/(num_zeros+num_ones)^2/4),  # Scale by total count
               col='green', fill = "white", alpha = 0.3) +
  
  # Mirrored density for y=1 (scaled to total count)
  geom_density(data = field_data_filtered %>% filter(y_value == 1),
               aes(x = PGV_mean, y = -..count.. * num_ones/ (num_zeros+num_ones)^2/4 + 1),  # Flip & normalize
               col='red', fill = "white", alpha = 0.3) +
  
  labs(x = "Peak Ground Acceleration (PGA)", y = "Probability of Damage", 
       title = "Posterior Damage Probability with Mean Estimate") +
  
  geom_point(aes(x = field_data$PGV_mean, y = ifelse(field_data$damage_condition == 'None', 0, 1))) + 
  
  theme_minimal() + 
  xlim(0, 1.5) +
  geom_line(data = data.frame(x = seq(0, 1.5, 0.01), y = plnorm(seq(0, 1.5, 0.01), log(0.55), 0.4)),  aes(x = x, y = y), alpha=0.1)
  
# prop_dam = field_data %>% group_by(PGA_mean) %>% summarise(prop_dam = mean(damage_condition != 'None'))
# ggplot() +
#   # Plot 1000 sampled probabilities as thin lines
#   geom_line(data = plot_df_plnorm$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
#             color = "cyan", alpha = 0.1) +
#   
#   # Mean probability curve
#   geom_line(data = plot_df_plnorm$cdf_mean, aes(x = x, y = prob), color = "blue", size = 1.5) +
#   
#   # Density for y=0 (scaled to total count)
#   geom_point(data=prop_dam, aes(x=PGA_mean, y=prop_dam)) + 
#   
#   labs(x = "Peak Ground Acceleration (PGA)", y = "Probability of Damage", 
#        title = "Posterior Damage Probability with Mean Estimate") +
#   
#   #geom_point(aes(x = field_data$PGA_mean, y = ifelse(field_data$damage_condition == 'None', 0, 1))) + 
#   theme_minimal() + xlim(0, 1.3) 
  #geom_line(data = data.frame(x = seq(0, 1.3, 0.01), y = plnorm(seq(0, 1.3, 0.01), log(0.55), 0.4)), 
  #          aes(x = x, y = y))

#----------------------------------------------------------------------------------------
#------------- Compare different Damage Levels ------------------------------------------
#----------------------------------------------------------------------------------------

field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves.stan")
#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_Logit.stan")

for(i in 1:4){
  stan_data <- list(
    N_buildings = NROW(field_data),
    PGV = field_data$PGV_mean,
    #sigma = 0.6,
    damage_flag = ifelse(field_data$GT >= i, 1, 0)
  )
  
  
  fit <- sampling(
    stan_model_compiled,  # Use the precompiled model
    data = stan_data,
    iter = 2000,
    chains = 3,
    warmup = 1000,
    seed = 1234
  )
  
  plot_df_name <- paste0("plot_df_plnorm_Dam", i)
  assign(plot_df_name, generate_plot_df(fit))#, pars=c('alpha', 'beta'), model_func='logistic_regr'))
  #assign(plot_df_name,generate_plot_df_single_par(fit, sigma=0.6))
}

ggplot() +
  geom_line(data = plot_df_plnorm_Dam1$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "yellow", alpha = 0.1) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_plnorm_Dam1$cdf_mean, aes(x = x, y = prob), color = "yellow", size = 1.5) +
  geom_line(data = plot_df_plnorm_Dam2$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "orange", alpha = 0.1) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_plnorm_Dam2$cdf_mean, aes(x = x, y = prob), color = "orange", size = 1.5) +
  geom_line(data = plot_df_plnorm_Dam3$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "red", alpha = 0.1) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_plnorm_Dam3$cdf_mean, aes(x = x, y = prob), color = "red", size = 1.5) +
  geom_line(data = plot_df_plnorm_Dam4$cdf_samples, aes(x = x, y = prob, group = interaction(mu, sigma)), 
            color = "black", alpha = 0.1) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_plnorm_Dam4$cdf_mean, aes(x = x, y = prob), color = "black", size = 1.5) +
  labs(x = "Peak Ground Acceleration (PGA)", y = "Probability of Damage", 
       title = "Posterior Damage Probability with Mean Estimate") +
  theme_minimal() + xlim(0, 1.5)

#----------------------------------------------------------------------------------------
#------------- Compare different building types ------------------------------------------
#----------------------------------------------------------------------------------------

field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_byBuildType.stan")
#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_Logit.stan")

field_data %<>% filter(!is.na(structure_type))
field_data$structure_type %<>% as.character()
field_data$structure_type %<>% factor(levels = unique(field_data$structure_type))
buildingTypes  = as.numeric(field_data$structure_type)

stan_data <- list(
  N_buildings = NROW(field_data),
  PGA = field_data$PGA_mean,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  N_buildingTypes = length(unique(buildingTypes)),
  buildingTypes = buildingTypes
)

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 6000,
  chains = 3,
  warmup = 4000,
  seed = 1234
)
  
plot(fit, pars='mu')
plot(fit, pars='sigma')
  
for (i in 1:11){
  plot_df_name <- paste0("plot_df_structure", i)
  assign(plot_df_name, generate_plot_df(fit, pars=c(paste0('mu[',i,']'),paste0('sigma[',i,']'))))
}
levels(field_data$structure_type)
(field_data %>% filter(structure_type==levels(field_data$structure_type)[1]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[4]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[3]))[,c('structure_type', 'PGA_mean', 'GT')]

ggplot() +
  geom_line(data = plot_df_structure3$cdf_samples, aes(x = x, y = prob, group = interaction(mu.3., sigma.3.), 
            color = "RC MRF (1-3 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure3$cdf_mean, aes(x = x, y = prob, color = "RC MRF (1-3 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure1$cdf_samples, aes(x = x, y = prob, group = interaction(mu.1., sigma.1.), 
            color = "RC MRF (4-7 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure1$cdf_mean, aes(x = x, y = prob, color = "RC MRF (4-7 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure4$cdf_samples, aes(x = x, y = prob, group = interaction(mu.4., sigma.4.), 
            color = "RC MRF (8+ Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure4$cdf_mean, aes(x = x, y = prob, color = "RC MRF (8+ Storeys)"), size = 1.5) +
  xlab('PGA') + ylab('Probability of Damage') +
  theme_minimal() +  scale_color_manual(name='',values = c('RC MRF (8+ Storeys)'='blue','RC MRF (4-7 Storeys)'='yellow',  'RC MRF (1-3 Storeys)'='green')) + xlim(0, 1.3)


#----------------------------------------------------------------------------------------
#------------- Compare different building types with location error ---------------------
#----------------------------------------------------------------------------------------
  
field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_byBuildType_locError.stan")
#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_Logit.stan")

field_data %<>% filter(!is.na(GT))
field_data %<>% filter(!is.na(structure_type))
field_data$structure_type %<>% as.character()
field_data$structure_type %<>% factor(levels = unique(field_data$structure_type))
buildingTypes  = as.numeric(field_data$structure_type)

field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities = as.numeric(field_data$City)

LN_mu_prior = list(dist='rnorm', params=c(-0.5,0.5))
LN_sigma_prior = list(dist='rgamma', params=c(5,10))

plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white')
for (i in 1:50){
  mu = do.call(get(LN_mu_prior$dist), as.list(c(1,LN_mu_prior$params)))
  sigma = do.call(get(LN_sigma_prior$dist), as.list(c(1,LN_sigma_prior$params)))
  lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma))
}

  
stan_data <- list(
  N_buildings = NROW(field_data),
  PGV = field_data$PGV_mean,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  N_buildingTypes = length(unique(buildingTypes)),
  buildingTypes = buildingTypes,
  N_cities = length(unique(cities)),
  cities = cities
)

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 6000,
  chains = 3,
  warmup = 4000,
  seed = 1234
)

plot(fit, pars='mu')
plot(fit, pars='sigma')
plot(fit, pars='eps_city_error')

for (i in 1:11){
  plot_df_name <- paste0("plot_df_structure", i)
  assign(plot_df_name, generate_plot_df(fit, pars=c(paste0('mu[',i,']'),paste0('sigma[',i,']'))))
}
levels(field_data$structure_type)
(field_data %>% filter(structure_type==levels(field_data$structure_type)[1]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[2]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[3]))[,c('structure_type', 'PGA_mean', 'GT')]

propDamGroup = field_data %>% group_by(PGA_mean, structure_type) %>% summarise(prop_dam = mean(GT >=1),
                                                                               nBuild = n()) %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 'RC MRF (8+ Storeys)'))

ggplot() +
  geom_line(data = plot_df_structure3$cdf_samples, aes(x = x, y = prob, group = interaction(mu.3., sigma.3.), 
                                                       color = "RC MRF (1-3 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure3$cdf_mean, aes(x = x, y = prob, color = "RC MRF (1-3 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure1$cdf_samples, aes(x = x, y = prob, group = interaction(mu.1., sigma.1.), 
                                                       color = "RC MRF (4-7 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure1$cdf_mean, aes(x = x, y = prob, color = "RC MRF (4-7 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure4$cdf_samples, aes(x = x, y = prob, group = interaction(mu.4., sigma.4.), 
                                                       color = "RC MRF (8+ Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure4$cdf_mean, aes(x = x, y = prob, color = "RC MRF (8+ Storeys)"), size = 1.5) +
  xlab('PGA') + ylab('Probability of Damage') +
  theme_minimal() +  scale_color_manual(name='',values = c('RC MRF (8+ Storeys)'='blue','RC MRF (4-7 Storeys)'='yellow',  'RC MRF (1-3 Storeys)'='green')) + xlim(0, 1.3) + 
  geom_point(data=propDamGroup, aes(x=PGA_mean, y=prop_dam, size=nBuild, col=structure_type))

#----------------------------------------------------------------------------------------
#------------- Compare only 3 building types with location error ---------------------
#----------------------------------------------------------------------------------------

field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data_and_shaking.xlsx') #read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data_kj.xlsx')
names(field_data)[which(names(field_data)=='City/Town')] = 'City'

field_data$PGV_mean = field_data$PGV_mean /100
field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_byBuildType_locError.stan")
#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/StanFragCurves_Logit.stan")

field_data %<>% filter(!is.na(GT))
field_data %<>% filter(!is.na(structure_type))
field_data %<>% filter(structure_type %in% c('RC MRF (1-3 Storeys)','RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)'))
field_data$structure_type %<>% as.character()
field_data$structure_type %<>% factor(levels = unique(field_data$structure_type))
buildingTypes  = as.numeric(field_data$structure_type)

field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities = as.numeric(field_data$City)

LN_mu_prior = list(dist='rnorm', params=c(-0.5,0.8))
LN_sigma_prior = list(dist='rnorm', params=c(0.6,0.05))#list(dist='rgamma', params=c(5,10))

plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white')
for (i in 1:50){
  mu = do.call(get(LN_mu_prior$dist), as.list(c(1,LN_mu_prior$params)))
  sigma = do.call(get(LN_sigma_prior$dist), as.list(c(1,LN_sigma_prior$params)))
  lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma))
}


stan_data <- list(
  N_buildings = NROW(field_data),
  PGV = field_data$PGV_mean,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  N_buildingTypes = length(unique(buildingTypes)),
  buildingTypes = buildingTypes,
  N_cities = length(unique(cities)),
  cities = cities
)

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 6000,
  chains = 3,
  warmup = 4000,
  seed = 1234
)

plot(fit, pars='mu')
plot(fit, pars='sigma')
plot(fit, pars='eps_city_error')

for (i in 1:3){
  plot_df_name <- paste0("plot_df_structure", i)
  assign(plot_df_name, generate_plot_df(fit, pars=c(paste0('mu[',i,']'),paste0('sigma[',i,']'))))
}
levels(field_data$structure_type)
(field_data %>% filter(structure_type==levels(field_data$structure_type)[1]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[2]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[3]))[,c('structure_type', 'PGA_mean', 'GT')]

propDamGroup = field_data %>% group_by(PGA_mean, structure_type) %>% summarise(prop_dam = mean(GT >=1),
                                                                               nBuild = n()) %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 'RC MRF (8+ Storeys)'))

ggplot() +
  geom_line(data = plot_df_structure2$cdf_samples, aes(x = x, y = prob, group = interaction(mu.2., sigma.2.), 
                                                       color = "RC MRF (1-3 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure2$cdf_mean, aes(x = x, y = prob, color = "RC MRF (1-3 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure1$cdf_samples, aes(x = x, y = prob, group = interaction(mu.1., sigma.1.), 
                                                       color = "RC MRF (4-7 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure1$cdf_mean, aes(x = x, y = prob, color = "RC MRF (4-7 Storeys)"), size = 1.5) +
  geom_line(data = plot_df_structure3$cdf_samples, aes(x = x, y = prob, group = interaction(mu.3., sigma.3.), 
                                                       color = "RC MRF (8+ Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure3$cdf_mean, aes(x = x, y = prob, color = "RC MRF (8+ Storeys)"), size = 1.5) +
  xlab('PGA') + ylab('Probability of Damage') +
  theme_minimal() +  scale_color_manual(name='',values = c('RC MRF (8+ Storeys)'='blue','RC MRF (4-7 Storeys)'='yellow',  'RC MRF (1-3 Storeys)'='green')) + xlim(0, 1.3) + 
  geom_point(data=propDamGroup, aes(x=PGA_mean, y=prop_dam, size=nBuild, col=structure_type))
