library('rstan')
library('openxlsx')
library('tidyverse')
library('dplyr')
library('magrittr')
library(XML)
library(xml2)
library(sf)
library(raster)
library(terra)
library(posterior)
library(MCMCprecision)
library(gridExtra)


get_PGV_from_xml <- function(xml_loc){
  shake_xml_loc = read_xml(xml_loc)
  grid <- xmlParse(shake_xml_loc)
  
  xml_data <- xmlToList(grid)
  lines <- strsplit(xml_data$grid_data, "\n")[[1]] #strsplit(xml_data[[20]], "\n")[[1]]
  
  intensities <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][3]))
  pgv <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][5]))
  longitude <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][1]))
  latitude <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][2]))
  plot_df <- data.frame(longitude=longitude, latitude=latitude, pgv = pgv)#intensities=intensities, pga=pga)
  
  grid_mmi = plot_df[-1,]
  grid_mmi$longitude = round(grid_mmi$longitude * 60 * 2)/60/2 #correct rounding issues to create an evenly spaced grid
  grid_mmi$latitude = round(grid_mmi$latitude * 60 * 2)/60/2 #correct rounding issues to create an evenly spaced grid
  meanhaz <- rast(x = grid_mmi, type = "xyz", crs = "EPSG:4326")
  names(meanhaz) = 'pga_mean'
  return(meanhaz)
}

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

# Set options for faster compilation
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())  # Use multiple CPU cores


#----------------------------------------------------------------------------------------------
#----------------------------------- Prepare survey data --------------------------------------
#----------------------------------------------------------------------------------------------

field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data_and_shaking.xlsx') #read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data_kj.xlsx')
names(field_data)[which(names(field_data)=='City/Town')] = 'City'

field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

field_data %<>% filter(!is.na(GT))
field_data %<>% filter(!is.na(structure_type))
field_data$structure_type %<>% as.character() %>% trimws()

structure_type_ordered = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                           'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                           'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Column with slab corrugated Asgolan',
                           'Reinforced masonry','Unreinforced masonry')
field_data$structure_type %<>% factor(levels = structure_type_ordered)
buildingTypes  = as.numeric(field_data$structure_type)

field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities = as.numeric(field_data$City)

buildingtype_counts = table(field_data$City, field_data$structure_type)

#----------------------------------------------------------------------------------------------
#-------------------------------- Prepare Ministrys data --------------------------------------
#----------------------------------------------------------------------------------------------

kahramanmaras_shp <- read_sf('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/1Kahramanmaras/Kahramanmaras_GT_BDarea_adminID.shp')
kahramanmaras_df <- kahramanmaras_df <- kahramanmaras_shp %>%
  mutate(centroid = st_centroid(st_make_valid(geometry))) %>%
  mutate(
    longitude = st_coordinates(centroid)[, 1],
    latitude = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry()

xml_loc <- '/home/manderso/Documents/GitHub/TUR2023_02_06/ShakeMapUpd.xml.gz'
meanhaz <- get_PGV_from_xml(xml_loc)

kahramanmaras_df$PGV_mean = pull(meanhaz %>% raster::extract(cbind(kahramanmaras_df$longitude, kahramanmaras_df$latitude)))
kahramanmaras_df$GT[which(is.na(kahramanmaras_df$GT))] = 0
kahramanmaras_df$city = which(levels(field_data$City) == "Kahramanmaras")

ministrys_df = kahramanmaras_df[sample(1:NROW(kahramanmaras_df), 20, replace=F),]


#----------------------------------------------------------------------------------------------
#----------------------------------- Select priors --------------------------------------------
#----------------------------------------------------------------------------------------------

# Prior proportion of different building types in the building stock:
prior_probs_structure_type = c('RC MRF (1-3 Storeys)' = 0.25, 
                'RC MRF (4-7 Storeys)' = 0.15, 
                'RC MRF (8+ Storeys)' = 0.1, 
                'RC Wall (1-3 Storeys)' = 0.05, 
                'RC Wall (4-7 Storeys)' = 0.03, 
                'RC Wall (8+ Storeys)' = 0.03,
                'RC Dual system (4-7 Storeys)' = 0.05,
                'RC Dual system (8+ Storeys)' = 0.03, 
                'Column with slab corrugated Asgolan' = 0.01,
                'Reinforced masonry' = 0.15,
                'Unreinforced masonry' = 0.15)

buildingtypes_priorprob = prior_probs_structure_type[levels(field_data$structure_type)] * 10

prior_samps = rdirichlet(10000, buildingtypes_priorprob)
par(mfrow=c(4,3))
for (i in 1:length(unique(field_data$structure_type))){
  hist(prior_samps[,i], xlab='Proportion of building stock', main=unique(field_data$structure_type)[i], freq=F, ylab='', yaxt='n')
}
par(mfrow=c(1,1))

# Prior fragility curves for different building types
# Model:                P(Slight Damage) = LogNormal(PGV | Mu, Sigma)
# Prior distributions:  Mu ~ Normal(column1, column2), Sigma ~ Normal(column3, column4) 
prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'RC MRF (4-7 Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'RC MRF (8+ Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'RC Wall (1-3 Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'RC Wall (4-7 Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'RC Wall (8+ Storeys)' = c(-0.5,0.5,0.6,0.05),
                               'RC Dual system (4-7 Storeys)' = c(-0.5,0.5,0.6,0.05),
                               'RC Dual system (8+ Storeys)' = c(-0.5,0.5,0.6,0.05), 
                               'Column with slab corrugated Asgolan' = c(-0.5,0.5,0.6,0.05),
                               'Reinforced masonry' = c(-0.8,0.2,0.6,0.05),
                               'Unreinforced masonry' = c(-1.1,0.2,0.6,0.05))

prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]

prior_dist_frag = 'rnorm'
par(mfrow=c(4,3))
for (i in 1:NROW(prior_frag_structure_type)){
  plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type)[i], xlab='PGV', ylab='P(Damage)')
  for (j in 1:50){
    mu = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,1]), as.numeric(prior_frag_structure_type[i,2]))))
    sigma = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,3]), as.numeric(prior_frag_structure_type[i,4]))))
    lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma), col='blue')
  }
}
par(mfrow=c(1,1))

#LN_mu_prior = list(dist='rnorm', params=c(-0.5,0.5))
#LN_sigma_prior = list(dist='rnorm', params=c(0.6,0.05))#list(dist='rgamma', params=c(5,10))
# plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white')
# for (i in 1:50){
#   mu = do.call(get(LN_mu_prior$dist), as.list(c(1,LN_mu_prior$params)))
#   sigma = do.call(get(LN_sigma_prior$dist), as.list(c(1,LN_sigma_prior$params)))
#   lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma))
# }

#----------------------------------------------------------------------------------------------
#----------------------------------- Fit model --- --------------------------------------------
#----------------------------------------------------------------------------------------------

#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/CombinedModel.stan")
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/CombinedModel3.stan")

stan_data <- list(
  N_buildingTypes = length(unique(buildingTypes)),
  N_cities = length(unique(cities)),
  
  # Field Survey Data:
  N_buildings = NROW(field_data),
  PGV = field_data$PGV_mean/100,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  buildingTypes = buildingTypes,
  cities = cities,
  buildingtype_counts = buildingtype_counts,
  
  # Ministrys Data:
  N_buildings_ministrys = NROW(ministrys_df),
  PGV_ministrys = ministrys_df$PGV_mean/100,
  cities_ministrys = ministrys_df$city,
  damage_flag_ministrys = ifelse(ministrys_df$GT > 0, 1, 0),
  
  # Priors:
  prior_frag_structure_type = prior_frag_structure_type,
  buildingtypes_priorprob = buildingtypes_priorprob
)

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000
)

saveRDS(fit, paste0('CombinedModelFit_buildError', as.Date(Sys.time())))

#----------------------------------------------------------------------------------------------
#----------------------------------- MCMC Diagnostics -----------------------------------------
#----------------------------------------------------------------------------------------------

traceplot(fit, pars='p_ministrys_unobserved')
traceplot(fit, pars='frag_adj_survey')

traceplot(fit, pars='eps_build_error', inc_warmup=TRUE)
traceplot(fit, pars='mu', inc_warmup=TRUE)
traceplot(fit, pars='sigma')
plot(fit, pars='eps_city_error')
plot(fit, pars='p_buildingtype_adj')
plot(fit, pars='obs_prob')


#----------------------------------------------------------------------------------------------
#----------------------------------- Posterior Analysis -----------------------------------------
#----------------------------------------------------------------------------------------------
field_data$rounded_PGV = round(field_data$PGV_mean * 2) / 200

#Prior vs Posterior fragility curves
prior_dist_frag = 'rnorm'

posterior_samples <- as_draws_df(fit)

par(mfrow=c(3,4))
for (i in 1:NROW(prior_frag_structure_type)){
  plot(x=0, y=0,xlim=c(0,1.7), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type)[i], xlab='PGV', ylab='P(Damage)')
  for (j in 1:50){
    mu_prior = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,1]), as.numeric(prior_frag_structure_type[i,2]))))
    sigma_prior = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,3]), as.numeric(prior_frag_structure_type[i,4]))))
    lines(seq(0,1.7,0.01), plnorm(seq(0,1.7,0.01), mu_prior, sigma_prior), col='blue')
  }
  mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
  sigma_post <- pull(posterior_samples[, grep(paste0("^sigma\\[", i, "\\]"), names(posterior_samples))])
  
  for (j in 1:50){
    mu_post_sample = sample(mu_post, 1)
    sigma_post_sample = sample(sigma_post, 1)
    lines(seq(0,1.7,0.01), plnorm(seq(0,1.7,0.01), mu_post_sample, sigma_post_sample), col='red')
  }
  survey_dat = field_data %>% filter(structure_type == levels(field_data$structure_type)[i]) %>% group_by(rounded_PGV) %>% summarise(prop_dam = mean(GT >=1), Build = n())
  for (j in 1:NROW(survey_dat)){
    points(survey_dat[j,1], survey_dat[j,2], pch=19, cex=as.numeric(log(survey_dat[j,3]+1)))
  }
}
par(mfrow=c(1,1))


#Prior vs posterior probability of building types (in a city)

structure_types <- levels(field_data$structure_type)
buildingtypes_priorprob <- prior_probs_structure_type[structure_types] * 10

# Sample from Dirichlet prior
prior_samps <- rdirichlet(10000, buildingtypes_priorprob)
colnames(prior_samps) <- structure_types

prior_df <- as_tibble(prior_samps) %>%
  pivot_longer(cols = everything(), names_to = "structure_type", values_to = "value") %>%
  mutate(source = "Prior")

# Extract posterior samples
posterior_list <- lapply(seq_along(structure_types), function(i) {
  col_name <- grep(paste0("^p_buildingtype_adj\\[3,", i, "\\]"), names(posterior_samples), value = TRUE)
  tibble(
    structure_type = structure_types[i],
    value = posterior_samples[[col_name]],
    source = "Posterior"
  )
})

posterior_df <- bind_rows(posterior_list)

# Combine prior and posterior
plot_df <- bind_rows(prior_df, posterior_df)

# Plot with facets
plot_list <- lapply(unique(plot_df$structure_type), function(st) {
  ggplot(filter(plot_df, structure_type == st), aes(x = value, fill = source)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 40) +
    labs(
      x = "Proportion of building stock",
      y = "Density",
      fill = "Source",
      title = st
    ) +
    scale_fill_manual(values = c("Prior" = "skyblue", "Posterior" = "tomato")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = "none"
    )
})

grid.arrange(grobs = plot_list, ncol = 3)

#------------------------------------------------------------------------------------------------------------------------------------

field_data$rounded_PGV = round(field_data$PGV_mean) / 100
plot_df = field_data %>% filter(structure_type == 'Unreinforced masonry') %>% group_by(rounded_PGV, City) %>% summarise(prop_dam = mean(GT >=1), nBuild = n())

ggplot(plot_df, aes(x=rounded_PGV, y=prop_dam, size=nBuild, color=City)) + geom_point() + xlim(0, 1.6)

# x_vals <- seq(0, 1.5, 0.01)
# n_samples <- 50
# 
# # Sample from priors
# prior_df <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
#   map_dfr(1:n_samples, function(j) {
#     mu <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 1]), as.numeric(prior_frag_structure_type[i, 2]))))
#     sigma <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 3]), as.numeric(prior_frag_structure_type[i, 4]))))
#     tibble(
#       x = x_vals,
#       y = plnorm(x_vals, meanlog = mu, sdlog = sigma),
#       group = j,
#       type = "Prior",
#       structure = rownames(prior_frag_structure_type)[i]
#     )
#   })
# })
# 
# # Sample from posterior
# posterior_samples <- as_draws_df(fit)
# 
# posterior_df <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
#   mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
#   sigma_post <- pull(posterior_samples[, grep(paste0("^sigma\\[", i, "\\]"), names(posterior_samples))])
#   
#   map_dfr(1:n_samples, function(j) {
#     mu <- sample(mu_post, 1)
#     sigma <- sample(sigma_post, 1)
#     tibble(
#       x = x_vals,
#       y = plnorm(x_vals, meanlog = mu, sdlog = sigma),
#       group = j,
#       type = "Posterior",
#       structure = rownames(prior_frag_structure_type)[i]
#     )
#   })
# })
# 
# # Combine both
# full_df <- bind_rows(prior_df, posterior_df)
# 
# # Plot
# ggplot(full_df, aes(x = x, y = y, group = interaction(type, group), color = type)) +
#   geom_line(alpha = 0.4) +
#   facet_wrap(~structure) +
#   labs(x = "PGV", y = "P(Damage)", color = "Distribution Type") +
#   scale_color_manual(values = c("Prior" = "blue", "Posterior" = "red")) +
#   theme_minimal() +
#   theme(panel.spacing = unit(4, "lines"))









kahranmaras_df_sample$PGV_mean[1]/100
kahranmaras_df_sample$GT


traceplot(fit, pars='mu')

for (i in 1:11){
  plot_df_name <- paste0("plot_df_structure", i)
  assign(plot_df_name, generate_plot_df(fit, pars=c(paste0('mu[',i,']'),paste0('sigma[',i,']'))))
}
levels(field_data$structure_type)
(field_data %>% filter(structure_type==levels(field_data$structure_type)[1]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[3]))[,c('structure_type', 'PGA_mean', 'GT')]
(field_data %>% filter(structure_type==levels(field_data$structure_type)[4]))[,c('structure_type', 'PGA_mean', 'GT')]

propDamGroup = field_data %>% group_by(PGV_mean, structure_type) %>% summarise(prop_dam = mean(GT >=1),
                                                                               nBuild = n()) %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 'RC MRF (8+ Storeys)', 'Unreinforced masonry'))

kahranmaras_df_sample$PGV_mean_rounded = round(kahranmaras_df_sample$PGV_mean) / 100
propDamMinistrys = kahranmaras_df_sample %>% group_by(PGV_mean_rounded) %>% summarise(prop_dam = mean(GT >=1),
                                                                       nBuild = n())

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
  geom_line(data = plot_df_structure7$cdf_samples, aes(x = x, y = prob, group = interaction(mu.7., sigma.7.), 
                                                       color = "Unreinforced masonry"), alpha = 0.2) +  # Semi-transparent for uncertainty
  geom_line(data = plot_df_structure7$cdf_mean, aes(x = x, y = prob, color = "Unreinforced masonry"), size = 1.5) +
  
  xlab('PGV') + ylab('Probability of Damage') +
  theme_minimal() +  scale_color_manual(name='',values = c('RC MRF (8+ Storeys)'='blue','RC MRF (4-7 Storeys)'='yellow',  'RC MRF (1-3 Storeys)'='green', 'Unreinforced masonry'='red')) + xlim(0, 1.3) + 
  geom_point(data=propDamGroup, aes(x=PGV_mean/100, y=prop_dam, size=nBuild, col=structure_type)) +
  geom_point(data=propDamMinistrys, aes(x=PGV_mean_rounded, y=prop_dam, size=nBuild))
  


posterior_samples <- as_draws_df(fit)

# Subset the columns for building 1
building2_cols <- posterior_samples[, grep("^buildingtype_probs_ministrys\\[6,", names(posterior_samples))]
building2_cols <- posterior_samples[, grep("^p_buildingtype\\[3,", names(posterior_samples))]

posterior_means <- colMeans(building2_cols)
structure_labels <- unique(field_data$structure_type)
names(posterior_means) <- structure_labels

df_plot <- tibble(
  structure_type = factor(names(posterior_means), levels = names(posterior_means)),
  probability = as.numeric(posterior_means)
)

# Plot as a horizontal stacked bar
ggplot(df_plot, aes(x = 1, y = probability, fill = structure_type)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Posterior Probability",
    fill = "Structure Type",
    title = "Posterior Building Type Probabilities for Building 2"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())









# 
PGV_seq = seq(0, 1.5, 0.01)
dam_prob = plnorm(PGV_seq,-1.5,0.6)
collapse_prob = plnorm(PGV_seq, -1.2, 0.6)
eff_prob = array(0, length(collapse_prob))
plot(PGV_seq, dam_prob)
lines(PGV_seq, collapse_prob, col='red')

for (i in 1:length(eff_prob)){
  eff_prob[i] = (10000* dam_prob[i] - 10000* collapse_prob[i])/(10000 - 10000* collapse_prob[i])
}

plot(PGV_seq, dam_prob, type='l')
lines(PGV_seq, collapse_prob, col='red')
lines(PGV_seq, eff_prob, col='blue')

lines(PGV_seq, plnorm(PGV_seq, -1.5+1, 0.6), col='green')


