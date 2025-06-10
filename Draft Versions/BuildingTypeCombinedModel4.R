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


get_IMT_from_xml <- function(xml_loc, IMT='pgv'){
  #IMT = MMI, pga, pgv, psa03, psa10, or psa30
  shake_xml_loc = read_xml(xml_loc)
  grid <- xmlParse(shake_xml_loc)
  
  xml_data <- xmlToList(grid)
  lines <- strsplit(xml_data$grid_data, "\n")[[1]] #strsplit(xml_data[[20]], "\n")[[1]]
  
  longitude <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][1]))
  latitude <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][2]))
  
  IMTs = c('MMI', 'pga', 'pgv', 'psa03', 'psa10', 'psa30')
  IMT_match = which(IMTs==IMT)
  IMT_vals =  sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][IMT_match+2]))
  
  # MMI <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][3]))
  # pga <-  sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][4]))
  # pgv <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][5]))
  # psa03 <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][6]))
  # psa10 <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][7]))
  # psa30 <- sapply(lines, function(x) as.numeric(strsplit(x, " ")[[1]][8]))
  
  plot_df <- data.frame(longitude=longitude, latitude=latitude, setNames(list(IMT_vals), IMT))#intensities=intensities, pga=pga)
  
  grid = plot_df[-1,]
  grid$longitude = round(grid$longitude * 60 * 2)/60/2 #correct rounding issues to create an evenly spaced grid
  grid$latitude = round(grid$latitude * 60 * 2)/60/2 #correct rounding issues to create an evenly spaced grid
  meanhaz <- rast(x = grid, type = "xyz", crs = "EPSG:4326")
  names(meanhaz) = paste0(IMT, '_mean')
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

field_data = read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data.xlsx')
#field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data_and_shaking.xlsx') #read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data_kj.xlsx')
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

field_data$structure_type[which(field_data$structure_type=='Column with slab corrugated Asgolan')] = 'Other'

structure_type_ordered = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                           'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                           'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
                           'Reinforced masonry','Unreinforced masonry')
field_data$structure_type %<>% factor(levels = structure_type_ordered)
buildingTypes  = as.numeric(field_data$structure_type)

field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities = as.numeric(field_data$City)

buildingtype_counts = table(field_data$City, field_data$structure_type)

dam_props = field_data %>% group_by(`SA(0.3)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
# p1 <- ggplot(field_data, aes(x = `SA(0.3)_mean`, y = as.numeric(GT >= 1), 
#                              col = as.factor(structure_type))) +
#   geom_jitter(width = 0.02, height = 0.02, alpha = 0.3, size = 1) +
#   stat_smooth(method = "glm", method.args = list(family = "binomial"),
#               se = FALSE, fullrange = TRUE) +
#   labs(y = "Probability of Damage", x = "SA(0.3)", 
#        color = "Structure Type") +
#   theme_minimal()
# p1

structure_types <- unique(field_data$structure_type)
set.seed(2)  # for reproducibility
colors <- sample(rainbow(length(structure_types)))

field_data$PGV_mean = exp(field_data$PGV_mean) / 100

dam_props = field_data %>% group_by(PGV_mean, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p1 = ggplot(dam_props, aes(x=exp(PGV_mean), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point() + 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`SA(0.3)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p2 = ggplot(dam_props, aes(x=exp(`SA(0.3)_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`SA(1.0)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p3 = ggplot(dam_props, aes(x=exp(`SA(1.0)_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`SA(3.0)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p4 = ggplot(dam_props, aes(x=exp(`SA(3.0)_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

# p2 <- ggplot(field_data, aes(x = `SA(0.3)_mean`, y = as.numeric(GT >= 1), 
#                              col = as.factor(structure_type))) +
#   geom_jitter(width = 0.02, height = 0.02, alpha = 0.3, size = 1) +
#   stat_smooth(method = "glm", method.args = list(family = binomial(link="probit")),
#               se = FALSE, fullrange = TRUE) +
#   labs(y = "Probability of Damage", x = "SA(0.3)", 
#        color = "Structure Type") +
#   theme_minimal()
# p2



#----------------------------------------------------------------------------------------------
#-------------------------------- Prior data --------------------------------------
#----------------------------------------------------------------------------------------------


#---------------------------
# Fragility curve prior data:
#---------------------------

field_data$PGA_mean_exp = exp(field_data$PGA_mean)
field_data$SA.3_exp = exp(field_data$`SA(0.3)_mean`) 
field_data$SA1_exp = exp(field_data$`SA(1.0)_mean`) 

lm_PGA_PGV = lm(PGV_mean ~ PGA_mean_exp-1, data=field_data)
lm_SA.3_PGV = lm(PGV_mean ~ SA.3_exp-1, data=field_data)
lm_SA1_PGV = lm(PGV_mean ~ SA1_exp-1, data=field_data)

par(mfrow=c(1,3))
plot(field_data$PGA_mean_exp, field_data$PGV_mean, xlab='PGA', ylab='PGV')
lines(seq(0, 1.5, 0.01), predict(lm_PGA_PGV, newdata=data.frame(PGA_mean_exp=seq(0, 1.5, 0.01))))
plot(field_data$SA.3_exp, field_data$PGV_mean, xlab='SA(0.3s)', ylab='PGV')
lines(seq(0, 2, 0.01), predict(lm_SA.3_PGV, newdata=data.frame(SA.3_exp=seq(0, 2, 0.01))))
plot(field_data$SA1_exp, field_data$PGV_mean, xlab='SA(1.0s)', ylab='PGV')
lines(seq(0, 2, 0.01), predict(lm_SA1_PGV, newdata=data.frame(SA1_exp=seq(0, 2, 0.01))))
par(mfrow=c(1,1))

frag_prev <- read.xlsx('/home/manderso/Documents/USGS/For Max/Jaiswal_Turkiye_fragilities.xlsx')


prior_store = data.frame(structure_type=character(),
                         plnorm_mu = numeric(),
                         plnorm_sigma = numeric())

par(mfrow=c(2,3))
for (i in seq(1, 20,4)){
  plot(0, 0, xlab='PGV', ylab=paste0('p damage'), main=frag_prev[i,1], xlim=c(0, 2), ylim=c(0,1))
  cols = c('gold', 'orange', 'red', 'black')
  for (j in 0:3){
    row_interest = i + j
    dam_seq = frag_prev[row_interest,4:NCOL(frag_prev)]
    IMT_vals <- as.numeric(names(dam_seq))
    IMT_name = frag_prev$IMT[row_interest]
    
    # Choose the correct model based on IMT_name
    if (IMT_name == "PGA") {
      PGV_pred <- predict(lm_PGA_PGV, newdata = data.frame(PGA_mean_exp = IMT_vals))
    } else if (IMT_name == "SA(0.3s)") {
      PGV_pred <- predict(lm_SA.3_PGV, newdata = setNames(data.frame(IMT_vals), "SA.3_exp"))
    } else if (IMT_name == "SA(1.0s)") {
      PGV_pred <- predict(lm_SA1_PGV, newdata = setNames(data.frame(IMT_vals), "SA1_exp"))
    } else {
      stop("Unsupported IMT type: ", IMT_name)
    }
    
    probs = as.numeric(dam_seq)
    lines(PGV_pred, probs, xlab='PGV', ylab=paste0('p(',frag_prev[row_interest,3], ' damage)'), main=frag_prev[row_interest,1], xlim=c(3, 5), col=cols[j+1], lwd=2)
    
    if (j != 0) next 
    
    objective_fn <- function(params) {
      meanlog <- params[1]
      sdlog <- params[2]
      fit <- plnorm(PGV_pred, meanlog = meanlog, sdlog = sdlog)
      sum((fit - probs)^2)
    }
    
    result <- optim(c(meanlog = 0.5, sdlog = 0.1), objective_fn, method = "L-BFGS-B", 
                    lower = c(-Inf, 1e-6))
    
    best_params <- result$par
    meanlog_fit <- best_params[1]
    sdlog_fit <- best_params[2]
    lines(PGV_pred, plnorm(PGV_pred, meanlog_fit, sdlog_fit), col = "blue", lwd = 2, lty=2)
    
    prior_store %<>% add_row(structure_type=frag_prev[row_interest,1],
                             plnorm_mu=meanlog_fit,
                             plnorm_sigma=sdlog_fit)
    #legend("bottomright", legend = c("Original", "plnorm Fit"), col = c("blue", "red"), lwd = 2)
    
    
  }
}


#---------------------------
# Building type prior data:
#---------------------------

build_type_by_city <- read.xlsx('/home/manderso/Documents/USGS/For Max/building_type_by_city.xlsx')
build_type_by_city <- build_type_by_city[-which(build_type_by_city$Row.Labels =='Grand Total'),]
build_type_by_city <- build_type_by_city[,-which(names(build_type_by_city) =='Grand.Total')]

add_building_category = function(df){
  df %<>%
    mutate(Building_Type = Building_Type %>%
             str_replace_all("\u00a0", "")) %>%
    mutate(building_category = case_when(
      
      # --- RC MRF (moment-resisting frame) ---
      Building_Type %in% c(
        "RC.frame.with.briquette./.hollow.concrete.block.infill",
        "RC.frame.with.clay.brick.masonry.infill",
        "RC.frame.with.concrete.block.infill"
      ) ~ "RC MRF or RC Dual System",
      
      # --- RC Wall (structural walls as primary lateral system) ---
      Building_Type %in% c(
        "Tunnel.form.system",
        "Unknown.wall.construction",
        "Other.wall.construction",
        "Unknown.construction",
        "Other.frame.construction",
        "Unknown.frame.construction"
      ) ~ "RC Wall",
      
      # --- RC Dual System (frame + wall system) ---
      #Building_Type %in% c(
      # 
      #   ) ~ "RC Dual System",
      
      # --- Unreinforced Masonry ---
      Building_Type %in% c(
        "RC.or.timber.frame.with.adobe.infill",
        "Clay.brick.masonry",
        "Briquette./.hollow.concrete.block.masonry",
        "Stone.masonry",
        "RC.or.timber.frame.with.stone.masonry.infill"
      ) ~ "Unreinforced Masonry",
      
      Building_Type %in% c(
        
      ) ~ "Composite",
      
      Building_Type %in% c(
        "Structural.steel.frame",
        "Prefabricated.structure",
        "Timber.frame",
        "Timber.wall",
        "Adobe"
      ) ~  "Other",
      
      # Fallback
      TRUE ~ "Unclasssified"
    ))
  return(df)
}

plot_stacked_bar <- function(build_type_by_city){
  # df_clean <- build_type_by_city %>%
  #   filter(Row.Labels != "Grand Total") %>%
  #   distinct()
  
  # Pivot longer
  
  df_long <- build_type_by_city %>%
    pivot_longer(-Row.Labels, names_to = "Building_Type", values_to = "Count") %>%
    filter(!is.na(Count))  # Optional: drop NA counts
  
  df_long %<>% add_building_category()
  
  df_long = df_long %>% group_by(Row.Labels, building_category) %>% summarise(Count=sum(Count))
  
  
  df_prop <- df_long %>%
    group_by(Row.Labels) %>%
    mutate(Proportion = Count / sum(Count)) %>%
    ungroup()
  
  p1 <- ggplot(df_prop, aes(x = Row.Labels, y = Proportion, fill = building_category)) +
    geom_bar(stat = "identity") +
    labs(title = "Proportional Building Types by City",
         x = "City", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p2 <- ggplot(df_long, aes(x = Row.Labels, y = Count, fill = building_category)) +
    geom_bar(stat = "identity") +
    labs(title = "Building Type Counts by City (Stacked)",
         x = "City", y = "Number of Buildings") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  grid.arrange(p1,p2, nrow=2)
}

plot_stacked_bar(build_type_by_city)

field_data$City %>% unique()


match_ups = c('Narli' = 'Kahramanmaras',
              'Pazarcik' = 'Kahramanmaras',
              'Kahramanmaras' = 'Kahramanmaras',
              'Turkoglu' = 'Kahramanmaras',
              'Nurdagi'='Nurdagi',
              'Hassa' = 'Hassa',
              'Islahiye' = 'Gaziantep',
              'Antakya' = 'Antakya',
              'Kirikhan' = 'Kirikhan',
              'Iskendurun' = 'Iskendurun')


build_type_overall <- read.xlsx('/home/manderso/Documents/USGS/For Max/building_type_overall.xlsx')
build_type_overall$Structure.type.Labels= gsub(" ", ".", build_type_overall$Structure.type.Labels)
names(build_type_overall)[1] = 'Building_Type'
total_buildings = build_type_overall[nrow(build_type_overall),2]
build_type_overall = build_type_overall[-nrow(build_type_overall),]
build_type_overall %<>% add_building_category()

build_categories_overall = build_type_overall %>% group_by(building_category) %>% summarise(proportion=sum(Sum.of.BUILDINGS.in.Turkey)/total_buildings)

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
meanhaz <- get_IMT_from_xml(xml_loc, 'pgv')
names(meanhaz)= 'PGV_mean'

kahramanmaras_df$PGV_mean = pull(meanhaz %>% raster::extract(cbind(kahramanmaras_df$longitude, kahramanmaras_df$latitude)))
kahramanmaras_df$GT[which(is.na(kahramanmaras_df$GT))] = 0
kahramanmaras_df$city = which(levels(field_data$City) == "Kahramanmaras")

ministrys_df = kahramanmaras_df[sample(1:NROW(kahramanmaras_df), 10, replace=F),]

kahramanmaras_dam_only = kahramanmaras_df%>% filter(GT >= 1)

ministrys_df$dist_nearest_dam = NA
sampled = sample(1:nrow(ministrys_df),  10, replace=F)
chunks_sampled = matrix(sampled, ncol=10)
for (i in 1:nrow(chunks_sampled)){
  print(i)
  dists = st_distance(st_as_sf(ministrys_df[chunks_sampled[i,], c('longitude', 'latitude')], coords=c(1,2)), st_as_sf(kahramanmaras_dam_only[, c('longitude', 'latitude')], coords=c(1,2)), pairwise=T)
  dists[dists==0] = Inf
  ministrys_df$dist_nearest_dam[chunks_sampled[i,]] <- apply(dists, 1, min)
}

plot(ministrys_df$dist_nearest_dam)

plot(kahramanmaras_df$longitude, kahramanmaras_df$latitude, col=ifelse(kahramanmaras_df$GT >=1,'red','black'), cex=0.05,pch=19)

# plot(build_counts, xlim=c(36.75, 37.05), ylim=c(37.5, 37.64))
# points(kahramanmaras_df$longitude, kahramanmaras_df$latitude, col=ifelse(kahramanmaras_df$GT >=1,'red','black'), cex=0.05)

# kahramanmaras_dam_only = kahramanmaras_df%>% filter(GT >= 1)

# kahramanmaras_df$dist_nearest_dam = NA
# sampled = sample(1:nrow(kahramanmaras_df),  60000, replace=F)
# chunks_sampled = matrix(sampled, ncol=1000)
# for (i in 1:nrow(chunks_sampled)){
#   print(i)
#   dists = st_distance(st_as_sf(kahramanmaras_df[chunks_sampled[i,], c('longitude', 'latitude')], coords=c(1,2)), st_as_sf(kahramanmaras_dam_only[, c('longitude', 'latitude')], coords=c(1,2)), pairwise=T)
#   dists[dists==0] = Inf
#   kahramanmaras_df$dist_nearest_dam[chunks_sampled[i,]] <- apply(dists, 1, min)
# }
# 
# num_bins <- 20  # Change this to your desired number of bins
# 
# # Create the binned variable using quantiles
# kahramanmaras_df$binned_dist_nearest_dam <- cut(
#   kahramanmaras_df$dist_nearest_dam,
#   breaks = quantile(kahramanmaras_df$dist_nearest_dam, probs = seq(0, 1, 1/num_bins), na.rm = TRUE),
#   include.lowest = TRUE,
#   labels = FALSE
# )
# 
# # If you want the midpoint or a representative value for each bin instead of categories
# # First compute the bin edges
# bin_edges <- quantile(kahramanmaras_df$dist_nearest_dam, probs = seq(0, 1, 1/num_bins), na.rm = TRUE)
# 
# # Then assign the midpoint of each bin as the rounded value
# kahramanmaras_df$round_dist_nearest_dam <- NA
# for (i in 1:num_bins) {
#   bin_indices <- kahramanmaras_df$binned_dist_nearest_dam == i
#   kahramanmaras_df$round_dist_nearest_dam[bin_indices] <- (bin_edges[i] + bin_edges[i+1]) / 2
# }
# 
# rounded_df = kahramanmaras_df %>% group_by(round_dist_nearest_dam) %>% summarise(prop_dam = mean(GT>=1))
# rounded_df = rounded_df[-nrow(rounded_df),]
# 
# plot(rounded_df$round_dist_nearest_dam, rounded_df$prop_dam, ylab='Probability of damage', xlab='Distance to closest damaged building', ylim=c(0,1), type='l')
# points(rounded_df$round_dist_nearest_dam, rounded_df$prop_dam)
# lines(seq(0, 0.02,0.0001), 1-exp(-2000*seq(0, 0.02,0.0001)), col='red')
# lines(seq(0, 0.02,0.0001), 1-exp(-300*seq(0, 0.02,0.0001)), col='red')
# #p_dam = rounded_df$round_dist_nearest_dam
# p_missed = 1-exp(-1000*rounded_df$round_dist_nearest_dam)
# #p_missed[21] = 0.9999
# lines(rounded_df$round_dist_nearest_dam, rounded_df$prop_dam/(1-p_missed), col='green', type='l')

#kahramanmaras_df$dist_nearest_dam[sampled] = st_connect(kahramanmaras_df$centroid[sampled], kahramanmaras_dam_only$centroid) %>% st_length()


# dists=st_distance(kahramanmaras_dam_only, kahramanmaras_df)
# terra::distance(kahramanmaras_df, kahramanmaras_dam_only)

# ministrys_df_full = readRDS('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point')
# 
# ministrys_df_full %<>% filter(latitude > 35.8991 & latitude < 39.04281 & longitude > 35.20948 & longitude < 39.35023)
# ministrys_dam_only = ministrys_df_full%>% filter(GT >= 1)
# 
# batch_size <- 100  # Adjust based on your system's memory capacity
# 
# # Calculate total number of batches needed
# n_total <- nrow(ministrys_df_full)
# n_batches <- ceiling(n_total / batch_size)
# 
# # Initialize dist_nearest_dam column
# ministrys_df_full$dist_nearest_dam <- NA
# 
# # Process in batches
# for (i in 1:n_batches) {
#   # Print progress
#   print(paste0("Processing batch ", i, " of ", n_batches))
#   
#   # Determine indices for current batch
#   start_idx <- (i-1) * batch_size + 1
#   end_idx <- min(i * batch_size, n_total)
#   current_indices <- start_idx:end_idx
#   
#   # Calculate distances for current batch
#   dists <- st_distance(
#     st_as_sf(ministrys_df_full[current_indices, c('longitude', 'latitude')], coords=c(1,2)), 
#     st_as_sf(ministrys_dam_only[, c('longitude', 'latitude')], coords=c(1,2))
#   )
#   
#   # Replace zeros with Inf (if zeros exist in the distance matrix)
#   dists[dists == 0] <- Inf
#   
#   # Find minimum distance for each point in the batch
#   ministrys_df_full$dist_nearest_dam[current_indices] <- apply(dists, 1, min)
# }
# saveRDS(ministrys_df_full,'/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point_with_distances' )

# bbox = c(min(ministrys_df_full$longitude), max(ministrys_df_full$longitude), min(ministrys_df_full$latitude), max(ministrys_df_full$latitude))
# r = raster(nrows=1000, ncols=1000,xmn=bbox[1], xmx=bbox[2], ymn=bbox[3], ymx=bbox[4])
# build_counts = rasterize(ministrys_df_full[, c('longitude', 'latitude')],r, fun='count')
# build_counts[is.na(build_counts)] = 0
# build_dam_counts = rasterize((ministrys_df_full %>% filter(GT>=1))[, c('longitude', 'latitude')],r, fun='count')
# build_dam_counts[is.na(build_dam_counts)] = 0
# pga_mean = rasterize(vect(ministrys_df_full[, c('longitude', 'latitude', 'pga_mean')], geom=c("longitude", "latitude")),rast(r), field='pga_mean', fun='max')
#pga_mean[is.na(pga_mean)] = 0
# pga_mean_df = as.data.frame(pga_mean, xy=T, na.rm=F)

# prop_dam = build_dam_counts/build_counts
# 
# dat = data.frame(
#   build_counts = values(build_counts),
#   build_dam_counts = values(build_dam_counts),
#   pga_mean =pga_mean_df$max,
#   lat=pga_mean_df$y,
#   lon=pga_mean_df$x
# )
# dat %<>% filter((build_counts > 200) & (pga_mean > -2))
# 
# plot(dat$pga_mean, dat$build_dam_counts/dat$build_counts)
# 
# dat_miss = dat[which(dat$build_dam_counts/dat$build_counts==0 & dat$build_counts > 300),]
# 
# dat2 = dat %>% filter(build_counts > 300)
# plot(dat2$pga_mean, dat2$build_dam_counts/dat2$build_counts)
# 
# xx = dat %>% group_by(build_counts) %>% summarize(propDam = mean(build_dam_counts!=0))
# 
# plot(xx$build_counts, xx$propDam)
# 
# plot(xx$build_counts, xx$propDam)
# 
# ministrys_df_full$buildDens = extract(build_counts,ministrys_df_full[,c('longitude', 'latitude')], method='bilinear')
# 
# #plot(ministrys_df_full$longitude, ministrys_df_full$latitude, col=ministrys_df_full$buildDens)
# filt = ministrys_df_full %>% filter(longitude > 36 & longitude < 37 & latitude > 37 & latitude < 37.5)
# ggplot(filt, aes(x=longitude, y=latitude, color=buildDens)) + geom_point()
# 
# prop_dam_vs_buildDens = ministrys_df_full %>% group_by(buildDens) %>% summarize(propDam = mean(GT>=1), pga_mean=mean(pga_mean))
# 
# plot(prop_dam_vs_buildDens$pga_mean, prop_dam_vs_buildDens$propDam)
# 
# plot(prop_dam_vs_buildDens$buildDens, prop_dam_vs_buildDens$propDam)
# 
# log_fit = glm(propDam ~ pga_mean, data = prop_dam_vs_buildDens, family = "binomial")
# plot(prop_dam_vs_buildDens$buildDens,log_fit$residuals)
# 
# plot(lin_fit$fitted.values, prop_dam_vs_buildDens$propDam)

#----------------------------------------------------------------------------------------------
#----------------------------------- Select priors --------------------------------------------
#----------------------------------------------------------------------------------------------

print(build_categories_overall)

prior_probs_structure_type = c('RC MRF (1-3 Storeys)' = 0.25, #should be higher in older city
                'RC MRF (4-7 Storeys)' = 0.15, #should be higher in new city
                'RC MRF (8+ Storeys)' = 0.1, #should be higher in new city
                'RC Wall (1-3 Storeys)' = 0.005, #should be higher in older city
                'RC Wall (4-7 Storeys)' = 0.005, #should be higher in new city
                'RC Wall (8+ Storeys)' = 0.005,
                'RC Dual system (4-7 Storeys)' = 0.015,
                'RC Dual system (8+ Storeys)' = 0.015, 
                'Other' = 0.05,
                'Reinforced masonry' = 0.03,
                'Unreinforced masonry' = 0.38)


buildingtypes_priorprob = prior_probs_structure_type[levels(field_data$structure_type)] * 10

prior_samps = rdirichlet(10000, buildingtypes_priorprob)
par(mfrow=c(4,3))
for (i in 1:length(levels(field_data$structure_type))){
  hist(prior_samps[,i], xlab='Proportion of building stock', main=levels(field_data$structure_type)[i], freq=F, ylab='', yaxt='n')
}
par(mfrow=c(1,1))


build_categories_overall




# Prior fragility curves for different building types
# Model:                P(Slight Damage) = LogNormal(PGV | Mu, Sigma)
# Prior distributions:  Mu ~ Normal(column1, column2), Sigma ~ Normal(column3, column4) 
prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.25,0.4,0.6,0.05), #c(-0.25,0.25,0.55,0.05), 
                               'RC MRF (4-7 Storeys)' = c(-1.125,0.4,0.625,0.05), #c(-1.25,0.25,0.6,0.05), 
                               'RC MRF (8+ Storeys)' = c(-1.6,0.4,0.6,0.05), 
                               'RC Wall (1-3 Storeys)' = c(0.4,0.5,0.6,0.05), 
                               'RC Wall (4-7 Storeys)' = c(0.2,0.5,0.6,0.05), 
                               'RC Wall (8+ Storeys)' = c(0,0.5,0.6,0.05),
                               'RC Dual system (4-7 Storeys)' = c(-0.5,0.75,0.6,0.05),
                               'RC Dual system (8+ Storeys)' = c(-0.5,0.75,0.6,0.05), 
                               'Other' = c(-0.5,0.75,0.6,0.05),
                               'Reinforced masonry' = c(-1.2,0.2,0.6,0.05),
                               'Unreinforced masonry' = c(-1.5,0.1,0.6,0.05))

prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]


print(prior_store)

prior_dist_frag = 'rnorm'
par(mfrow=c(3,4))
for (i in 1:NROW(prior_frag_structure_type)){
  plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type)[i], xlab='PGV', ylab='P(Damage)')
  for (j in 1:50){
    mu = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,1]), as.numeric(prior_frag_structure_type[i,2]))))
    sigma = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,3]), as.numeric(prior_frag_structure_type[i,4]))))
    lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma), col='skyblue')
  }
  if (i == 1){
    lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.25873007, 0.4868709), col='red') #Reinforced concrete, Infilled frame, High code, Moderate ductility, 1 storey 
    lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), -0.76894697, 0.6728247), col='darkgreen') #Reinforced concrete, Infilled frame, High code, Moderate ductility, 3 storey
  } else if (i == 2){
    lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), -1.50332700, 0.5926148), col='orange') #Reinforced concrete, Infilled frame, High code, Moderate ductility, 6 storey
  } else if (i == 5){
    lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.20096261, 0.6306427), col='blue') #Reinforced concrete, Wall, High code, High ductility, 7 storey RES
    lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.08368013, 0.6599120), col='purple') #Reinforced concrete, Wall, High code, Moderate ductility, 7 storeys RES
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

building_types_survey_include = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)',
                                  'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                                  'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)')

mean_PGV_by_city = field_data %>% group_by(City) %>% summarise(mean_PGV=mean(PGV_mean/100))

#----------------------------------------------------------------------------------------------
#----------------------------------- Fit model --- --------------------------------------------
#----------------------------------------------------------------------------------------------

#stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/CombinedModel.stan")
stan_model_compiled <- stan_model("/home/manderso/Documents/GitHub/TUR2023_02_06/CombinedModel5.stan")

stan_data <- list(
  N_buildingTypes = length(unique(buildingTypes)),
  N_cities = length(unique(cities)),
  
  # Field Survey Data:
  N_buildings = NROW(field_data),
  PGV = field_data$PGV_mean,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  buildingTypes = buildingTypes,
  cities = cities,
  buildingtype_counts = buildingtype_counts,
  mean_PGV = mean_PGV_by_city$mean_PGV[match(levels(field_data$City), mean_PGV_by_city$City)],
  
  # Ministrys Data:
  N_buildings_ministrys = NROW(ministrys_df),
  PGV_ministrys = ministrys_df$PGV_mean/100,
  cities_ministrys = ministrys_df$city,
  damage_flag_ministrys = ifelse(ministrys_df$GT > 0, 1, 0),
  dist_nearest_dam = ministrys_df$dist_nearest_dam,
  
  # Priors:
  prior_frag_structure_type = prior_frag_structure_type,
  buildingtypes_priorprob = buildingtypes_priorprob,
  
  p_include_dam_URM = 0.1, #rep(0.1, length(unique(cities))),
  p_include_undam_URM = 1#rep(1, length(unique(cities)))
  
  #p_ministrys_unobserved_param= 1900
)

control = list(adapt_delta = 0.99, max_treedepth = 15)

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000
)

fit_summary = summary(fit)$summary
which(as.numeric(fit_summary[,colnames(fit_summary)=='Rhat'])> 1.1)

traceplot(fit, pars='PGV_sigma_ministrys')
pairs(fit, pars=c('obs_prob[3,11]', 'p_include_dam_URM'))

saveRDS(fit, paste0('CombinedModelFit_buildError', as.Date(Sys.time())))

#----------------------------------------------------------------------------------------------
#----------------------------------- MCMC Diagnostics -----------------------------------------
#----------------------------------------------------------------------------------------------

traceplot(fit, pars='p_ministrys_unobserved_param')
traceplot(fit, pars='p_include_undam_URM')
traceplot(fit, pars='p_include_dam_URM')

#traceplot(fit, pars='frag_adj_URM')

traceplot(fit, pars='eps_build_error', inc_warmup=TRUE)
traceplot(fit, pars='mu', inc_warmup=TRUE)
traceplot(fit, pars='sigma')
plot(fit, pars='eps_city_error')
plot(fit, pars='p_buildingtype_adj')
plot(fit, pars='obs_prob')


#----------------------------------------------------------------------------------------------
#----------------------------------- Posterior Analysis -----------------------------------------
#----------------------------------------------------------------------------------------------


pgv_grid <- seq(0, 1.7, 0.01)

# Create a long dataframe of prior samples
prior_curves <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
  map_dfr(1:50, function(j) {
    mu_prior <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 1]), as.numeric(prior_frag_structure_type[i, 2]))))
    sigma_prior <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 3]), as.numeric(prior_frag_structure_type[i, 4]))))
    tibble(
      structure_type = rownames(prior_frag_structure_type)[i],
      PGV = pgv_grid,
      prob = plnorm(pgv_grid, mu_prior, sigma_prior),
      type = "Prior",
      curve_id = paste0("prior_", i, "_", j)
    )
  })
})


# Posterior samples
posterior_samples <- as_draws_df(fit)


posterior_curves <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
  mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
  sigma_post <- pull(posterior_samples[, grep(paste0("^sigma\\[", i, "\\]"), names(posterior_samples))])
  
  map_dfr(1:50, function(j) {
    mu_post_sample <- sample(mu_post, 1)
    sigma_post_sample <- sample(sigma_post, 1)
    tibble(
      structure_type = rownames(prior_frag_structure_type)[i],
      PGV = pgv_grid,
      prob = plnorm(pgv_grid, mu_post_sample, sigma_post_sample),
      type = "Posterior",
      curve_id = paste0("post_", i, "_", j)
    )
  })
})

# Survey data
field_data$rounded_PGV <- round(field_data$PGV_mean * 200) / 200
survey_points <- field_data %>%
  group_by(structure_type, rounded_PGV) %>%
  summarise(prop_dam = mean(GT >= 1), Build = n(), .groups = "drop") %>%
  rename(PGV = rounded_PGV)

# Combine curves
all_curves <- bind_rows(prior_curves, posterior_curves)

# Plot
all_curves$type <- factor(all_curves$type, levels = c("Prior", "Posterior"))
all_curves$structure_type <- factor(all_curves$structure_type, levels = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                                                                          'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                                                                          'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)',
                                                                          'Reinforced masonry','Unreinforced masonry','Other'))
ggplot() +
  geom_line(data = filter(all_curves, type == "Prior"),
            aes(x = PGV, y = prob, group = curve_id),
            color = "skyblue", alpha = 0.7) +
  geom_line(data = filter(all_curves, type == "Posterior"),
            aes(x = PGV, y = prob, group = curve_id),
            color = "tomato", alpha = 0.7) +
  geom_point(data = survey_points,
             aes(x = PGV, y = prop_dam, size = Build),
             color = "black", alpha = 0.7) +
  facet_wrap(~ structure_type) +
  theme_minimal() +
  labs(x = "PGV", y = "P(Damage)", title = "Fragility Curves: Prior vs Posterior") +
  scale_size_continuous(range = c(1, 6))


target_structures <- c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", "RC MRF (8+ Storeys)")

posterior_plot_data <- all_curves %>%
  filter(type == "Posterior", structure_type %in% target_structures)

lognorm_curves <- tibble()

print(prior_store)

# Example parameters for 3 lognormal CDFs:
labelled_lognorms <- tibble(
  label = c("Reinforced concrete, Infilled frame, High code, Moderate ductility, 1 storey", 
            "Reinforced concrete, Infilled frame, High code, Moderate ductility, 3 storey", 
            "Reinforced concrete, Infilled frame, High code, Moderate ductility, 6 storey"),
  meanlog = c(0.25873007, -0.76894697, -1.50332700),
  sdlog = c(0.4868709, 0.6728247, 0.5926148),
  line_type = c("solid", "dotdash", "twodash")
)


# Generate CDF data
pgv_grid <- seq(0.01, 1.7, 0.01)

lognorm_curves <- labelled_lognorms %>%
  rowwise() %>%
  mutate(data = list(tibble(
    PGV = pgv_grid,
    prob = plnorm(pgv_grid, meanlog, sdlog)
  ))) %>%
  unnest(data)

ggplot() +
  geom_line(data = posterior_plot_data,
            aes(x = PGV, y = prob, group = curve_id, color = structure_type),
            alpha = 0.4) +
  geom_line(data = lognorm_curves,
            aes(x = PGV, y = prob, linetype = label),
            color = "black", size = 1) +
  scale_color_viridis_d(option = "D") +
  scale_linetype_manual(values = c("solid", "dotdash", "twodash")) +
  labs(x = "PGV", y = "P(Damage)", title = "Posterior Fragility Curves with Reference Lognormal CDFs",
       color = "Posterior Fragility Curves", linetype = "Reference Curve") +
  theme_minimal()


target_structures <- c("RC Wall (1-3 Storeys)", "RC Wall (4-7 Storeys)", "RC Wall (8+ Storeys)")

posterior_plot_data <- all_curves %>%
  filter(type == "Posterior", structure_type %in% target_structures)

lognorm_curves <- tibble()

print(prior_store)

# Example parameters for 3 lognormal CDFs:
labelled_lognorms <- tibble(
  label = c("Reinforced concrete, Wall, High code, High ductility, 7 storey RES", 
            "Reinforced concrete, Wall, High code, Moderate ductility, 7 storeys RES"),
  meanlog = c(0.20096261, 0.08368013),
  sdlog = c(0.6306427, 0.6599120),
  line_type = c("solid", "dotdash")
)


# Generate CDF data
pgv_grid <- seq(0.01, 1.7, 0.01)

lognorm_curves <- labelled_lognorms %>%
  rowwise() %>%
  mutate(data = list(tibble(
    PGV = pgv_grid,
    prob = plnorm(pgv_grid, meanlog, sdlog)
  ))) %>%
  unnest(data)

ggplot() +
  geom_line(data = posterior_plot_data,
            aes(x = PGV, y = prob, group = curve_id, color = structure_type),
            alpha = 0.4) +
  geom_line(data = lognorm_curves,
            aes(x = PGV, y = prob, linetype = label),
            color = "black", size = 1) +
  scale_color_viridis_d(option = "D") +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  labs(x = "PGV", y = "P(Damage)", title = "Posterior Fragility Curves with Reference Lognormal CDFs",
       color = "Posterior Fragility Curves", linetype = "Reference Curve") +
  theme_minimal()



field_data$rounded_PGV = round(field_data$PGV_mean * 200) / 200

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
city_indices <- c("Kahramanmaras" = 3, "Narli" = 1)

posterior_list <- lapply(names(city_indices), function(city_name) {
  city_index <- city_indices[city_name]
  lapply(seq_along(structure_types), function(i) {
    col_name <- grep(paste0("^p_buildingtype\\[", city_index, ",", i, "\\]"), names(posterior_samples), value = TRUE)
    tibble(
      structure_type = structure_types[i],
      value = posterior_samples[[col_name]],
      source = paste("Posterior -", city_name)
    )
  }) %>% bind_rows()
}) %>% bind_rows()

plot_df <- bind_rows(prior_df, posterior_list)


plot_list <- lapply(unique(plot_df$structure_type), function(st) {
  ggplot(filter(plot_df, structure_type == st), aes(x = value, fill = source)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 40) +
    labs(
      x = "Proportion of building stock",
      y = "Density",
      fill = "Source",
      title = st
    ) +
    scale_fill_manual(values = c(
      "Prior" = "skyblue",
      "Posterior - Kahramanmaras" = "tomato",
      "Posterior - Narli" = "darkgreen"
    )) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = "bottom"
    )
})

library(patchwork)
combined_plot <- wrap_plots(plot_list, ncol = 4) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")
combined_plot

#------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------- Posterior predictive checks---------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------


field_data$rounded_PGV = round(field_data$PGV_mean * 2) / 200

#Prior vs Posterior fragility curves
prior_dist_frag = 'rnorm'

posterior_samples <- as_draws_df(fit)

field_data_with_preds = data.frame()
post_samples_i = sample(1:NROW(posterior_samples), 100)
for (j in 1:stan_data$N_cities){
  survey_data_city = field_data %>% filter(City == levels(field_data$City)[j])
  survey_data_city_grouped = survey_data_city %>% group_by(rounded_PGV, structure_type) %>% summarise(prop_dam = mean(GT >=1), Build = n())
  survey_data_city_grouped[, paste0('sampled.', 1:length(post_samples_i))] = 0
  survey_data_city_grouped$min_samp = NA
  survey_data_city_grouped$max_samp = NA
  survey_data_city_grouped$median_samp = NA
  eps_city_error = pull(posterior_samples[paste0('eps_city_error')])
  error_samp = rnorm(length(post_samples_i), 0, eps_city_error[post_samples_i])
  for (i in 1:NROW(survey_data_city_grouped)){
    k = match(survey_data_city_grouped$structure_type[i], levels(field_data$structure_type))
    plnorm_means =  pull(posterior_samples[paste0('mu[',k,']')])[post_samples_i]
    plnorm_stds =  pull(posterior_samples[paste0('sigma[',k,']')])[post_samples_i]
    dam_probs = plnorm(survey_data_city_grouped$rounded_PGV[i], plnorm_means + error_samp, plnorm_stds)
    obs_samples = rbinom(length(dam_probs), survey_data_city_grouped$Build[i], prob=dam_probs)
    survey_data_city_grouped[i, grep('sampled.',names(survey_data_city_grouped))] = t(obs_samples)
    survey_data_city_grouped$min_samp[i] = quantile(obs_samples, 0.05)/survey_data_city_grouped$Build[i]
    survey_data_city_grouped$max_samp[i] = quantile(obs_samples, 0.95)/survey_data_city_grouped$Build[i]
    survey_data_city_grouped$median_samp[i] = median(obs_samples)/survey_data_city_grouped$Build[i]
  }
  survey_data_city_grouped$City = levels(field_data$City)[j]
  field_data_with_preds %<>% rbind(survey_data_city_grouped)
}
field_data_with_preds$obs_count = field_data_with_preds$Build * field_data_with_preds$prop_dam

par(mfrow=c(3,4))
field_data_with_preds_grouped = group_by(field_data_with_preds,structure_type) %>% group_split()
for (i in 1:length(field_data_with_preds_grouped)){
  dat = field_data_with_preds_grouped[[i]]
  plot(x=0, y=0, col='white',xlim=c(0, 2), ylim=c(0, 1), xlab='PGV', ylab='Proportion damaged', main=dat$structure_type[1])
  for (j in 1:NROW(dat)){
    arrows(dat$rounded_PGV[j],dat$min_samp[j], dat$rounded_PGV[j], dat$max_samp[j], length=0.05, angle=90, code=3, col='red')
    points(dat$rounded_PGV[j], dat$median_samp[j], col='red')
    points(dat$rounded_PGV[j], dat$prop_dam[j], pch=19)
  }
}

row1=11
row2=13
plot(t(field_data_with_preds[row1,grep('sampled.', names(field_data_with_preds))]),t(field_data_with_preds[row2,grep('sampled.', names(field_data_with_preds))]))
points(field_data_with_preds$obs_count[row1], field_data_with_preds$obs_count[row2], col='red', pch=19)

#Need to fix
pairplot_regions <- function(dam_counts){
  
  sampled_df <- (dam_counts %>%
                   dplyr::select(polygon_name, starts_with("sampled.")) %>%
                   pivot_longer(cols = -polygon_name, names_to = "sample", values_to = "value") %>%
                   pivot_wider(names_from = polygon_name, values_from = value) %>%
                   mutate(type = "Sampled"))[,-1]
  
  sampled_df[1:(ncol(sampled_df)-1)] <- lapply(sampled_df[1:(ncol(sampled_df)-1)], as.numeric)
  
  # Step 2: Get observed values, same format
  observed_row <- impact_mort %>%
    dplyr::select(polygon_name, observed) %>%
    pivot_wider(names_from = polygon_name, values_from = observed) %>%
    mutate(type = "Observed")
  
  # Step 3: Ensure column types match
  # Make sure all except `type` are numeric
  observed_row[1:(ncol(observed_row)-1)] <- lapply(observed_row[1:(ncol(observed_row)-1)], as.numeric)
  
  # Step 4: Combine
  plot_df <- bind_rows(sampled_df, observed_row)
  
  obs_vals <- filter(plot_df, type == "Observed")
  
  # Function to overlay observed points
  overlay_observed <- function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      geom_point(alpha = 0.7, size = 0.5, color = "#440154", ...) +
      geom_point(data = obs_vals, mapping = mapping, color = "red", size = 3, shape=4, stroke = 1.5)
  }
  
  gpairs_lower <- function(g){
    g$plots <- g$plots[-(1:g$nrow)]
    g$yAxisLabels <- g$yAxisLabels[-1]
    g$nrow <- g$nrow -1
    
    g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
    g$xAxisLabels <- g$xAxisLabels[-g$ncol]
    g$ncol <- g$ncol - 1
    
    g
  }
  
  # Pair plot with custom overlay
  g <- ggpairs(plot_df %>% dplyr::select(-type),
               upper = list(continuous = "blank"),
               lower = list(continuous = overlay_observed),
               diag = list(continuous = "blankDiag"), 
               showStrips=T
  ) +
    theme_minimal() +
    theme(strip.text = element_text(size = 10),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) + 
    scale_x_continuous(
      trans = scales::pseudo_log_trans(sigma = 0.5, base = 10),
      breaks = c(0, 10, 100, 1000),
      labels = scales::comma_format(),
      minor_breaks = NULL
    ) + 
    scale_y_continuous(
      trans = scales::pseudo_log_trans(sigma = 0.5, base = 10),
      breaks = c(0, 10, 100, 1000),
      labels = scales::comma_format(),
      minor_breaks = NULL
    )
  
  gpairs_lower(g)
}



for (i in 1:NROW(prior_frag_structure_type)){
  survey_dat = field_data %>% filter(structure_type == levels(field_data$structure_type)[i]) %>% group_by(rounded_PGV) %>% summarise(prop_dam = mean(GT >=1), Build = n())
  
  
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
  
  for (j in 1:NROW(survey_dat)){
    points(survey_dat[j,1], survey_dat[j,2], pch=19, cex=as.numeric(log(survey_dat[j,3]+1)))
  }
}
par(mfrow=c(1,1))

#-------------------------------------------------------------------------------------------
#---------------- Check posterior building type by city vs true ----------------------------
#-------------------------------------------------------------------------------------------

type_match =  c('RC MRF (1-3 Storeys)' = 'RC MRF or RC Dual System', #should be higher in older city
                'RC MRF (4-7 Storeys)' = 'RC MRF or RC Dual System', #should be higher in new city
                'RC MRF (8+ Storeys)' = 'RC MRF or RC Dual System', #should be higher in new city
                'RC Wall (1-3 Storeys)' = 'RC Wall', #should be higher in older city
                'RC Wall (4-7 Storeys)' = 'RC Wall', #should be higher in new city
                'RC Wall (8+ Storeys)' = 'RC Wall',
                'RC Dual system (4-7 Storeys)' = 'RC MRF or RC Dual System',
                'RC Dual system (8+ Storeys)' = 'RC MRF or RC Dual System', 
                'Other' = 'Other',
                'Reinforced masonry' = 'Other',
                'Unreinforced masonry' = 'Unreinforced Masonry')

type_match_df = data.frame(Type_Index = 1:length(type_match),
                           building_type = names(type_match),
                           building_category = c(type_match))

city_match_df = data.frame(City_Index = 1:length(levels(field_data$City)),
                           City = levels(field_data$City))


df_long <- build_type_by_city %>%
  pivot_longer(-Row.Labels, names_to = "Building_Type", values_to = "Count") %>%
  filter(!is.na(Count))  # Optional: drop NA counts

df_long %<>% add_building_category()


df_percent <- df_long %>%
  group_by(building_category, Row.Labels) %>%
  summarise(Count_New = sum(Count)) %>%
  group_by(Row.Labels) %>%
  reframe(building_category=building_category, Percent = Count_New / sum(Count_New) * 100)

posterior_df <- as_draws_df(fit) %>%
  dplyr::select(starts_with("p_buildingtype"))

# Pivot longer: assuming cities are indexed i and building types j in Stan
posterior_long <- posterior_df %>%
  mutate(.draw = row_number()) %>%  # Adds draw index
  pivot_longer(cols = starts_with("p_buildingtype"), names_to = "name", values_to = "value") %>%
  tidyr::extract(name, into = c("city", "type"), regex = "p_buildingtype\\[(\\d+),(\\d+)\\]", convert = TRUE) %>%
  rename(City_Index = city, Type_Index = type)

posterior_long_with_info <- posterior_long %>%
  left_join(type_match_df, by = "Type_Index") %>%
  left_join(city_match_df, by = "City_Index")

# Now, sum draws by City and building_category
posterior_draws_summed <- posterior_long_with_info %>%
  group_by(.draw, City, building_category) %>%
  summarise(value = sum(value), .groups = "drop")

# Then, summarize
posterior_summary_categories <- posterior_draws_summed %>%
  group_by(City, building_category) %>%
  summarise(
    p_mean = mean(value),
    p_lower = quantile(value, 0.025),
    p_upper = quantile(value, 0.975),
    .groups = "drop"
  )

# Summarize posterior means
# posterior_summary <- posterior_long %>%
#   group_by(City_Index, Type_Index) %>%
#   summarise(
#     p_mean = mean(value),
#     p_lower = quantile(value, 0.025),
#     p_upper = quantile(value, 0.975),
#     .groups = "drop"
#   )

# posterior_summary_full = posterior_summary %>% merge(city_match_df, by='City_Index') %>% merge(type_match_df, by='Type_Index')

# posterior_summary_categories <- posterior_summary_full %>%
#   group_by(City, building_category) %>%
#   summarise(
#     p_mean = sum(p_mean),
#     p_lower = sum(p_lower),
#     p_upper = sum(p_upper),
#     .groups = "drop"
#   )

df_percent_clean <- df_percent %>%
  rename(City = `Row.Labels`) %>%
  mutate(
    City = str_trim(str_to_upper(City)),
    building_category = str_trim(str_to_lower(building_category)),
    p_mean = Percent / 100,
    Source = "Observed"
  ) %>%
  dplyr::select(City, building_category, p_mean, Source)

df_posterior_clean <- posterior_summary_categories %>%
  mutate(
    City = str_trim(str_to_upper(City)),
    building_category = str_trim(str_to_lower(building_category)),
    Source = "Posterior"
  )

df_posterior_clean$City[df_posterior_clean$City=="NURDAƑÛI"] = "NURDAGI"
df_posterior_clean$City[df_posterior_clean$City=="ISKENDURUN"] = "ISKENDERUN"
df_posterior_clean$City[df_posterior_clean$City=="ISLAHIYE"] = "GAZIANTEP"

# Keep only matching combinations
combined_df <- df_percent_clean %>%
  inner_join(df_posterior_clean, by = c("City", "building_category")) %>%
  rename(p_mean_obs = p_mean.x, p_mean_post = p_mean.y) %>%
  pivot_longer(
    cols = starts_with("p_mean"),
    names_to = "Source",
    values_to = "p_mean",
    names_prefix = "p_mean_"
  ) %>%
  mutate(
    p_lower = if_else(Source == "post", p_lower, NA_real_),
    p_upper = if_else(Source == "post", p_upper, NA_real_)
  )

# Plot
ggplot(combined_df, aes(x = City, y = p_mean, fill = Source, group = Source)) +
  geom_col(position = position_dodge2(width = -0.5), width = 0.7) +
  geom_errorbar(
    data = subset(combined_df, Source == "post"),
    aes(ymin = p_lower, ymax = p_upper),
    position = position_dodge2(width = 10),
    width = 0.2
  ) +
  facet_wrap(~building_category, scales = "free") +
  scale_fill_manual(
    values = c("post" = "tomato", "obs" = "skyblue"),
    labels = c("post" = "Posterior", "obs" = "Observed")
  ) +
  labs(
    title = "Observed vs Posterior Building Category Proportions by Building Type",
    x = "City",
    y = "Proportion",
    fill = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 1)

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
build_interest = 8
ministrys_df$GT[build_interest]
ministrys_df$PGV_mean[build_interest] / 100
building2_cols <- posterior_samples[, grep(paste0("^buildingtype_probs_ministrys\\[",build_interest,","), names(posterior_samples))]

building2_cols <- posterior_samples[, grep("^p_buildingtype\\[3,", names(posterior_samples))]

posterior_means <- colMeans(building2_cols)
structure_labels <- levels(field_data$structure_type)
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





stan_data$
  
  
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


