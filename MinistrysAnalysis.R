
dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

#----------------------------------------------------------------------------------------------
#-------------------------------- Prepare Ministrys data --------------------------------------
#----------------------------------------------------------------------------------------------

xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
meanhaz <- get_IMT_from_xml(xml_loc, 'pgv')
names(meanhaz)= 'PGV_mean'

# ---------- building missingness ----------------------------

ministrys_df_full = readRDS(paste0(dir, 'Data/fullmap_point'))

ministrys_df_full$PGV_mean = pull(meanhaz %>% raster::extract(cbind(ministrys_df_full$longitude, ministrys_df_full$latitude)))

bbox = c(min(ministrys_df_full$longitude), max(ministrys_df_full$longitude), min(ministrys_df_full$latitude), max(ministrys_df_full$latitude))

r = raster(resolution = c(1/60/3, 1/60/3),xmn=bbox[1], xmx=bbox[2], ymn=bbox[3], ymx=bbox[4])
#20 x 20 arcseconds

# rasterize building counts at 20 x 20 arcsecond resolution
build_counts = rasterize(ministrys_df_full[, c('longitude', 'latitude')], r, fun='count')
build_counts[is.na(build_counts)] = 0

# rasterize counts of damaged buildings at 20 x 20 arcsecond resolution
build_dam_counts = rasterize((ministrys_df_full %>% filter(GT>=1))[, c('longitude', 'latitude')],r, fun='count')
build_dam_counts[is.na(build_dam_counts)] = 0

# rasterize pgv_mean at 20 x 20 arcsecond resolution
pgv_mean = rasterize(vect(ministrys_df_full[, c('longitude', 'latitude', 'PGV_mean')], geom=c("longitude", "latitude")),rast(r), field='PGV_mean', fun='max')
pgv_mean[is.na(pgv_mean)] = 0
pgv_mean_df = as.data.frame(pgv_mean, xy=T, na.rm=F)

prop_dam = build_dam_counts/build_counts

dat = data.frame(
  build_counts = values(build_counts),
  build_dam_counts = values(build_dam_counts),
  pgv_mean =pgv_mean_df$max,
  lat=pgv_mean_df$y,
  lon=pgv_mean_df$x
)

dat_groupedByBuildingDensity = dat %>% filter(pgv_mean > 50) %>% group_by(build_counts = round(build_counts/2)*2) %>% summarize(propDam = mean(build_dam_counts!=0))

df_testsim = data.frame(build_count=0, prop_dam = 0)
for (i in 1:400){
  build_count = i
  n_dam = rbinom(10000, build_count, 0.25)
  df_testsim %<>% add_row(build_count = i, prop_dam = mean(n_dam>0))
}
df_testsim %<>% add_row(build_count=1200, prop_dam = 1)

n_curves <- 100
xgr <- seq(0, 10000, 1)

curve_list <- lapply(1:n_curves, function(i) {
  alph1 <- runif(1, 0.3, 1)
  alph2 <- runif(1, 20, 250)#runif(1, 0.0001, 0.0015)
  data.frame(
    xgr = xgr,
    yax =  alph1 * sqrt(xgr)/(sqrt(xgr)+alph2),#alph1/(1+exp(-alph2*(xgr-10))), # alph1 * (2/pi * atan(alph2 * xgr)), #alph1 * (1 - exp(-alph2 * xgr)),#pmin(alph1, alph2 * sqrt(xgr)), # 
    #n_dam = rbinom(10000, xgr, 0.25),
    #prop_dam = mean(rbinom(10000, xgr, 0.25 * yax) > 0),
    group = i
  )
})

for (i in 1:length(curve_list)){
  print(i)
  curve_list[[i]]$propDam = NA
  for (j in 1:nrow(curve_list[[i]])){
    build_count = curve_list[[i]]$xgr[j]
    curve_list[[i]]$propDam[j] = 1 - pbinom(0, build_count, 0.25 * curve_list[[i]]$yax[j]) #mean(rbinom(100, build_count, 0.25* curve_list[[i]]$yax[j])>0)
  }
}

ablines_all <- bind_rows(curve_list)

ggplot(dat_groupedByBuildingDensity, aes(x = build_counts, y = propDam)) +
  geom_point() +
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 10, base = 10),
    breaks = c(0, 20, 50, 100, 200, 500, 1000),
    labels = scales::comma_format(),
    minor_breaks = NULL
  ) +
  theme_minimal() +
  xlab('Building Density') +
  ylab('Proportion with at least one damaged building') +
  geom_line(data = df_testsim, aes(x = build_count, y = prop_dam), color = 'red') +
  #(data = ablines_all, aes(x = xgr, y = yax, group = group), color = 'gray70', alpha = 0.3) + #+
  geom_line(data = ablines_all, aes(x = xgr, y = propDam, group = group), color = 'blue', alpha = 0.1)
  #geom_line(data = abline_single, aes(x = xgr, y = yax), color = 'blue', size = 1.1)

df_full_vect = vect(ministrys_df_full, geom = c("longitude", "latitude"))
# 7 x 4, MEUCCMissingnessModel


ministrys_df_full$build_dens = extract(rast(build_counts), df_full_vect)$layer

# Rasterize: count number of buildings in each cell
smoothed_density <- focal(build_counts, w = matrix(1, 3, 3), fun = mean, na.policy = "omit")

# Extract smoothed values at each original point
ministrys_df_full$smoothed_density <- extract(rast(smoothed_density), df_full_vect)$layer

saveRDS(ministrys_df_full, paste0(dir, 'ministrys_df_full'))

# ---------------------------- prep data -------------------------------------------
# 
# kahramanmaras_shp <- read_sf('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/1Kahramanmaras/Kahramanmaras_GT_BDarea_adminID.shp')
# kahramanmaras_df <- kahramanmaras_df <- kahramanmaras_shp %>%
#   mutate(centroid = st_centroid(st_make_valid(geometry))) %>%
#   mutate(
#     longitude = st_coordinates(centroid)[, 1],
#     latitude = st_coordinates(centroid)[, 2]
#   ) %>%
#   st_drop_geometry()
# 
# kahramanmaras_df$PGV_mean = pull(meanhaz %>% raster::extract(cbind(kahramanmaras_df$longitude, kahramanmaras_df$latitude)))
# kahramanmaras_df$GT[which(is.na(kahramanmaras_df$GT))] = 0
# kahramanmaras_df$city = 3 #which(levels(field_data$City) == "Kahramanmaras")
# 
# bbox_k = c(min(kahramanmaras_df$longitude), max(kahramanmaras_df$longitude), min(kahramanmaras_df$latitude), max(kahramanmaras_df$latitude))
# 
# 
# filt = ministrys_df_full %>% filter(longitude > bbox_k[1] & longitude < bbox_k[2] & latitude > bbox_k[3] & latitude < bbox_k[4])
# 
# kahramanmaras_df$build_dens = extract(rast(build_counts), vect(kahramanmaras_df, geom = c("longitude", "latitude")))$layer
# 
# 
# ministrys_df = kahramanmaras_df[sample(1:NROW(kahramanmaras_df), 10, replace=F),]

#------------------------------------- Prepare data for stan ----------------------------------

set.seed(1)
ministrys_df = ministrys_df_full[sample(1:NROW(ministrys_df_full), 10000, replace=F),]
ministrys_df %<>% filter(!is.na(PGV_mean))

grouped_min_df = ministrys_df %>% group_by(PGV_roundmean = round(PGV_mean/10)*10) %>% summarise(prop_dam = mean(GT >=1))
ggplot(grouped_min_df, aes(x=PGV_roundmean, y=prop_dam)) + geom_point()

stan_data <- list(
  N_buildings = NROW(ministrys_df),
  PGV = ministrys_df$PGV_mean/100,
  damage_flag = ifelse(ministrys_df$GT >= 1, 1, 0),
  build_dens = ministrys_df$build_dens
)
#----------------------------------------------------------------------------------------------
#--------------------------- Fit model: Not accounting for missingness --- --------------------
#----------------------------------------------------------------------------------------------

stan_model_compiled_simple <- stan_model(paste0(dir, "StanModels/MinistrysFragCurves.stan"))

control = list(adapt_delta = 0.99, max_treedepth = 15)

fit_simple <- sampling(
  stan_model_compiled_simple,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  control = list(max_treedepth=15)
)


#----------------------------------------------------------------------------------------------
#--------------------------- Fit model: Accounting for missingness --- ------------------------
#----------------------------------------------------------------------------------------------

stan_model_compiled_missing_accounted <- stan_model(paste0(dir,"StanModels/MinistrysFragCurves_MissingAccounted.stan"))

fit_missing_accounted <- sampling(
  stan_model_compiled_missing_accounted,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  control = list(max_treedepth=15)
)

traceplot(fit_missing_accounted, pars=c('mu', 'beta', 'alph1', 'alph2'))

#------------------------------------------------------------------------------------------

posterior_samples_simple <- as_draws_df(fit_simple)
posterior_samples_missing_accounted <- as_draws_df(fit_missing_accounted)

mu_post_simple <- pull(posterior_samples_simple[, "mu"])
beta_post_simple <- pull(posterior_samples_simple[, "beta"])
mu_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "mu"])
beta_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "beta"])

pgv_grid <- seq(0, 1.7, 0.01)

post_curves = map_dfr(1:50, function(j) {
  
  mu_post_sample_simple <- sample(mu_post_simple, 1)
  beta_post_sample_missing_accounted <- sample(beta_post_simple, 1)
  mu_post_sample_missing_accounted <- sample(mu_post_missing_accounted, 1)
  beta_post_sample_simple <- sample(beta_post_missing_accounted, 1)
  
  tibble(
    PGV = pgv_grid,
    prob_simple = plnorm(pgv_grid, mu_post_sample_simple, beta_post_sample_simple),
    prob_missing_accounted = plnorm(pgv_grid, mu_post_sample_missing_accounted, beta_post_sample_missing_accounted),
    type = "Posterior - Missing Accounted",
    curve_id = paste0("post_", j)
  )
})

ggplot() +
  geom_line(data = filter(post_curves),
            aes(x = PGV, y = prob_simple, group = curve_id),
            color = "tomato", alpha = 0.7) +
  geom_line(data = filter(post_curves),
            aes(x = PGV, y = prob_missing_accounted, group = curve_id),
            color = "green", alpha = 0.7) + ylab('p(Damage)') + theme_minimal()

#MinistrysPostCompare.pdf, 6 x 3
