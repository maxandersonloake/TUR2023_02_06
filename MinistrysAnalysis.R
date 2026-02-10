
dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

#----------------------------------------------------------------------------------------------
#-------------------------------- Prepare Ministrys data --------------------------------------
#----------------------------------------------------------------------------------------------

xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
meanhaz <- get_IMT_from_xml(xml_loc, 'psa03')
names(meanhaz)= 'psa03_mean'

# ---------- building missingness ----------------------------

ministrys_df_full = readRDS(paste0(dir, 'Data/fullmap_point'))

ministrys_df_full$psa03_mean = pull(meanhaz %>% raster::extract(cbind(ministrys_df_full$longitude, ministrys_df_full$latitude)))

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
psa03_mean = rasterize(vect(ministrys_df_full[, c('longitude', 'latitude', 'psa03_mean')], geom=c("longitude", "latitude")),rast(r), field='psa03_mean', fun='max')
psa03_mean[is.na(psa03_mean)] = 0
psa03_mean_df = as.data.frame(psa03_mean, xy=T, na.rm=F)

prop_dam = build_dam_counts/build_counts

dat = data.frame(
  build_counts = values(build_counts),
  build_dam_counts = values(build_dam_counts),
  psa03_mean =psa03_mean_df$max,
  lat=psa03_mean_df$y,
  lon=psa03_mean_df$x
)

mean((ministrys_df_full %>% filter(psa03_mean>140))$GT >=1)

#dat_groupedByBuildingDensity = dat %>% filter(pgv_mean > 50) %>% group_by(build_counts = round(build_counts/2)*2) %>% summarize(propDam = mean(build_dam_counts!=0))
dat_groupedByBuildingDensity = dat %>% filter(psa03_mean > 140) %>% group_by(build_counts= round(build_counts/5)*5) %>% summarize(propDam = mean(build_dam_counts!=0))


df_testsim = data.frame(build_count=0, prop_dam = 0)
for (i in 1:400){
  build_count = i
  n_dam = rbinom(10000, build_count, 0.25)
  df_testsim %<>% add_row(build_count = i, prop_dam = mean(n_dam>0))
}
df_testsim %<>% add_row(build_count=1200, prop_dam = 1)

n_curves <- 100
xgr <- seq(0, 12000, 1)

# curve_list <- lapply(1:n_curves, function(i) {
#   alph1 <- runif(1, 0.5, 1)
#   alph2 <- runif(1, 30, 300)#runif(1, 0.0001, 0.0015)
#   data.frame(
#     xgr = xgr,
#     yax =  alph1 * sqrt(xgr)/(sqrt(xgr)+alph2),#alph1/(1+exp(-alph2*(xgr-10))), # alph1 * (2/pi * atan(alph2 * xgr)), #alph1 * (1 - exp(-alph2 * xgr)),#pmin(alph1, alph2 * sqrt(xgr)), # 
#     #n_dam = rbinom(10000, xgr, 0.25),
#     #prop_dam = mean(rbinom(10000, xgr, 0.25 * yax) > 0),
#     group = i
#   )
# })

curve_list <- lapply(1:n_curves, function(i) {
  # Sample priors roughly matching your Stan choices
  kappa_0 <- runif(1, 0.5, 1)           # in (0,1)
  kappa_1 <- runif(1, 10, 300)      # >0, centered ~100
  
  # (optional) enforce your earlier soft bounds
  # kappa_0 <- min(max(kappa_0, 30), 300)
  # kappa_1 <- max(kappa_1, 0.5)
  
  data.frame(
        xgr = xgr,
        yax =  kappa_0 * sqrt(xgr)/(sqrt(xgr)+kappa_1),#alph1/(1+exp(-alph2*(xgr-10))), # alph1 * (2/pi * atan(alph2 * xgr)), #alph1 * (1 - exp(-alph2 * xgr)),#pmin(alph1, alph2 * sqrt(xgr)), #
        #n_dam = rbinom(10000, xgr, 0.25),
        #prop_dam = mean(rbinom(10000, xgr, 0.25 * yax) > 0),
        group = i
      )
    
  # sbd <- sqrt(xgr)
  # inc_prob <- kappa_0 * (sbd / (sbd + kappa_1))            # inclusion probability
  # inc_prob <- pmin(pmax(inc_prob, 1e-8), 1 - 1e-8)         # clamp for safety
  # unobs_prob <- 1 - inc_prob                               # p_ministrys_unobserved
  # 
  # data.frame(
  #   xgr = xgr,
  #   inc_prob = inc_prob,
  #   unobs_prob = unobs_prob,
  #   # set yax to what you want to plot; here we mirror Stan's p_ministrys_unobserved:
  #   yax = unobs_prob,
  #   group = i,
  #   kappa_0 = kappa_0,
  #   kappa_1 = kappa_1
  # )
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

abline_ribbon <- ablines_all %>%
  group_by(xgr) %>%
  summarise(
    lower = quantile(propDam, 0.025, na.rm = TRUE),
    upper = quantile(propDam, 0.975, na.rm = TRUE)
  )

dat_groupedByBuildingDensity$source <- "Observed"
df_testsim$source <- "Theoretical"
abline_ribbon$source <- "Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)"

ggplot() +
  # Black points: Observed
  geom_point(
    data = dat_groupedByBuildingDensity,
    aes(x = build_counts, y = propDam, color = source),
    shape = 19
  ) +
  
  # Red dashed line: Theoretical
  geom_line(
    data = df_testsim,
    aes(x = build_count, y = prop_dam, color = source),
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  # Blue ribbon: Theoretical adjusted
  geom_ribbon(
    data = abline_ribbon,
    aes(x = xgr, ymin = lower, ymax = upper, fill = source, color = source),
    alpha = 0.2, linewidth = 0
  ) +
  
  scale_x_continuous(
    trans = scales::pseudo_log_trans(sigma = 14, base = 3),
    breaks = c(0, 20, 50, 100, 200, 400, 800),
    labels = scales::comma_format(),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.01, 0.01)),
    limits= c(0, 1200)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "Theoretical" = "tomato",
      "Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)" = "#1FA187"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)" = "#1FA187"
    )
  ) +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    legend.text = element_text(size = 12),           # Increase font size
    legend.key.height = unit(1.5, "lines") ,
    legend.position = c(0.98, 0.04),         # Bottom right inside
    legend.justification = c(1, 0),          # Align to bottom right corner
    legend.box = "vertical",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",                       # Border color
      linewidth = 0.4                        # Border thickness
    ), 
  ) +
  xlab('Building Density') +
  ylab('Proportion of Raster Cells Containing Damage') +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c( "blank", "solid","blank"),
        shape = c(19, NA, NA),
        fill = c(NA, NA, "#1FA187")
      )
    ),
    fill = "none",
  ) + labs(color = NULL, fill = NULL, shape = NULL, linetype = NULL)

df_full_vect = vect(ministrys_df_full, geom = c("longitude", "latitude"))
# 7.6 x 4.2 , MEUCC_Missingness.pdf


ministrys_df_full$build_dens = extract(rast(build_counts), df_full_vect)$layer

# Rasterize: count number of buildings in each cell
smoothed_density <- focal(build_counts, w = matrix(1, 3, 3), fun = mean, na.policy = "omit")

# Extract smoothed values at each original point
ministrys_df_full$smoothed_density <- extract(rast(smoothed_density), df_full_vect)$layer

saveRDS(ministrys_df_full, paste0(dir, 'ministrys_df_full_psa'))

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

#ministrys_df_full = readRDS(paste0(dir, 'ministrys_df_full_psa'))
#set.seed(1)
#ministrys_df_full %<>% filter(psa03_mean > 140)
#ministrys_df_full %<>% filter(!is.na(psa03_mean))

ministrys_df <- readRDS(paste0(dir, 'Data/ministrys_df_filt'))
set.seed(1)
ministrys_df_sampled <- ministrys_df %>%
  group_by(City) %>%
  group_modify(~ slice_sample(.x, n = pmin(nrow(.x), 1000))) %>%
  ungroup()

#ministrys_df = ministrys_df_full[sample(1:NROW(ministrys_df_full), 10000, replace=F),]

grouped_min_df = ministrys_df %>% group_by(psa03_roundmean = round(psa03_mean/10)*10) %>% summarise(prop_dam = mean(GT >=1))
ggplot(grouped_min_df, aes(x=psa03_roundmean, y=prop_dam)) + geom_point()

prior_mu = c(-0.5, 0.5)
prior_beta = c(0.6, 0.05)

stan_data <- list(
  N_buildings = NROW(ministrys_df_sampled),
  psa = ministrys_df_sampled$psa03_mean/100,
  damage_flag = ifelse(ministrys_df_sampled$GT >= 1, 1, 0),
  build_dens = ministrys_df_sampled$build_dens, 
  prior_mu = prior_mu,
  prior_beta = prior_beta
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

summary(fit_missing_accounted)
#plot(seq(0, 3, 0.01), plnorm(seq(0, 3, 0.01), -1.15, 0.5), ylim=c(0, 1), xlim= c(0, 1.5))
# 
# ministrys_df_full$psa_rounded = round(ministrys_df_full$psa03_mean )/100
# ministrys_df_full$damage_flag = ifelse(ministrys_df_full$GT >0, 1,0)
# df = data.frame(psa_rounded = ministrys_df_full$psa_rounded,
#                 damage_flag =ministrys_df_full$damage_flag)
# prop_damaged = df %>% group_by(psa_rounded) %>% summarise(prop_dam = mean(damage_flag),
#                                                           num_buildings = n())
# 
# ggplot(prop_damaged, aes(x=psa_rounded, y= prop_dam, size=num_buildings)) + geom_point() + xlim(0, 2.5)


#------------------------------------------------------------------------------------------

# Find 99th percentile of distribution

post <- rstan::extract(fit_missing_accounted, pars = c("mu", "beta"))
mu_draws  <- as.numeric(post$mu)    # vector of posterior draws for mu
beta_draws <- as.numeric(post$beta) # vector of posterior draws for beta

# Number of draws
n_draws <- length(mu_draws)
if(length(beta_draws) != n_draws) stop("mu and beta draws length mismatch")

# 99th quantile on the standard normal
z99 <- qnorm(0.99)  # ≈ 2.326348

# Compute posterior samples of the PGA where CDF = 0.99
pga99_samples <- exp(mu_draws + beta_draws * z99)

# Summarize posterior of PGA_0.99
pga99_summary <- c(
  mean = mean(pga99_samples),
  median = median(pga99_samples),
  sd = sd(pga99_samples),
  q2.5 = quantile(pga99_samples, 0.025),
  q97.5 = quantile(pga99_samples, 0.975)
)
print(round(pga99_summary, 4))

# Also report a point estimate using posterior means of mu and beta (optional)
pga99_from_means <- exp(mean(mu_draws) + mean(beta_draws) * z99)
cat("PGA_0.99 from posterior means: ", round(pga99_from_means, 4), "\n")

#------------------------------------------------------------------------------------------

posterior_samples_simple <- as_draws_df(fit_simple)
posterior_samples_missing_accounted <- as_draws_df(fit_missing_accounted)

mu_post_simple <- pull(posterior_samples_simple[, "mu"])
beta_post_simple <- pull(posterior_samples_simple[, "beta"])
mu_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "mu"])
beta_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "beta"])

psa_grid <- seq(0, 1.5, 0.01)

post_curves = map_dfr(1:50, function(j) {
  
  mu_post_sample_simple <- sample(mu_post_simple, 1)
  beta_post_sample_missing_accounted <- sample(beta_post_missing_accounted, 1)
  mu_post_sample_missing_accounted <- sample(mu_post_missing_accounted, 1)
  beta_post_sample_simple <- sample(beta_post_simple, 1)
  
  tibble(
    PSA = psa_grid,
    prob_simple = plnorm(psa_grid, mu_post_sample_simple, beta_post_sample_simple),
    prob_missing_accounted = plnorm(psa_grid, mu_post_sample_missing_accounted, beta_post_sample_missing_accounted),
    type = "Posterior - Missing Accounted",
    curve_id = paste0("post_", j)
  )
})

post_summary <- post_curves %>%
  group_by(PSA) %>%
  summarise(
    simple_lower = quantile(prob_simple, 0.025, na.rm = TRUE),
    simple_upper = quantile(prob_simple, 0.975, na.rm = TRUE),
    simple_mean  = mean(prob_simple, na.rm = TRUE),
    missing_lower = quantile(prob_missing_accounted, 0.025, na.rm = TRUE),
    missing_upper = quantile(prob_missing_accounted, 0.975, na.rm = TRUE),
    missing_mean  = mean(prob_missing_accounted, na.rm = TRUE),
    .groups = "drop"
  )

n_prior = 5000

mu_prior <- rnorm(n_prior, mean = stan_data$prior_mu[1], sd=stan_data$prior_mu[2])#stan_data$prior_mu[1], sd = stan_data$prior_mu[2])
beta_prior <- rnorm(n_prior, mean = stan_data$prior_beta[1], sd = stan_data$prior_beta[2])

# Generate PSA values (x-axis)
psa_vals <- unique(post_summary$PSA)

# Evaluate lognormal CDF at each PSA for all prior draws
prior_draws <- expand.grid(
  PSA = psa_vals,
  draw = 1:n_prior
) %>%
  mutate(
    mu = mu_prior[draw],
    beta = beta_prior[draw],
    prior_prob = plnorm(PSA, meanlog = mu, sdlog = beta)
  )

prior_summary <- prior_draws %>%
  group_by(PSA) %>%
  summarise(
    prior_lower = quantile(prior_prob, 0.025, na.rm = TRUE),
    prior_median = quantile(prior_prob, 0.5, na.rm = TRUE),
    prior_upper = quantile(prior_prob, 0.975, na.rm = TRUE),
    .groups = "drop"
  )


ggplot(post_summary, aes(x=simple_mean, y=simple_mean)) + 
  stat_smooth(geom="ribbon", aes(x= PSA, ymin=simple_lower, ymax=simple_upper))


ggplot() +
  # Posterior without missingness model
  geom_ribbon(
    data = prior_summary,
    aes(x = PSA, ymin = prior_lower, ymax = prior_upper,fill = factor("Prior", 
                                                                             levels = c("Prior", 
                                                                                        "Posterior (without missingness model)", 
                                                                                        "Posterior (with missingness model)"))),
    alpha = 0.2, color="#1FA187", linewidth=0.01
  ) +
  geom_line(data = prior_summary,
            aes(x = PSA, y = prior_median),
            color = "#1FA187", linetype = "dashed", linewidth = 1) +
  geom_ribbon(
    data = post_summary,
    aes(x = PSA, ymin = simple_lower, ymax = simple_upper, fill = factor("Posterior (without missingness model)", 
                                                                         levels = c("Prior", 
                                                                                    "Posterior (without missingness model)", 
                                                                                    "Posterior (with missingness model)"))),
    alpha = 0.4, color="#FDE725FF", linewidth = 0.01
  ) +
  # Posterior with missingness model
  geom_ribbon(
    data = post_summary,
    aes(x = PSA, ymin = missing_lower, ymax = missing_upper, fill = factor("Posterior (with missingness model)", levels = c("Prior", 
                                                                                                                   "Posterior (without missingness model)", 
                                                                                                                   "Posterior (with missingness model)"))),
    alpha = 0.4, color="#440154", linewidth=0.01
  ) +
  geom_line(data = post_summary,
            aes(x = PSA, y = simple_mean),
            color = "#FDE725FF", linetype = "dashed", linewidth = 1) +
  geom_line(data = post_summary,
            aes(x = PSA, y = missing_mean),
            color = "#440154", linetype = "dashed", linewidth = 1) +
  # Prior median line
  ylab('Probability of Damage') +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    legend.position = c(0.98, 0.02),         # Bottom right inside
    legend.justification = c(1, 0),          # Align to bottom right corner
    legend.box = "vertical",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",                       # Border color
      linewidth = 0.4                        # Border thickness
    ),  # Semi-transparent background
    legend.text = element_text(size = 12),
    legend.key.height = unit(1.5, "lines")
  )+
  scale_fill_manual(
    name = NULL,
    values = c("Prior" = "#1FA187",
               "Posterior (without missingness model)" = "#FDE725FF",
               "Posterior (with missingness model)" = "#440154")
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.01))
  ) +
  xlab('PSA[T=0.3s] (g)')

#MinistrysPostCompare.pdf, 6.5 x 3.25 #, 695 x 265
