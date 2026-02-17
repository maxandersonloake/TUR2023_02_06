# --- Project directory & helpers ---------------------------------------------
# Base directory for the project (update if running elsewhere)
dir <- '/home/manderso/Documents/GitHub/TUR2023_02_06/'

# Load shared helper functions (Functions.R must define get_IMT_from_xml, etc.)
source(paste0(dir, 'Functions.R'))

# --------------------------------------------------------------------------------
# ----------------------------- Prepare Ministrys data ---------------------------
# --------------------------------------------------------------------------------

# Read ShakeMap XML and extract PSA(0.3s) as a terra raster
xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
meanhaz <- get_IMT_from_xml(xml_loc, 'psa03')
names(meanhaz) <- 'psa03_mean'   # ensure consistent layer name

# Load building-level point dataset (MEUCC / ministrys full points)
if (!file.exists(paste0(dir, 'Data_NonPublic/fullmap_point'))){
  if (!file.exists(paste0(dir, 'Data_NonPublic/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/Final_fullmap_GT_bdarea_adminID.shp'))){
    stop(paste0('Missing Ministrys Data. Please request from authors of Yu et al, 2024.'))
  } else {
    MEUCC_shp <- read_sf(paste0(dir, 'Data_NonPublic/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/Final_fullmap_GT_bdarea_adminID.shp'))
  
    #make shp valid and get centroid
    valid_flags <- st_is_valid(MEUCC_shp, reason = TRUE)
    
    # show which features are invalid
    invalid_idx <- which(!st_is_valid(MEUCC_shp))
    if(length(invalid_idx) > 0) {
      message("Invalid geometries at rows: ", paste(invalid_idx, collapse = ", "))
      print(st_is_valid(MEUCC_shp[invalid_idx, ], reason = TRUE))
    } else {
      message("All geometries valid.")
    }
    
    # 2. Try to fix invalid geometries: preferred -> st_make_valid, fallback -> st_buffer(0)
    # Use GEOS-based st_make_valid if available. If not, lwgeom::st_make_valid is alternative.
    if(any(!st_is_valid(MEUCC_shp))) {
      # try st_make_valid (requires GEOS >= 3.8, but sf exposes it if available)
      MEUCC_fixed <- tryCatch({
        st_make_valid(MEUCC_shp)
      }, error = function(e) {
        message("st_make_valid failed: ", e$message)
        # fallback: buffer by zero — often fixes duplicate-vertex / self-touching polygons
        message("Trying st_buffer(..., 0) fallback.")
        st_buffer(MEUCC_shp, 0)
      })
      
      # If still invalid, you can try casting multipart/singlepart or dropping degenerate features
      still_invalid <- which(!st_is_valid(MEUCC_fixed))
      if(length(still_invalid) > 0) {
        message("Still invalid at rows: ", paste(still_invalid, collapse = ", "))
        # attempt one more common fallback: cast to MULTIPOLYGON (or POLYGON) then buffer(0)
        MEUCC_fixed <- tryCatch({
          st_cast(MEUCC_fixed, "MULTIPOLYGON")
        }, error = function(e){
          MEUCC_fixed
        }) %>% st_buffer(0)
      }
    } else {
      MEUCC_fixed <- MEUCC_shp
    }
    
    pts <- st_centroid(MEUCC_fixed)
    
    # 3) extract coordinates (lon = X, lat = Y)
    coords <- st_coordinates(pts)           # matrix with columns X (lon), Y (lat)
    
    # 4) drop geometry and create the tibble with latitude/longitude columns
    ministrys_df_full <- MEUCC_shp %>%
      st_set_geometry(NULL) %>%              # drop geometry
      as_tibble() %>%
      mutate(
        longitude = coords[, "X"],
        latitude  = coords[, "Y"],
        # consistent types shown in your example:
        osm_id   = as.character(osm_id),
        osm_id_2 = as.character(osm_id_2),
        # replace NA GT with 0 like your target DF
        GT = if_else(is.na(GT), 0, GT)
      ) %>%
      # select/order columns to match your example
      select(FID, osm_id, code, fclass, name, type, GT, building_a,
             full_id, osm_id_2, Admin_area, latitude, longitude)
    
    saveRDS(ministrys_df_full, paste0(dir, 'Data_NonPublic/fullmap_point'))
  }
} else {
  ministrys_df_full <- readRDS(paste0(dir, 'Data_NonPublic/fullmap_point'))
}

# Sample PSA values for each building by extracting raster at point coords
# 'pull' requires dplyr; raster::extract works with matrix of coords
ministrys_df_full$psa03_mean <- pull(meanhaz %>% raster::extract(
  cbind(ministrys_df_full$longitude, ministrys_df_full$latitude)
))

# Build bounding box covering all points (xmin,xmax,ymin,ymax)
bbox <- c(min(ministrys_df_full$longitude),
          max(ministrys_df_full$longitude),
          min(ministrys_df_full$latitude),
          max(ministrys_df_full$latitude))

# Create an empty raster at ~20 x 20 arcsecond resolution (1/60 deg / 3)
r <- raster(resolution = c(1/60/3, 1/60/3), xmn = bbox[1], xmx = bbox[2],
            ymn = bbox[3], ymx = bbox[4])
# Note: resolution of c(1/60/3,1/60/3) gives ~20 arcsec cells.

# ----------------------- Rasterize building counts & damage counts ----------
# Count of buildings per raster cell
build_counts <- rasterize(ministrys_df_full[, c('longitude', 'latitude')], r, fun = 'count')
build_counts[is.na(build_counts)] <- 0

# Count of damaged buildings (GT >= 1) per raster cell
build_dam_counts <- rasterize((ministrys_df_full %>% filter(GT >= 1))[, c('longitude', 'latitude')], r, fun = 'count')
build_dam_counts[is.na(build_dam_counts)] <- 0

# Rasterize PSA(0.3s) mean to the same grid (use vect/rast for terra compatibility)
psa03_mean <- rasterize(vect(ministrys_df_full[, c('longitude','latitude','psa03_mean')], geom = c("longitude","latitude")),
                        rast(r), field = 'psa03_mean', fun = 'max')
psa03_mean[is.na(psa03_mean)] <- 0
psa03_mean_df <- as.data.frame(psa03_mean, xy = TRUE, na.rm = FALSE)

# Proportion damaged per cell (note: may produce NaN where build_counts==0)
prop_dam <- build_dam_counts / build_counts

# Assemble a data.frame summarising per-cell metrics for plotting/analysis
dat <- data.frame(
  build_counts = values(build_counts),
  build_dam_counts = values(build_dam_counts),
  psa03_mean = psa03_mean_df$max,
  lat = psa03_mean_df$y,
  lon = psa03_mean_df$x
)

# Quick exploratory check: proportion of buildings with GT >= 1 where PSA > 140
mean((ministrys_df_full %>% filter(psa03_mean > 140))$GT >= 1)

# Group observed data by building-density bins (only keep cells with PSA>140)
dat_groupedByBuildingDensity <- dat %>%
  filter(psa03_mean > 140) %>%
  group_by(build_counts = round(build_counts / 5) * 5) %>%
  summarize(propDam = mean(build_dam_counts != 0), .groups = 'drop')

# ----------------------- Theoretical simulation of 'prop damaged' ------------
# Simulate the probability that a cell contains at least one damaged building,
# assuming independent Bernoulli damage per building with p = 0.25.
df_testsim <- data.frame(build_count = 0, prop_dam = 0)
for (i in 1:400) {
  n_dam <- rbinom(10000, i, 0.25)
  df_testsim %<>% add_row(build_count = i, prop_dam = mean(n_dam > 0))
}
# Add a saturated case for display
df_testsim %<>% add_row(build_count = 1200, prop_dam = 1)

# ----------------------- Prior-driven incompleteness curves -------------------
n_curves <- 100
xgr <- seq(0, 12000, 1)

# Sample plausible prior values for incompleteness model and create curves
curve_list <- lapply(1:n_curves, function(i) {
  kappa_0 <- runif(1, 0.5, 1)     # scales asymptote in (0,1)
  kappa_1 <- runif(1, 10, 300)    # shape/scale parameter > 0
  
  data.frame(
    xgr = xgr,
    # Model for inclusion probability (or related monotone function); mirrors Stan form
    yax = kappa_0 * sqrt(xgr) / (sqrt(xgr) + kappa_1),
    group = i
  )
})

# Convert each curve to observed proportion-of-cells-with-damage using binomial formula:
for (i in seq_along(curve_list)) {
  curve_list[[i]]$propDam <- NA_real_
  for (j in seq_len(nrow(curve_list[[i]]))) {
    build_count <- curve_list[[i]]$xgr[j]
    # Probability at least one damaged building given per-building damage prob = 0.25 * yax
    curve_list[[i]]$propDam[j] <- 1 - pbinom(0, build_count, 0.25 * curve_list[[i]]$yax[j])
  }
}

# Combine sampled curves and compute 95% interval per x
ablines_all <- bind_rows(curve_list)
abline_ribbon <- ablines_all %>%
  group_by(xgr) %>%
  summarise(
    lower = quantile(propDam, 0.025, na.rm = TRUE),
    upper = quantile(propDam, 0.975, na.rm = TRUE),
    .groups = 'drop'
  )

# Label sources for plotting legend
dat_groupedByBuildingDensity$source <- "Observed"
df_testsim$source <- "Theoretical"
abline_ribbon$source <- "Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)"

# ----------------------------- Plot observed vs theoretical -------------------
ggplot() +
  geom_point(data = dat_groupedByBuildingDensity, aes(x = build_counts, y = propDam, color = source), shape = 19) +
  geom_line(data = df_testsim, aes(x = build_count, y = prop_dam, color = source), linetype = "dashed", linewidth = 1.2) +
  geom_ribbon(data = abline_ribbon, aes(x = xgr, ymin = lower, ymax = upper, fill = source, color = source), alpha = 0.2) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 14, base = 3),
                     breaks = c(0, 20, 50, 100, 200, 400, 800),
                     labels = scales::comma_format(), minor_breaks = NULL,
                     expand = expansion(mult = c(0.01, 0.01)), limits = c(0, 1200)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_color_manual(values = c("Observed" = "black", "Theoretical" = "tomato",
                                "Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)" = "#1FA187")) +
  scale_fill_manual(values = c("Theoretical adjusted by incompleteness model\n(95% interval derived from prior samples)" = "#1FA187")) +
  theme_minimal(base_family = "Times New Roman") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        legend.position = c(0.98, 0.04), legend.justification = c(1, 0)) +
  xlab('Building Density') +
  ylab('Proportion of Raster Cells Containing Damage') +
  guides(color = guide_legend(override.aes = list(linetype = c("blank", "solid", "blank"),
                                                  shape = c(19, NA, NA), fill = c(NA, NA, "#1FA187"))),
         fill = "none")

#Save as: MEUCC_Missingness.pdf — intended size: 7.6 x 4.2 inches

# ----------------------- Compute smoothed densities & save --------------------
# Convert ministrys_df_full to terra vect for raster extraction
df_full_vect <- vect(ministrys_df_full, geom = c("longitude", "latitude"))

# Extract per-point building count (cell-level) into the dataframe
ministrys_df_full$build_dens <- extract(rast(build_counts), df_full_vect)$layer

# Smooth building count raster with a 3x3 mean filter to approximate neighbourhood density
smoothed_density <- focal(build_counts, w = matrix(1, 3, 3), fun = mean, na.policy = "omit")

# Extract smoothed (neighbourhood) density values at each point
ministrys_df_full$smoothed_density <- extract(rast(smoothed_density), df_full_vect)$layer

# Save updated dataframe for downstream steps (PSA included earlier)
saveRDS(ministrys_df_full, paste0(dir, 'ministrys_df_full_psa'))


# --------------------------------------------------------------------------------
# --------------------- Filter data to modelled cities ---------------------------
# --------------------------------------------------------------------------------

# Run MinistysExtractCities.R to filter to only data within cities explored in
# the engineering surveys.

source(paste0(dir, 'MinistrysExtractCities.R'))



# --------------------------------------------------------------------------------
# -------------------------- Prepare data for Stan models ------------------------
# --------------------------------------------------------------------------------

# Load a filtered subset (city bounding boxes applied previously)
ministrys_df <- readRDS(paste0(dir, 'Data_NonPublic/ministrys_df_filt'))

# Subsample per city up to 1000 points each to limit computational cost
set.seed(1)
ministrys_df_sampled <- ministrys_df %>%
  group_by(City) %>%
  group_modify(~ slice_sample(.x, n = pmin(nrow(.x), 1000))) %>%
  ungroup()

# Quick diagnostic: proportion damaged by rounded PSA mean
grouped_min_df <- ministrys_df %>% group_by(psa03_roundmean = round(psa03_mean / 10) * 10) %>%
  summarise(prop_dam = mean(GT >= 1), .groups = 'drop')
ggplot(grouped_min_df, aes(x = psa03_roundmean, y = prop_dam)) + geom_point()

# Priors used in Stan
prior_mu <- c(-0.5, 0.5)
prior_beta <- c(0.6, 0.05)
  
# Assemble data list for Stan
stan_data <- list(
  N_buildings = NROW(ministrys_df_sampled),
  psa = ministrys_df_sampled$psa03_mean / 100,    # scale to g units if needed
  damage_flag = ifelse(ministrys_df_sampled$GT >= 1, 1, 0),
  build_dens = ministrys_df_sampled$build_dens,
  prior_mu = prior_mu,
  prior_beta = prior_beta
)

# --------------------------------------------------------------------------------
# ---------------------- Fit model: simple (no missingness) ---------------------
# --------------------------------------------------------------------------------
stan_model_compiled_simple <- stan_model(paste0(dir, "StanModels/MinistrysFragCurves.stan"))

fit_simple <- sampling(
  stan_model_compiled_simple,
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000,
  control = list(max_treedepth = 15)
)

# --------------------------------------------------------------------------------
# ---------------- Fit model: missingness explicitly accounted for --------------
# --------------------------------------------------------------------------------
stan_model_compiled_missing_accounted <- stan_model(paste0(dir,"StanModels/MinistrysFragCurves_MissingAccounted.stan"))

fit_missing_accounted <- sampling(
  stan_model_compiled_missing_accounted,
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000,
  control = list(max_treedepth = 15)
)

# Quick diagnostics
traceplot(fit_missing_accounted, pars = c('mu', 'beta', 'alph1', 'alph2'))
print(summary(fit_missing_accounted))

# --------------------------------------------------------------------------------
# ------------------ Posterior summaries & PGA_0.99 computation ------------------
# --------------------------------------------------------------------------------
post <- rstan::extract(fit_missing_accounted, pars = c("mu", "beta"))
mu_draws <- as.numeric(post$mu)
beta_draws <- as.numeric(post$beta)
if (length(mu_draws) != length(beta_draws)) stop("Posterior draws length mismatch")

z99 <- qnorm(0.99)                      # 99th percentile of standard normal
pga99_samples <- exp(mu_draws + beta_draws * z99)   # invert lognormal param
pga99_summary <- c(
  mean = mean(pga99_samples),
  median = median(pga99_samples),
  sd = sd(pga99_samples),
  q2.5 = quantile(pga99_samples, 0.025),
  q97.5 = quantile(pga99_samples, 0.975)
)
print(round(pga99_summary, 4))
pga99_from_means <- exp(mean(mu_draws) + mean(beta_draws) * z99)
cat("PGA_0.99 from posterior means: ", round(pga99_from_means, 4), "\n")

# --------------------------------------------------------------------------------
# ------------------------- Plot posterior fragility curves ---------------------
# --------------------------------------------------------------------------------
posterior_samples_simple <- as_draws_df(fit_simple)
posterior_samples_missing_accounted <- as_draws_df(fit_missing_accounted)

mu_post_simple <- pull(posterior_samples_simple[, "mu"])
beta_post_simple <- pull(posterior_samples_simple[, "beta"])
mu_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "mu"])
beta_post_missing_accounted <- pull(posterior_samples_missing_accounted[, "beta"])

psa_grid <- seq(0, 1.5, 0.01)

# Sample 50 posterior curves for plotting
post_curves <- map_dfr(1:50, function(j) {
  mu_simp <- sample(mu_post_simple, 1)
  beta_simp <- sample(beta_post_simple, 1)
  mu_miss <- sample(mu_post_missing_accounted, 1)
  beta_miss <- sample(beta_post_missing_accounted, 1)
  
  tibble(
    PSA = psa_grid,
    prob_simple = plnorm(psa_grid, mu_simp, beta_simp),
    prob_missing_accounted = plnorm(psa_grid, mu_miss, beta_miss),
    type = "Posterior - Missing Accounted",
    curve_id = paste0("post_", j)
  )
})

# Summarize posteriors across sampled curves (median & 95% intervals)
post_summary <- post_curves %>%
  group_by(PSA) %>%
  summarise(
    simple_lower = quantile(prob_simple, 0.025, na.rm = TRUE),
    simple_upper = quantile(prob_simple, 0.975, na.rm = TRUE),
    simple_mean = mean(prob_simple, na.rm = TRUE),
    missing_lower = quantile(prob_missing_accounted, 0.025, na.rm = TRUE),
    missing_upper = quantile(prob_missing_accounted, 0.975, na.rm = TRUE),
    missing_mean = mean(prob_missing_accounted, na.rm = TRUE),
    .groups = 'drop'
  )

# --------------------------------------------------------------------------------
# --------------------------- Prior draws for plotting --------------------------
# --------------------------------------------------------------------------------
n_prior <- 5000
mu_prior <- rnorm(n_prior, mean = stan_data$prior_mu[1], sd = stan_data$prior_mu[2])
beta_prior <- rnorm(n_prior, mean = stan_data$prior_beta[1], sd = stan_data$prior_beta[2])

psa_vals <- unique(post_summary$PSA)
prior_draws <- expand.grid(PSA = psa_vals, draw = 1:n_prior) %>%
  mutate(mu = mu_prior[draw], beta = beta_prior[draw],
         prior_prob = plnorm(PSA, meanlog = mu, sdlog = beta))

prior_summary <- prior_draws %>%
  group_by(PSA) %>%
  summarise(
    prior_lower = quantile(prior_prob, 0.025, na.rm = TRUE),
    prior_median = quantile(prior_prob, 0.5, na.rm = TRUE),
    prior_upper = quantile(prior_prob, 0.975, na.rm = TRUE),
    .groups = 'drop'
  )

# Plot prior & posterior intervals together
ggplot() +
  geom_ribbon(data = prior_summary, aes(x = PSA, ymin = prior_lower, ymax = prior_upper, fill = "Prior"), alpha = 0.2, color = "#1FA187") +
  geom_line(data = prior_summary, aes(x = PSA, y = prior_median), color = "#1FA187", linetype = "dashed") +
  geom_ribbon(data = post_summary, aes(x = PSA, ymin = simple_lower, ymax = simple_upper, fill = "Posterior (without missingness model)"), alpha = 0.4, color = "#FDE725FF") +
  geom_ribbon(data = post_summary, aes(x = PSA, ymin = missing_lower, ymax = missing_upper, fill = "Posterior (with missingness model)"), alpha = 0.4, color = "#440154") +
  geom_line(data = post_summary, aes(x = PSA, y = simple_mean), color = "#FDE725FF", linetype = "dashed") +
  geom_line(data = post_summary, aes(x = PSA, y = missing_mean), color = "#440154", linetype = "dashed") +
  ylab('Probability of Damage') +
  xlab('PSA[T=0.3s] (g)') +
  theme_minimal(base_family = "Times New Roman") +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks = element_line(color = "black"),
        legend.position = c(0.98, 0.02), legend.justification = c(1, 0)) +
  scale_fill_manual(name = NULL, values = c("Prior" = "#1FA187",
                                            "Posterior (without missingness model)" = "#FDE725FF",
                                            "Posterior (with missingness model)" = "#440154")) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
# Save as : MinistrysPostCompare.pdf, 6.5 x 3.25 #, 695 x 265

