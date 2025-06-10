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
