# --- Load Libraries -----------------------------------------------------------
# Core tidy / spatial / Bayesian helpers used across scripts
library(rstan)
library(openxlsx)
library(tidyverse)  
library(XML)      
library(xml2)     
library(sf)
library(raster)
library(terra)
library(posterior)
library(MCMCprecision)
library(gridExtra)
library(magrittr)
library(geodata)
library(cowplot)
library(pROC)
library(MASS)   
library(gtools) 
library(tidybayes)
library(patchwork)

# --- rstan options ----------------------------------------------------------
# speed up compilation and use multiple cores for sampling
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# --- get_IMT_from_xml --------------------------------------------------------
# Read a USGS ShakeMap XML and return a 'terra' raster for the requested IMT.
# xml_loc: path or URL to ShakeMap XML
# IMT: one of 'MMI', 'pga', 'pgv', 'psa03', 'psa10', 'psa30'
get_IMT_from_xml <- function(xml_loc, IMT = "pgv") {
  # supported IMTs (order assumed to match columns in grid_data after lon/lat)
  IMTs <- c("MMI", "pga", "pgv", "psa03", "psa10", "psa30")
  if (!IMT %in% IMTs) stop("IMT must be one of: ", paste(IMTs, collapse = ", "))
  
  # read xml (xml2 is used to support URLs and robust parsing)
  xm <- xml2::read_xml(xml_loc)
  # convert to XML::xmlParse/xmlToList for the original grid_data structure
  parsed <- XML::xmlParse(xm)
  xml_list <- XML::xmlToList(parsed)
  
  # grid_data is usually a single text block with newline-separated rows
  if (is.null(xml_list$grid_data)) stop("No grid_data element found in XML.")
  lines <- strsplit(xml_list$grid_data, "\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]   # drop empty lines
  
  # parse lon/lat and the chosen IMT value from each line
  # each line expected to be whitespace-separated, e.g. "lon lat MMI pga pgv psa03 psa10 psa30"
  parts <- strsplit(lines, "\\s+")
  lon <- vapply(parts, function(x) as.numeric(x[1]), numeric(1))
  lat <- vapply(parts, function(x) as.numeric(x[2]), numeric(1))
  
  # IMT column index is offset by 2 (lon, lat) -> 3..8
  imt_index <- which(IMTs == IMT) + 2
  imt_vals <- vapply(parts, function(x) as.numeric(x[imt_index]), numeric(1))
  
  # assemble data.frame and round coordinates so grid becomes regularly spaced
  df <- data.frame(longitude = lon, latitude = lat, value = imt_vals)
  # original code removed first row; keep behavior but check size first
  if (nrow(df) > 1) df <- df[-1, ]
  
  # round to 30 arcsec (0.5 arcminute) grid to correct tiny floating errors
  df$longitude <- round(df$longitude * 60 * 2) / (60 * 2)
  df$latitude  <- round(df$latitude  * 60 * 2) / (60 * 2)
  
  # convert to terra raster (x=lon, y=lat, z=value)
  rast_obj <- terra::rast(x = df, type = "xyz", crs = "EPSG:4326")
  names(rast_obj) <- paste0(IMT, "_mean")
  return(rast_obj)
}

#----------------- Attempt to load file, if not raise error --------------------

check_and_read <- function(filename, url) {
  filepath <- file.path(dir, "Data", filename)
  
  if (!file.exists(filepath)) {
    stop(
      paste0(
        "Required file not found: ", filepath, "\n\n",
        "Please download it from:\n",
        url, "\n\n",
        "and save it to:\n",
        filepath
      ),
      call. = FALSE
    )
  }
  
  read.csv(filepath)
}

#---------------- Compare Multinomial-Dirichlet Models -------------------------
library(gtools)

set.seed(1234)  # reproducible

# Parameters
alphas <- c(100, 5, 1)            # concentration parameters to show
base_p <- rep(0.2, 5)  # underlying base proportions for categories A-E
cats <- c("A", "B", "C", "D", "E")
n_samples <- 4                    # number of independent samples per alpha
n_count <- 100                    # sample size for each multinomial draw

# Plot layout: 3 rows x 4 columns (one row per alpha, 4 samples each)
op <- par(mfrow = c(length(alphas), n_samples),
          mar = c(3.2, 3.2, 2.8, 1),  # margins: bottom, left, top, right
          oma = c(2, 2, 3, 2))       # outer margins for a big title

# Colors for each alpha row (pick any you like)
row_cols <- c("#56B4E9", "#F0E442", "#D55E00")  # blue, yellow, orange-red

for (i in seq_along(alphas)) {
  alpha <- alphas[i]
  col_fill <- row_cols[i]
  
  for (s in 1:n_samples) {
    # sample Dirichlet proportions with concentration alpha
    # rdirichlet expects a parameter vector; we use alpha * base_p
    p <- as.vector(gtools::rdirichlet(1, alpha * base_p))
    # draw multinomial counts
    counts <- as.vector(rmultinom(1, size = n_count, prob = p))
    
    # barplot
    bp <- barplot(counts,
                  names.arg = cats,
                  #ylim = c(0, 100),        # common y-axis across all plots
                  las = 1,                 # horizontal labels
                  yaxt = "n",              # we'll add y-axis ticks selectively
                  main = paste0("Sample ", s),
                  col = col_fill,
                  border = "white")
    # add y-axis ticks only on first column to reduce clutter
    if (s == 1) axis(2, at = seq(0, 100, by = 20), las = 1)
    # add a small subtitle in the first column describing alpha
    if (s == 1) mtext(bquote(alpha == .(alpha)), side = 3, line = 0.2, adj = -0.1, cex = 0.9)
  }
}

# Overall title
mtext("Four independent draws per row: Dirichlet-Multinomial (n = 100)",
      outer = TRUE, cex = 1, line = 1)

# restore par
par(op)


