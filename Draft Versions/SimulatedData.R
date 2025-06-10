rm(list=ls())

# Load libraries and packages
library('sf')
library('ggplot2')
library('openxlsx')
library('tidyverse')
library("dplyr")
library("geojsonsf")
library('magrittr')
library('terra')
library('geosphere')

field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data.xlsx')

temp<-paste0("/home/manderso/Documents/GitHub/TUR2023_02_06/tmp/shakemap")

# Download the raster file to the location 'temp'
download.file("https://earthquake.usgs.gov/product/shakemap/us6000jllz/us/1681495642674/download/raster.zip",paste0(temp, '.zip'))
# Unpack the files in the zip document
unzip(paste0(temp, '.zip'),exdir = paste0(temp,"/"))
# Extract the mean hazard intensity from raster
meanhaz<-rast(file.path(temp,"/mmi_mean.flt"))
sdhaz<-rast(file.path(temp,"/mmi_std.flt"))
meanpga<-rast(file.path(temp,"/pga_mean.flt"))
sdpga<-rast(file.path(temp,"/pga_std.flt"))

field_data$mmi_mean = pull(meanhaz %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))
field_data$mmi_std = pull(sdhaz %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))

frag_curves_SA1 = list(
  list(build_class = 'CR/LFM+CDM+DUM/H8', 0.6441431107061223, 0.8070079009468215, 0.9712647342778422, 1.127938424882021, 0.5015144133949087 ),
  list(build_class = 'CR/LFM+CDM+DUM/H5', 0.45766533903016376, 0.6688380915492208, 0.8849425842302858, 1.0936296292151988, 0.4991038795967254)
)
  
)
simDam = function(MMI, build_type){
  
}