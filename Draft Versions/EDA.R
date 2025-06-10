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

#styling
col_values <- list('OSM'='white',
                   'None'='white',
                   'slightly_damaged'='yellow', 
                   'Minor (few cracks)' = 'yellow',
                   'moderate damage but repaired' = 'gold',
                   'Moderate (extensive cracks in walls)' = 'goldenrod1',
                   'heavily_damaged'='orange',
                   'Severe (structural damage to system)'='orange',
                   'needs_demolition'='darkorange1',
                   'Partial collapse (portion collapsed)'= 'darkorange1',
                   'collapse'='red',
                   'Complete collapse' = 'red')

#------------------------------------------------------------------------------------------------------------
#----------------------------------------- READ IN THE DATA -------------------------------------------------
#------------------------------------------------------------------------------------------------------------

# Read in the field data:
field_data <- read.xlsx('/home/manderso/Documents/USGS/For Max/all_field_data_kj.xlsx')

# Read in the remaining damage data:
#    - More data points but contains less information (e.g. structure type, age)
#    - Refer to as data_incomplete
csv_transform_geojson <- function(df_csv){
  df_csv$X_geojson <- gsub("'", "\"", df_csv$X_geojson)
  df_csv %<>%
    mutate(geometry = map(X_geojson, geojson_sf)) %>%  # Convert GeoJSON to sf
    tidyr::unnest(geometry)
  return(df_csv)
}

collapse <- read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/Collapse.csv')
collapse %<>% add_column(damage_grade='collapse')

heavily_damaged = read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/Heavily_Damaged.csv')
heavily_damaged %<>% add_column(damage_grade='heavily_damaged')

needs_demolition = read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/Needs_Demolition.csv')
needs_demolition %<>% add_column(damage_grade='needs_demolition')

slightly_damaged = read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/Slightly_Damaged.csv')
slightly_damaged %<>% add_column(damage_grade='slightly_damaged')

data_incomplete = rbind(collapse,
                 needs_demolition,
                 heavily_damaged,
                 slightly_damaged)

data_incomplete %<>% csv_transform_geojson() 
data_incomplete[, c('longitude', 'latitude')] <- st_coordinates(data_incomplete$geometry)

write.csv(data_incomplete, '/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/All_Dam.csv')

# plot_data = data.frame(
#   damage_level = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
#   nBuildings = c(3641936, 175283  ,  6499  , 37265 ,   6658, 1058993, 492188, 44241, 173686, 41398),
#   source = c(rep('Susu', 5), rep('Ministries Residential', 5))
# )
# 
# ggplot(plot_data, aes(x=factor(damage_level), y=nBuildings, fill=source)) +
#   geom_bar(stat="identity", position="dodge") +
#   labs(x="Damage Level", y="Number of Buildings", title="Building Damage Levels by Source") +
#   theme_minimal()

#data_incomplete <- read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/All_Dam.csv')
#data_incomplete <- readRDS('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point')
#------------------------------------------------------------------------------------------------------------
#------------------------------------- Read in Susu's data---------------------------------------------------
#------------------------------------------------------------------------------------------------------------

library(raster)
library(foreign)
nbdi <- raster('/home/manderso/Downloads/2023-Turkey-Syria-Earthquake-ML-Data-v6/meinvyuxiao-2023-Turkey-Syria-Earthquake-ML-Data-bc2b3ad/resampled_tifs/ndbi_re.tif')
dpm_re_alos <- raster('/home/manderso/Downloads/2023-Turkey-Syria-Earthquake-ML-Data-v6/meinvyuxiao-2023-Turkey-Syria-Earthquake-ML-Data-bc2b3ad/resampled_tifs/dpm_re_alos.tif')
dpm_re <- raster('/home/manderso/Downloads/2023-Turkey-Syria-Earthquake-ML-Data-v6/meinvyuxiao-2023-Turkey-Syria-Earthquake-ML-Data-bc2b3ad/resampled_tifs/dpm_re.tif')
adi <- raster('/home/manderso/Downloads/2023-Turkey-Syria-Earthquake-ML-Data-v6/meinvyuxiao-2023-Turkey-Syria-Earthquake-ML-Data-bc2b3ad/resampled_tifs/adi_re.tif')

k_shp <- read_sf('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/1Kahramanmaras/Kahramanmaras_GT_BDarea_adminID.shp')

k_dbf <- read.dbf('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/1Kahramanmaras/Kahramanmaras_GT_BDarea_adminID.dbf', as.is = FALSE)

plot(nbdi, xlim=c(36.95,36.975), ylim=c(37.58,  37.586))
#plot(dpm_re_alos, xlim=c(36.95,36.975), ylim=c(37.58,  37.586))

#MEUCC_dat_full = read.csv('/home/manderso/Documents/USGS/For Max/2023-turkey-syria-earthquake-master-damage/damage/All_Dam.csv')
#table(MEUCC_dat_full$damage_grade)
#------------------------------------------------------------------------------------------------------------
#--------------------------------------- Add Shakemap -------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# directory = dir = '/home/manderso/Documents/GitHub/ODDRIN/'
# source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetUSGS.R')
# ShakeRast <- GetUSGS(bbox = c(35, 35, 42, 40),sdate=as.Date('2023/02/05'), fdate = as.Date('2023/02/08'))
# names(ShakeRast[[2]])[1] = 'MMI'

# temp<-paste0("/home/manderso/Documents/GitHub/TUR2023_02_06/tmp/shakemap")
# 
# # Download the raster file to the location 'temp'
# download.file("https://earthquake.usgs.gov/product/shakemap/us6000jllz/us/1681495642674/download/raster.zip",paste0(temp, '.zip'))
# # Unpack the files in the zip document
# unzip(paste0(temp, '.zip'),exdir = paste0(temp,"/"))
# # Extract the mean hazard intensity from raster
# meanhaz<-rast(file.path(temp,"/mmi_mean.flt"))
# sdhaz<-rast(file.path(temp,"/mmi_std.flt"))
# meanpga<-rast(file.path(temp,"/pga_mean.flt"))
# sdpga<-rast(file.path(temp,"/pga_std.flt"))
# 
# field_data$mmi_mean = pull(meanhaz %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))
# field_data$mmi_std = pull(sdhaz %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))
# data_incomplete$mmi_mean = pull(meanhaz %>% raster::extract(cbind(data_incomplete$longitude, data_incomplete$latitude)))
# data_incomplete$mmi_std = pull(sdhaz %>% raster::extract(cbind(data_incomplete$longitude, data_incomplete$latitude)))


source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetUSGS.R')
shake_xml_loc <- '/home/manderso/Documents/GitHub/TUR2023_02_06/ShakeMapUpd.xml.gz'
haz <- ExtractUSGS_xml(shake_xml_loc)
field_data$mmi_mean = pull(haz$mmi_mean %>% raster::extract(cbind(field_data$longitude, field_data$latitude)))
data_incomplete$mmi_mean = pull(haz$mmi_mean %>% raster::extract(cbind(data_incomplete$longitude, data_incomplete$latitude)))

#------------------------------------------------------------------------------------------------------------
#------------------------------ Turn damage levels to ordered factors----------------------------------------
#------------------------------------------------------------------------------------------------------------

data_incomplete$damage_grade =  factor(data_incomplete$damage_grade, levels=c('OSM', 'slightly_damaged', 'heavily_damaged',
                                                                       'needs_demolition', 'collapse'))

field_data$damage_condition  =  factor(field_data$damage_condition, levels=c('None', 'Minor (few cracks)',
                                                                             'moderate damage but repaired', 
                                                                             'Moderate (extensive cracks in walls)',
                                                                             'Severe (structural damage to system)',
                                                                             'Partial collapse (portion collapsed)',
                                                                             'Complete collapse'))

field_data$estimated_building_damage[which(is.na(field_data$estimated_building_damage))] = 'NA'
field_data$estimated_building_damage  =  factor(field_data$estimated_building_damage, levels=c('NA', 'None', '0-5%',
                                                                                               '5-15%', '15-30%', '30-60%',
                                                                                               '60-100%'))

#------------------------------------------------------------------------------------------------------------
#------------------------------ Plot data over defined bbox -------------------------------------------------
#------------------------------------------------------------------------------------------------------------
source('/home/manderso/Documents/GitHub/ODDRIN/RCode/Functions.R')
source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetOSM.R')

#bbox1 <- c(36.95,37.58, 36.975, 37.586)
bbox1 <- c(36.9575, 37.5818, 36.963,37.586)

bbox2 <- c(36.34, 36.48, 36.38, 36.5)

bbox = bbox1

collect_and_add_OSM_buildings <- function(bbox){
  OSM_build <- GetOSMbuildings(bbox)
  OSM_build %<>% as.data.frame()
  OSM_build$area = NULL
  names(OSM_build) = c('longitude', 'latitude')
  OSM_build$damage_grade = 'OSM'
  
  data_incomplete_bbox = data_incomplete %>% filter(longitude > bbox[1], 
                                                    longitude < bbox[3],
                                                    latitude > bbox[2],
                                                    latitude < bbox[4])
  
  data_incomplete_bbox %<>% add_row(OSM_build %>% filter(longitude > bbox[1], 
                                                         longitude < bbox[3],
                                                         latitude > bbox[2],
                                                         latitude < bbox[4]))
  

  return(data_incomplete_bbox)
}

data_incomplete_bbox = collect_and_add_OSM_buildings(bbox)
  
ggplot() + 
  geom_point(data=data_incomplete_bbox %>% map_df(rev), aes(x=longitude, y=latitude, col=damage_grade)) +
  scale_color_manual(values = col_values) 

remove_assumed_duplicates <- function(data_incomplete_bbox){
  osm_points <- data_incomplete_bbox %>% filter(damage_grade == "OSM")
  non_osm_points <- data_incomplete_bbox %>% filter(damage_grade != "OSM")
  
  # Function to find the nearest OSM point for each non-OSM point
  find_nearest_osm <- function(lat, lon, osm_df) {
    if (nrow(osm_df) == 0) return(NA) # Edge case: no OSM points
    distances <- distHaversine(cbind(osm_df$longitude, osm_df$latitude), c(lon, lat))
    return(which.min(distances))  # Returns the index of the nearest OSM point
  }
  
  # Apply function to each non-OSM row
  if (nrow(osm_points) > 0 && nrow(non_osm_points) > 0) {
    for (i in 1:nrow(non_osm_points)){
      row = non_osm_points[i,]
      nearest_index = find_nearest_osm(as.numeric(row["latitude"]), as.numeric(row["longitude"]), osm_points)
      osm_points = osm_points[-nearest_index, ]
    }

    data_incomplete_bbox_nodups = rbind(non_osm_points, osm_points)
  }
}

data_incomplete_bbox_nodups = remove_assumed_duplicates(data_incomplete_bbox)

ggplot() + 
  geom_raster(data = as.data.frame(nbdi, xy=T) %>% filter(x > bbox[1], 
                                                          x < bbox[3],
                                                          y > bbox[2],
                                                          y < bbox[4]), aes(x = x, y = y, fill = ndbi_re)) + 
  geom_point(data=data_incomplete_bbox_nodups %>% map_df(rev), aes(x=longitude, y=latitude, col=damage_grade)) +
  scale_color_manual(values = col_values) 

# Percent of buildings unaffected, assuming all those in OSM but not data_incomplete are unaffected
(sum(data_incomplete_bbox$damage_grade=='OSM')-sum(data_incomplete_bbox$damage_grade!='OSM'))/(nrow(data_incomplete_bbox) - sum(data_incomplete_bbox$damage_grade!='OSM'))

field_data_bbox = field_data %>% filter(longitude > bbox[1], 
                                             longitude < bbox[3],
                                             latitude > bbox[2],
                                             latitude < bbox[4])
  
# plot complete and incomplete data together:
ggplot() + 
  geom_point(data=data_incomplete_bbox_nodups %>% map_df(rev), aes(x=longitude, y=latitude, col=damage_grade)) +
  scale_color_manual(values = col_values) +
  geom_point(data=field_data_bbox, aes(x=longitude, y=latitude, fill=as.factor(damage_condition)),color='black', pch=24, alpha=0.8) + 
  scale_fill_manual(values=col_values) #+ xlim(36.362, 36.368) + ylim(36.496, 36.499)

ggplot() + 
  geom_raster(data = as.data.frame(adi, xy=T) %>% 
                filter(x > bbox[1], x < bbox[3], y > bbox[2], y < bbox[4]), 
              aes(x = x, y = y, fill = adi_re), alpha=0.5) +
  scale_fill_gradient( low = "grey", high = "red") +  # Continuous legend
  geom_point(data=data_incomplete_bbox_nodups %>% map_df(rev), aes(x=longitude, y=latitude, col=damage_grade), pch=1) +
  scale_color_manual(values = col_values) +
  geom_point(data=field_data_bbox, aes(x=longitude, y=latitude,color=as.factor(damage_condition)), pch=19) +#+
  geom_point(data=field_data_bbox, aes(x=longitude, y=latitude), col='black', pch=1, size=2.1)
  #theme_bw()
  #scale_fill_manual(values=col_values) #+ xlim(36.362, 36.368) + ylim(36.496, 36.499)

#ggplot() +
#  geom_raster(data = as.data.frame(nbdi, xy=T) , aes(x = x, y = y, fill = ndbi_re))

# plot building types
ggplot() + 
  geom_point(data=field_data_bbox, aes(x=longitude, y=latitude, col=as.factor(construction_age)))

ggplot() + 
  geom_point(data=field_data_bbox, aes(x=longitude, y=latitude, col=as.factor(structure_type)))

# ggplot() + 
#   geom_point(data=OSM_build, aes(x=Longitude, y=Latitude), col='grey') +
#   #geom_point(data=BingBuild, aes(x=Longitude, y=Latitude), col='white')
#   geom_point(data=data_all, aes(x=Longitude, y=Latitude,color=as.factor(damage_grade)), alpha=0.8) +
#   geom_point(data=field_data1, aes(x=longitude, y=latitude, fill=as.factor(damage_condition)),color='black', pch=24, alpha=0.8) + #color=as.factor(damage_condition))) + 
#   #scale_color_manual(values = col_values) + scale_fill_manual(values = col_values) + 
#   xlim(36.95, 36.975) + ylim(37.581, 37.5865) #+ geom_contour(data=as.data.frame(ShakeRast[[2]]$MMI, xy=T, drop=F), aes(x=x,y=y, z=MMI)) 

#------------------------------------------------------------------------------------------------------------
#------------------------- Plot MMI vs damage across build types---------------------------------------------
#------------------------------------------------------------------------------------------------------------

ggplot(data_incomplete, aes(x=damage_grade, y=mmi_mean)) + geom_violin(scale='count')

ggplot(field_data, aes(x=damage_condition, y=mmi_mean)) + geom_violin(scale="count") 

field_data %>% filter()

field_data$damage_condition[which(field_data$damage_condition=='moderate damage but repaired')] = 'Minor (few cracks)'

ggplot(field_data %>% filter(damage_condition != 'NA'), aes(x=mmi_mean, y=damage_condition, col= structure_type)) + geom_jitter(width=0.05, height=0.1)

# ggplot(field_data %>% filter(damage_condition != 'NA', construction_age != 'NA', construction_age != '1935-1976'), 
#        aes(x = mmi_mean, y = damage_condition, 
#            col = structure_type )) + 
#   geom_jitter(width = 0.05, height = 0.1, stroke = 1, size = 1) + 
#   theme_minimal()

ggplot(field_data %>% filter(damage_condition != 'NA', 
                             construction_age != 'NA', 
                             construction_age != '1935-1976'), 
       aes(x = mmi_mean, y = damage_condition, 
           color = structure_type)) + 
  geom_jitter(width = 0.05, height = 0.1, stroke = 1, size = 1) + 
  #scale_shape_manual(values = c(13, 12, 5, 4, 3)) +  # Use shapes that support fill
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", 'yellow', 'darkorange', 'pink', 'gold', 'black', 'cyan')) +  # Adjust colors as needed
  theme_minimal()



ggplot(field_data %>% filter(damage_condition != 'NA' & structure_type=='RC MRF (1-3 Storeys)'), 
       aes(x = mmi_mean, y = ifelse(damage_condition == 'None', 0, 1))) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  labs(x = "Shaking Intensity", y = "Probability of Damage", 
       title = "Damage Probability vs Shaking Intensity") +
  theme_minimal()

print(field_data %>% group_by(structure_type, construction_age) %>% summarise(n()), n=34)

ggplot(field_data %>% filter(structure_type == "RC MRF (1-3 Storeys)" & 
                               construction_age =='2001-2018'), aes(x=mmi_mean, y=damage_condition)) + geom_jitter(width=0.05, height=0.1)

ggplot(field_data %>% filter(structure_type == "RC MRF (4-7 Storeys)" & 
                               construction_age =='2001-2018'), aes(x=mmi_mean, y=damage_condition)) + geom_jitter(width=0.05, height=0.1)

ggplot(field_data %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 
                                                   "RC MRF (8+ Storeys)") & 
                               construction_age =='2001-2018' & damage_condition != 'NA'), 
                                aes(x=mmi_mean, y=damage_condition, color=structure_type)) + 
                                geom_jitter(width=0.05, height=0.1)

ggplot(field_data %>% filter(structure_type == "RC MRF (1-3 Storeys)" & 
                               construction_age %in% c('1977-1984', '1985-2000', '2001-2018', '2018+') & damage_condition != 'NA'), 
                                aes(x=mmi_mean, y=damage_condition, color=construction_age)) + 
                                geom_jitter(width=0.05, height=0.1)

ggplot(field_data %>% filter(structure_type == "RC MRF (1-3 Storeys)" & 
                               construction_age %in% c('1977-1984', '1985-2000', '2001-2018', '2018+') & damage_condition != 'NA'), 
       aes(x=construction_age, y=as.numeric(estimated_building_damage))) + 
        geom_violin(scale='count')

ggplot(field_data %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 
                                                   "RC MRF (8+ Storeys)") & damage_condition != 'NA'), 
       aes(x=structure_type, y=as.numeric(estimated_building_damage))) + 
  geom_violin(scale='count')


#-------------------------------------------------------------------------------------------------------------
#------------------------------------ Stacked Bar by Location ------------------------------------------------
#-------------------------------------------------------------------------------------------------------------

library(gridExtra)


names(field_data)[which(names(field_data)=='City/Town')] = 'City'

type_percent <- ggplot(field_data, aes(x = City, fill = structure_type)) +
  geom_bar(position='fill') +  # No "identity" needed; ggplot counts occurrences
  labs(title = "Structure Type by City", x = "Category", y = "Percent") +
  theme_minimal()

type_count <- ggplot(field_data, aes(x = City, fill = structure_type)) +
  geom_bar() +  # No "identity" needed; ggplot counts occurrences
  labs(title = "Structure Type by City", x = "Category", y = "Count") +
  theme_minimal()

grid.arrange(type_percent + guides(fill="none"), type_count, ncol=2, widths = c(1.4, 2))

ggplot(field_data, aes(x = City, fill = construction_age)) +
  geom_bar(position='fill') +  # No "identity" needed; ggplot counts occurrences
  labs(title = "Stacked Bar Chart by Count", x = "Category", y = "Count") +
  theme_minimal()



#-------------------------------------------------------------------------------------------------------------
#-------------------------------------Attempt to identify trends----------------------------------------------
#-------------------------------------------------------------------------------------------------------------

data_incomplete$damage_grade_numeric = ifelse(data_incomplete$damage_grade=='slightly_damaged', 1, 2)

data_incomplete_binned <- data_incomplete %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(proportion_damage_2 = mean(damage_grade_numeric == 2))  # Compute proportion

# Plot
ggplot(data_incomplete_binned, aes(x = mmi_rounded, y = proportion_damage_2)) +
  geom_line(color = "blue", size = 1) +  # Line plot
  theme_minimal() +
  labs(title = "Proportion of Damage Grade 2 vs MMI",
       x = "Rounded MMI Mean",
       y = "Proportion of Damage Grade 2")

# data_incomplete_shp <- read_sf('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/Final_fullmap_GT_bdarea_adminID.shp')
# 
# data_incomplete_shp$latitude <- NA
# data_incomplete_shp$longitude <- NA
# 
# chunk_size <- 10000
# total_rows <- nrow(data_incomplete_shp)
# for (start_row in seq(1, total_rows, by = chunk_size)) {
#   end_row <- min(start_row + chunk_size - 1, total_rows)  # Ensure it doesn’t exceed total rows
#   
#   # Extract subset
#   data_chunk <- data_incomplete_shp[start_row:end_row, ]
#   
#   coords <- st_coordinates(st_centroid(st_make_valid(data_chunk$geometry)))
#   
#   # Assign back to main dataframe
#   data_incomplete_shp$longitude[start_row:end_row] <- coords[, 1]
#   data_incomplete_shp$latitude[start_row:end_row] <- coords[, 2]
#   
#   print(paste("Processed rows:", start_row, "to", end_row))
# }
# 
# #data_incomplete_shp[, c('longitude', 'latitude')] = st_coordinates(st_centroid(st_make_valid(data_incomplete_shp$geometry)))
# 
# data_incomplete_no_geometry <- st_drop_geometry(data_incomplete_shp)
# saveRDS(data_incomplete_no_geometry, '/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point')
# fullmap_point <- data_incomplete_no_geometry 
# temp<-paste0("/home/manderso/Documents/GitHub/TUR2023_02_06/tmp/shakemap")
# 
# # Download the raster file to the location 'temp'
# download.file("https://earthquake.usgs.gov/product/shakemap/us6000jllz/us/1681495642674/download/raster.zip",paste0(temp, '.zip'))
# # Unpack the files in the zip document
# unzip(paste0(temp, '.zip'),exdir = paste0(temp,"/"))
# # Extract the mean hazard intensity from raster
# meanhaz<-rast(file.path(temp,"/mmi_mean.flt"))
# sdhaz<-rast(file.path(temp,"/mmi_std.flt"))
# meanpga<-rast(file.path(temp,"/pga_mean.flt"))
# sdpga<-rast(file.path(temp,"/pga_std.flt"))
# 
# fullmap_point$pga_mean = pull(meanpga %>% raster::extract(cbind(fullmap_point$longitude, fullmap_point$latitude)))
# fullmap_point$pga_std = pull(sdpga %>% raster::extract(cbind(fullmap_point$longitude, fullmap_point$latitude)))
# 
# saveRDS(fullmap_point, '/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point')

fullmap_point <- readRDS('/home/manderso/Documents/USGS/For Max/1New_Turkey/1New_Turkey/new_fullmap_GT_bdarea_adminID/fullmap_point')

#data_incomplete$damage_grade_numeric = ifelse(data_incomplete$damage_grade=='slightly_damaged', 1, 2)

fullmap_point_sf <- st_as_sf(fullmap_point, coords=c('longitude', 'latitude'), crs=4326)
fullmap_point_vect <- vect(fullmap_point_sf)
rast_res = 0.05
fullmap_raster <- rasterize(fullmap_point_vect,y= rast(ext(fullmap_point_vect), resolution = rast_res), fun='count')
fullmap_GTprop <- rasterize(fullmap_point_vect[fullmap_point_vect$GT %in% c(1,2,3,4), ],y= rast(ext(fullmap_point_vect), resolution = rast_res), fun='count')

values_at_points <- extract(fullmap_GTprop, fullmap_point_vect)
fullmap_point_assessed <- fullmap_point[!is.na(values_at_points$count),]

ggplot() + 
  geom_smooth(data= fullmap_point_assessed, aes(x=mmi_mean, y=ifelse(GT>=1,1,0)), se=T, col='yellow') + 
  geom_smooth(data= fullmap_point_assessed, aes(x=mmi_mean, y=ifelse(GT>=2,1,0)), se=T, col='orange') +
  geom_smooth(data= fullmap_point_assessed, aes(x=mmi_mean, y=ifelse(GT>=3,1,0)), se=T, col='red') +
  geom_smooth(data= fullmap_point_assessed, aes(x=mmi_mean, y=ifelse(GT>=4,1,0)), se=T, col='black')

library(osmdata)
#boundary_matrix = getbb('kahramanmaraş', format_out = "polygon") 
#boundary_sf <- st_polygon(list(boundary_matrix)) %>% st_sfc() %>% st_sf(crs=4326)

#points_inside <- fullmap_point_sf[st_within(fullmap_point_sf, boundary_sf, sparse = FALSE), ]

bbox1 = c(36.13108, 36.18461, 36.17966, 36.23300)
fullmap_point_bbox1 = fullmap_point %>% filter(longitude > bbox1[1],
                                              longitude < bbox1[3],
                                              latitude > bbox1[2],
                                              latitude < bbox1[4])
#ggplot() + geom_point(data=fullmap_point_bbox1, aes(x=longitude, y=latitude,col=GT))

bbox_binned1 <- fullmap_point_bbox1 %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

bbox2 = c(36.8073, 37.5124, 37.0208, 37.6108)
fullmap_point_bbox2 = fullmap_point %>% filter(longitude > bbox2[1],
                                               longitude < bbox2[3],
                                               latitude > bbox2[2],
                                               latitude < bbox2[4])
#ggplot() + geom_point(data=fullmap_point_bbox, aes(x=longitude, y=latitude,col=GT))
bbox_binned2 <- fullmap_point_bbox2 %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

bbox3 = c(36.2098, 37.0413, 36.2987, 37.1110)
fullmap_point_bbox3 = fullmap_point %>% filter(longitude > bbox3[1],
                                               longitude < bbox3[3],
                                               latitude > bbox3[2],
                                               latitude < bbox3[4])
#ggplot() + geom_point(data=fullmap_point_bbox3, aes(x=longitude, y=latitude,col=GT))
bbox_binned3 <- fullmap_point_bbox3 %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

ggplot() + 
  geom_line(data= bbox_binned1, aes(x=mmi_rounded, y=propDam1),col='yellow') + 
  geom_line(data= bbox_binned1, aes(x=mmi_rounded, y=propDam2), col='orange') +
  geom_line(data= bbox_binned1, aes(x=mmi_rounded, y=propDam3), col='red') +
  geom_line(data= bbox_binned1, aes(x=mmi_rounded, y=propDam4), col='black') +
  
fullmap_point_bbox1$City <- "Kirikhan"
fullmap_point_bbox2$City <- "Kahramanmaras"
fullmap_point_bbox3$City <- "bbox3"

# Combine all data frames into one
df_combined <- bind_rows(fullmap_point_bbox1, fullmap_point_bbox2, fullmap_point_bbox3)

# Calculate the percentage of each GT category within each Source

# Plot the stacked bar chart
ggplot(df_combined, aes(x = City, fill = as.factor(GT))) +
  geom_bar(position='fill') +  # No "identity" needed; ggplot counts occurrences
  labs(title = "Damage Level by City", x = "Category", y = "Percent") +
  theme_minimal()



# Dist to nearest damaged:
# fullmap_dam <- fullmap_point[which(fullmap_point$GT>0),]
# #sf_full <- st_as_sf(fullmap_point, coords = c("longitude", "latitude"), crs = 4326)
# #sf_dam <- st_as_sf(fullmap_dam, coords = c("longitude", "latitude"), crs = 4326)
# 
# dist_within_1 <- function(coords){
#   return(nrow(filter(fullmap_dam, latitude > coords[1] - 0.01,
#                  latitude < coords[1] + 0.01,
#                  longitude > coords[2] - 0.01,
#                  longitude < coords[2] + 0.01)))
# }
# 
# chunk_size <- 1000
# total_rows <- nrow(fullmap_point)
# fullmap_point$nBuildingsNear = NA
# for (start_row in seq(1, total_rows, by = chunk_size)) {
#   end_row <- min(start_row + chunk_size - 1, total_rows)  # Ensure it doesn’t exceed total rows
#   
#   # Extract subset
#   data_chunk <- fullmap_point[start_row:end_row, c('latitude', 'longitude')]
#   fullmap_point$nBuildingsNear[start_row:end_row] = apply(data_chunk, 1, dist_within_1)
#   
#   
#   print(paste("Processed rows:", start_row, "to", end_row))
# }

fullmap_point_assessed$GT[which(is.na(fullmap_point_assessed$GT))] = 0
fullmap_point_assessed_binned <- fullmap_point_assessed %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

fullmap_point$GT[which(is.na(fullmap_point$GT))] = 0
fullmap_point_binned <- fullmap_point %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))


field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                               ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                               ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                               ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                        )))))

field_data_binned <- field_data %>%
  mutate(mmi_rounded = round(mmi_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(mmi_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

# Plot
ggplot() +
  geom_line(data=fullmap_point_assessed_binned, aes(x = mmi_rounded, y = propDam1), color = 'yellow', size = 1, lty=2) + 
  geom_line(data=fullmap_point_assessed_binned, aes(x = mmi_rounded, y = propDam2), color = 'orange', size = 1, lty=2) +
  geom_line(data=fullmap_point_assessed_binned, aes(x = mmi_rounded, y = propDam3), color = 'red', size = 1, lty=2) +
  geom_line(data=fullmap_point_assessed_binned, aes(x = mmi_rounded, y = propDam4), color = 'black', size = 1, lty=2) +
  geom_line(data=fullmap_point_binned, aes(x = mmi_rounded, y = propDam1), color = 'yellow', size = 1) + 
  geom_line(data=fullmap_point_binned, aes(x = mmi_rounded, y = propDam2), color = 'orange', size = 1) +
  geom_line(data=fullmap_point_binned, aes(x = mmi_rounded, y = propDam3), color = 'red', size = 1) +
  geom_line(data=fullmap_point_binned, aes(x = mmi_rounded, y = propDam4), color = 'black', size = 1) +
  #geom_point(data=field_data_binned, aes(x=mmi_rounded, y=propDam1), col='yellow') + 
  #geom_point(data=field_data_binned, aes(x=mmi_rounded, y=propDam2), col='orange') + 
  #geom_point(data=field_data_binned, aes(x=mmi_rounded, y=propDam3), col='red') + 
  #geom_point(data=field_data_binned, aes(x=mmi_rounded, y=propDam4), col='black') + 
  theme_minimal() +
  labs(x = "Rounded MMI Mean",
       y = "Proportion of >= Damage Grade")

#ggplot(fullmap_point) + geom_point(aes(x=longitude, y=latitude, col=GT))

fullmap_point$GT[which(is.na(fullmap_point$GT))] = 0
fullmap_point_binned <- fullmap_point %>%
  mutate(pga_rounded = round(pga_mean / 0.2) * 0.2) %>%  # Round to nearest 0.2
  group_by(pga_rounded) %>%
  summarise(propDam1 = mean(GT >= 1), propDam2 = mean(GT >= 2), propDam3 = mean(GT >= 3), propDam4 = mean(GT >= 4))

ggplot() +
  geom_line(data=fullmap_point_binned, aes(x = pga_rounded * 9.81/100, y = propDam1), color = 'yellow', size = 1) + 
  geom_line(data=fullmap_point_binned, aes(x = pga_rounded * 9.81/100, y = propDam2), color = 'orange', size = 1) +
  geom_line(data=fullmap_point_binned, aes(x = pga_rounded * 9.81/100, y = propDam3), color = 'red', size = 1) +
  geom_line(data=fullmap_point_binned, aes(x = pga_rounded * 9.81/100, y = propDam4), color = 'black', size = 1) +
  theme_minimal() +
  labs(x = "Rounded PGA Mean",
       y = "Proportion of >= Damage Grade")

#------------------------------------------------------------------------------------------------------------
#---------------------------------------- OLD----------------------------------------------------------
#------------------------------------------------------------------------------------------------------------


ggplot(data_all, aes(x=Longitude, y=Latitude,color=as.factor(damage_grade))) + geom_point() + xlim(35.8, 41.5) + ylim(35.5, 39.5) + 
  scale_color_manual(values = col_values)  +xlim(36.95, 36.975) + ylim(37.581, 37.5865)# xlim(36.88, 37) + ylim(37.58, 37.59)# + 
  #geom_point(dat=field_data1, aes(x=longitude, y=latitude, color=damage_condition))

ggplot(field_data1, aes(x=longitude, y=latitude,color=as.factor(damage_condition))) + 
  geom_point() + xlim(36.88, 37) + ylim(37.582, 37.586) #+ stat_bin_2d(drop = F,binwidth = 0.1) 
  #xlim(36.95, 36.975) + ylim(37.581, 37.5865)

OSM_build <- GetOSMbuildings(c(36.95,37.58, 36.975, 37.586))
OSM_build %<>% as.data.frame()

ggplot() + 
  geom_point(data=OSM_build, aes(x=Longitude, y=Latitude), col='grey') +
  #geom_point(data=BingBuild, aes(x=Longitude, y=Latitude), col='white')
  geom_point(data=data_all, aes(x=Longitude, y=Latitude,color=as.factor(damage_grade)), alpha=0.8) +
  geom_point(data=field_data1, aes(x=longitude, y=latitude, fill=as.factor(damage_condition)),color='black', pch=24, alpha=0.8) + #color=as.factor(damage_condition))) + 
  #scale_color_manual(values = col_values) + scale_fill_manual(values = col_values) + 
  xlim(36.95, 36.975) + ylim(37.581, 37.5865) #+ geom_contour(data=as.data.frame(ShakeRast[[2]]$MMI, xy=T, drop=F), aes(x=x,y=y, z=MMI)) 


ggplot() + 
  geom_point(data=field_data1, aes(x=longitude, y=latitude,col=shaking),alpha=0.8)  #color=as.factor(damage_condition))) + 
  #scale_color_gradient(low = "blue", high = "red") +
  #scale_color_manual(values = col_values) + scale_fill_manual(values = col_values) + 
  #xlim(36.95, 36.975) + ylim(37.581, 37.5865) #+ geom_contour(data=as.data.frame(ShakeRast[[2]]$MMI, xy=T, drop=F), aes(x=x,y=y, z=MMI)) 


ggplot() + 
  geom_sf(data = shakemap, color = "blue") +
  #geom_sf(data = slightly_damaged_kmz, color='yellow') + 
  #geom_sf(data = heavily_damaged_kmz, color='red') + xlim(35, 41.5) + ylim(35.5, 39.5) +
  #geom_point(dat=field_data1, aes(x=longitude, y=latitude), col='green') + #+
  #geom_point(dat=OSM_build, aes(x=Longitude, y=Latitude), col='grey') + 
  geom_raster(data=as.data.frame(ShakeRast[[2]]$MMI, xy=T, drop=F), aes(x=x,y=y, fill=MMI)) +
  geom_sf(data=slightly_damaged$geometry, color='yellow') + 
  geom_sf(data=heavily_damaged$geometry, color='orange') + 
  geom_sf(data=collapse$geometry, color='red') +
  geom_sf(data=needs_demolition$geometry, color='purple') +
  geom_point(dat=field_data2, aes(x=longitude, y=latitude), col='green') +
  #xlim(36, 36.1) + ylim(36, 36.1)
  xlim(35.9, 41.3) + ylim(35.5, 39.5) + xlab('Longitude') + ylab('Latitude')
  
source('/home/manderso/Documents/GitHub/ODDRIN/RCode/Functions.R')
source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetOSM.R')
#OSM_build <- GetOSMbuildings(c(36,36, 37, 37))
#plot(OSM_build$Longitude, OSM_build$Latitude)


timeout = 60
p<-ggplot(expand.grid(x = seq(36, 41.5, 0.05), 
                      y = seq(35.5, 39.5, 0.05)), 
          aes(x=x, y=y))+stat_bin_2d(drop = F,binwidth = 0.1) #+ xlim(36, 41.5) + ylim(35.5,39.5) #LOOSEEND: will not work if BD has not been passed as an argument
pg<-(ggplot_build(p))$data[[1]]; 
rm(p)
buildings<-data.frame()
for(i in 1:nrow(pg)){
  print(paste0('Collected buildings: ', i, '/', nrow(pg)))
  bbox<-as.double(pg[i,c("xmin","ymin","xmax","ymax")])
  tbuildings<-tryCatch(ExtractOSMbuild(bbox,timeout=timeout),error=function(e) NULL)
  if(is.null(tbuildings)) next
  buildings%<>%rbind(tbuildings)
}
plot(buildings$Longitude, buildings$Latitude)

# source('/home/manderso/Documents/GitHub/ODDRIN/RCode/GetBuildingCounts.R')
# BingBuild <- getBingBuildingsFromTiles(c(36,35.5, 41.5, 39.5),event_id =169, file_write=NULL)
