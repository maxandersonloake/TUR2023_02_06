
dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

field_data = read.xlsx(paste0(dir, 'Data/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data.xlsx'))
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

ministrys_df_full = readRDS(paste0(dir, 'Data/ministrys_df_full_psa'))

turkoglu_boundary = c(36.830707, 36.877134, 37.366635, 37.399231)
narli_boundary = c(37.127, 37.149, 37.364592, 37.406404)
pazarcik_boundary = c( 37.283116, 37.315828, 37.473938, 37.498906)
kahramanmaras_boundary = c( 36.78380, 37.03423, 37.50322, 37.64026)
nurdagi_boundary = c(36.715787,  36.757126, 37.170644, 37.191371)
hassa_boundary = c(36.498853,  36.538504, 36.790169, 36.810662)
islahiye_boundary = c(36.617287,36.654022, 36.999779, 37.044802)
antakya_boundary = c(36.102128, 36.234871, 36.164137, 36.279347)
kirikhan_boundary = c(36.334894,  36.379665, 36.481456, 36.517878)
iskendurun_boundary = c(36.085182, 36.232811, 36.519096, 36.653900)
# boundary = iskendurun_boundary
# ministrys_df_filt = ministrys_df_full %>% filter(
#   longitude > boundary[1] & longitude < boundary[2] &
#     latitude > boundary[3] & latitude < boundary[4]
# )
# plot(ministrys_df_filt$longitude, ministrys_df_filt$latitude)
# points((field_data %>% filter(City=='Iskendurun'))[,c('longitude', 'latitude')], col='red')

boundaries <- list(
  Turkoglu      = turkoglu_boundary,
  Narli         = narli_boundary,
  Pazarcik      = pazarcik_boundary,
  Kahramanmaras = kahramanmaras_boundary,
  Nurdagi       = nurdagi_boundary,
  Hassa         = hassa_boundary,
  Islahiye      = islahiye_boundary,
  Antakya       = antakya_boundary,
  Kirikhan      = kirikhan_boundary,
  Iskendurun    = iskendurun_boundary
)

# helper: filter df by bounding box
filter_by_bbox <- function(df, bbox_vec, city_name) {
  stopifnot(length(bbox_vec) == 4)
  min_lon <- bbox_vec[1]; max_lon <- bbox_vec[2]
  min_lat <- bbox_vec[3]; max_lat <- bbox_vec[4]
  
  df %>%
    filter(
      !is.na(longitude), !is.na(latitude),
      longitude >= min_lon, longitude <= max_lon,
      latitude  >= min_lat, latitude  <= max_lat
    ) %>%
    mutate(City = factor(city_name, levels = levels(field_data$City)))
}

# apply to all bounding boxes
filt_by_boundary <- lapply(names(boundaries), function(nm) {
  filter_by_bbox(ministrys_df_full, boundaries[[nm]], nm)
})

# combined data frame
ministrys_df_filt <- bind_rows(filt_by_boundary)

# quick check
table(ministrys_df_filt$City)

saveRDS(ministrys_df_filt, paste0(dir, 'Data/ministrys_df_filt'))

