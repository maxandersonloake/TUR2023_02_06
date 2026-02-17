# --- Setup -------------------------------------------------------------------
# Base directory for the project (update to your local path if needed)
dir <- '/home/manderso/Documents/GitHub/TUR2023_02_06/'

# Load shared helper functions (assumes Functions.R exists at repo root)
source(paste0(dir, 'Functions.R'))

# --- Load and clean field survey data ---------------------------------------
# Read the Jaiswal field-survey spreadsheet
field_data <- read.xlsx(paste0(dir, 'Data/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data.xlsx'))

# Rename column for consistency
names(field_data)[which(names(field_data) == 'City/Town')] <- 'City'

# Map textual damage_condition to an ordinal GT (ground-truth) scale:
# 0 = None, 1 = Minor, 2 = Moderate, 3 = Partial/Severe, 4 = Collapse (fallback)
field_data$GT <- ifelse(field_data$damage_condition == 'None', 0,
                        ifelse(field_data$damage_condition == 'Minor (few cracks)', 1,
                               ifelse(field_data$damage_condition == 'moderate damage but repaired', 2,
                                      ifelse(field_data$damage_condition == 'Moderate (extensive cracks in walls)', 2,
                                             ifelse(field_data$damage_condition == 'Partial collapse (portion collapsed)', 3,
                                                    ifelse(field_data$damage_condition == 'Severe (structural damage to system)', 3, 4))))))

# Keep only records with GT and a known structure type
field_data %<>% filter(!is.na(GT))
field_data %<>% filter(!is.na(structure_type))

# Clean whitespace and coerce structure_type to character
field_data$structure_type %<>% as.character() %>% trimws()

# Replace an odd spelling/label with 'Other' (data-specific cleaning)
field_data$structure_type[which(field_data$structure_type == 'Column with slab corrugated Asgolan')] <- 'Other'

# Define the canonical ordering of structure types used in modelling
structure_type_ordered <- c(
  'RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)',
  'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
  'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
  'Reinforced masonry', 'Unreinforced masonry'
)

# Convert to ordered factor and numeric code for modelling
field_data$structure_type %<>% factor(levels = structure_type_ordered)
buildingTypes <- as.numeric(field_data$structure_type)

# Standardise city column and create numeric city codes
field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities <- as.numeric(field_data$City)

# --- Load large building-level (MEUCC) dataset -------------------------------
# Preprocessed dataset containing imputed undamaged building footprints + attributes


ministrys_df_full <- readRDS(paste0(dir, 'Data_NonPublic/ministrys_df_full_psa'))

# --- Define bounding boxes for cities of interest ----------------------------
# Each boundary vector: c(min_lon, max_lon, min_lat, max_lat)
turkoglu_boundary      <- c(36.830707, 36.877134, 37.366635, 37.399231)
narli_boundary         <- c(37.127, 37.149, 37.364592, 37.406404)
pazarcik_boundary      <- c(37.283116, 37.315828, 37.473938, 37.498906)
kahramanmaras_boundary <- c(36.78380, 37.03423, 37.50322, 37.64026)
nurdagi_boundary       <- c(36.715787, 36.757126, 37.170644, 37.191371)
hassa_boundary         <- c(36.498853, 36.538504, 36.790169, 36.810662)
islahiye_boundary      <- c(36.617287, 36.654022, 36.999779, 37.044802)
antakya_boundary       <- c(36.102128, 36.234871, 36.164137, 36.279347)
kirikhan_boundary      <- c(36.334894, 36.379665, 36.481456, 36.517878)
iskendurun_boundary    <- c(36.085182, 36.232811, 36.519096, 36.653900)

# Combine into a named list for programmatic filtering
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

# --- Helper: bounding-box filter --------------------------------------------
# Filter a data.frame of points by a bbox vector and assign the given city name.
# Expects columns named 'longitude' and 'latitude' in df.
filter_by_bbox <- function(df, bbox_vec, city_name) {
  stopifnot(length(bbox_vec) == 4)
  min_lon <- bbox_vec[1]; max_lon <- bbox_vec[2]
  min_lat <- bbox_vec[3]; max_lat <- bbox_vec[4]
  
  df %>%
    filter(
      !is.na(longitude), !is.na(latitude),
      longitude >= min_lon, longitude <= max_lon,
      latitude  >= min_lat,  latitude  <= max_lat
    ) %>%
    # Ensure City factor matches levels used in field_data for consistency
    mutate(City = factor(city_name, levels = levels(field_data$City)))
}

# Apply the bbox filter to each named boundary and combine results
filt_by_boundary <- lapply(names(boundaries), function(nm) {
  filter_by_bbox(ministrys_df_full, boundaries[[nm]], nm)
})

# Bind the per-city frames into one combined filtered dataframe
ministrys_df_filt <- bind_rows(filt_by_boundary)

# Quick sanity check: counts per city
print(table(ministrys_df_filt$City))

# Save the filtered set (for downstream analysis)
saveRDS(ministrys_df_filt, paste0(dir, 'Data_NonPublic/ministrys_df_filt'))

# --- Quick plotting check (interactive) -------------------------------------
# Re-load and plot a single city's points as a sanity check
ministrys_filt <- readRDS(paste0(dir, 'Data_NonPublic/ministrys_df_filt'))
plot((ministrys_filt %>% filter(City == 'Kahramanmaras'))$longitude,
     (ministrys_filt %>% filter(City == 'Kahramanmaras'))$latitude,
     main = "Points in Kahramanmaras", xlab = "Longitude", ylab = "Latitude")

