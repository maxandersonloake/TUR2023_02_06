dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

#----------------------------------------------------------------------------------------------
#----------------------------------- Prepare survey data --------------------------------------
#----------------------------------------------------------------------------------------------

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
set.seed(1)  # for reproducibility
colors <- sample(rainbow(length(structure_types)))

field_data$`SA(0.3)_mean` = exp(field_data$`SA(0.3)_mean`)

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




#----------------------------------------------------------------------------------------------
#-------------------------------------- Plot full map -----------------------------------------
#----------------------------------------------------------------------------------------------
library(ggplot2)
library(sf)
library(geodata)


xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
meanhaz <- get_IMT_from_xml(xml_loc, 'psa03')
names(meanhaz)= 'psa03_mean'

# 2. Get Turkey Province Data
turkey_sf <- st_as_sf(gadm(country = "TUR", level = 1, path = tempdir()))

ministrys_df_full = readRDS(paste0(dir, 'Data/ministrys_df_full_psa'))


library(ggplot2)
library(sf)
library(tidyterra) # For geom_spatraster
library(ggnewscale) # Allows multiple fill scales
library(cowplot)
library(ggpubr)

# Increase resolution by a factor of 4 using bilinear interpolation
# 'fact = 4' means each pixel becomes 16 smaller pixels (4x4)
#meanhaz_smooth <- disagg(meanhaz, fact = 8, method = "cubic")

w <- focalMat(meanhaz, d=0.02, type = "Gauss")
meanhaz_smooth <- focal(meanhaz, w = w)
#meanhaz_smooth <- disagg(meanhaz_smooth, fact = 4, method = "near")
#meanhaz_smooth <- disagg(meanhaz_smooth, fact = 4, method = "near")

# 3. Convert smoothed raster to Data Frame
haz_df <- as.data.frame(meanhaz_smooth, xy = TRUE)
# Ensure you use the correct column name for the hazard values (e.g., "meanhaz" or "lyr.1")
names(haz_df)[3] <- "hazard_val"

gt_colors_meucc <- c("Undamaged" = "darkgrey", "Slightly damaged" = "yellow", "Partially collapsed" = "orange", "Moderately/severely collapsed" = "red", "Collapsed" = "darkred")
gt_colors_survey <- c("Undamaged" = "darkgrey", "Minor" = "yellow", "Moderate damage" = "orange", "Severe damage/partial collapse" = "red", "Collapsed" = "darkred")

ministrys_df_full$damage <- factor(ministrys_df_full$GT, 
                               levels = c(0, 1, 2, 3, 4),
                               labels = names(gt_colors_meucc))

# Field Data (Survey)
field_data$damage <- factor(field_data$GT, 
                        levels = c(0, 1, 2, 3, 4),
                        labels = names(gt_colors_survey))


ministrys_plot_data <- ministrys_df_full[sample(1:nrow(ministrys_df_full), 100000), ]
ministrys_plot_data <- ministrys_plot_data[order(ministrys_plot_data$damage), ]

# Filter province labels to remove those outside or cut off by the frame
# We calculate centroids and keep only those within your specific xlim/ylim
map_centers <- st_centroid(turkey_sf)
labels_filtered <- turkey_sf[st_coordinates(map_centers)[,1] > 35.89 & 
                               st_coordinates(map_centers)[,1] < 39.36 &
                               st_coordinates(map_centers)[,2] > 35.8 & 
                               st_coordinates(map_centers)[,2] < 39.37, ]

labels_filtered$NAME_1[4] = 'Kahramanmaras'


#kb <- c(xmin = 36.122128, xmax = 36.184871, ymin =  36.164137,  ymax = 36.249347)
kb <- c(xmin = 36.102128, xmax = 36.234871, ymin =  36.164137,  ymax = 36.279347)

# 2. THE PLOT
p_main = ggplot() +
  # Base Map
  geom_sf(data = turkey_sf, fill = "grey92", color = "black", linewidth = 0.2) +
  
  # Ministry Data (Plotted in order: Undamaged -> Collapsed)
  geom_point(data = ministrys_plot_data, 
             aes(x = longitude, y = latitude, color = damage), 
             size = 0.1, alpha = 0.8) +
  scale_color_manual(
    values = gt_colors_meucc, 
    name = "MEUCC Data",
    guide = guide_legend(override.aes = list(size = 4, alpha = 1)) # Larger legend points
  ) +
  
  # Field Data
  geom_point(data = field_data[-471,], # one data point with erroneous latitude/latitude recording
             aes(x = longitude, y = latitude, fill = damage), 
             shape = 22, color = "black", size = 2, stroke = 0.7) +
  scale_fill_manual(
    values = gt_colors_survey, 
    name = "Engineering Survey Data",
    guide = guide_legend(override.aes = list(size = 4)) # Larger legend points
  ) +
  
  # Reset Color for Contours
  new_scale_color() +
  geom_contour(data = haz_df, aes(x = x, y = y, z = hazard_val, color = after_stat(level)), 
               linewidth = 0.6, alpha = 0.7, bins = 7) +
  scale_color_viridis_c(
    name = "SA[T=0.3] (g)",
    guide = guide_colorbar(barheight = 10) # Makes the hazard scale taller/easier to read
  ) +
  
  # Filtered Labels (Centroids only within view)
  geom_sf_text(data = labels_filtered, aes(label = NAME_1), 
               size = 4, color = "black", fontface = "bold", family = "Times New Roman") +
  
  # Map Outline (Clean top layer)
  geom_sf(data = turkey_sf, fill = NA, color = "black", linewidth = 0.4) +
  
  # Zoom & Aesthetics
  coord_sf(xlim = c(35.82785, 39.10934), ylim = c(35.8, 39.37362), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )# +
  #annotate("rect",
  #         xmin = kb["xmin"], xmax = kb["xmax"],
  #         ymin = kb["ymin"], ymax = kb["ymax"],
  #         fill = NA, color = "black", size = 0.8)

p_main

#-----------------------------------------------------

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

# Helper to add a rect layer for a named boundary
rect_layer <- function(b, color = "red", size = 0.6, linetype = "solid") {
  annotate("rect",
           xmin = b[1], xmax = b[2],
           ymin = b[3], ymax = b[4],
           fill = NA, color = color, size = size, linetype = linetype)
}

haz_df$hazard_val = haz_df$hazard_val / 100
which(turkey_sf$NAME_1 == 'Sanliurfa')

# Rebuild p_main without the field_data layer and with rectangle annotations
p_main <- ggplot() +
  # Base Map
  geom_sf(data = turkey_sf, fill = "grey92", color = "darkgrey", linewidth = 0.2) +
  
  # Ministry Data (Plotted in order: Undamaged -> Collapsed)
  #geom_point(data = ministrys_plot_data, 
  #           aes(x = longitude, y = latitude, color = damage), 
  #           size = 0.1, alpha = 0.8) +
  #scale_color_manual(
  #  values = gt_colors_meucc, 
  #  name = "MEUCC Data",
  #  guide = guide_legend(override.aes = list(size = 4, alpha = 1))
  #) +
  
  # NOTE: removed the Engineering Survey geom_point layer (field_data) per request
  
  # Reset Color for Contours
  new_scale_color() +
  geom_contour(data = haz_df, aes(x = x, y = y, z = hazard_val, color = after_stat(level)), 
               linewidth = 0.6, alpha = 0.7, bins = 7) +
  scale_color_viridis_c(
    name = "SA[T=0.3] (g)",
    guide = guide_colorbar(barheight = 10)
  ) +
  
  # Filtered Labels (Centroids only within view)
  geom_sf_text(data = labels_filtered, aes(label = NAME_1), 
               size = 4.8, color = "black", family='Times',
               nudge_x = ifelse(labels_filtered$NAME_1 == "Sanliurfa", -0.2, 0)) +
  
  # Map Outline (Clean top layer)
  geom_sf(data = turkey_sf, fill = NA, color = "black", linewidth = 0.4) +
  
  # Zoom & Aesthetics
  coord_sf(xlim = c(35.82785, 39.10934), ylim = c(35.8, 39.37362), expand = FALSE) +
  theme_void() +
  theme(
    text = element_text(family = "serif"),
    # Positioning
    legend.position = c(0.97, 0.02),        
    legend.justification = c(1, 0),
    legend.box = "vertical",               
    legend.box.just = "right",
    legend.title = element_text(face = "bold", size = 15),
    legend.text  = element_text(size = 15),
    
    # --- The Unified Legend Box ---
    # Remove individual backgrounds
    legend.background = element_blank(),
    # Apply one background to the entire container
    legend.box.background = element_rect(
      fill = alpha("white", 0.85),
      color = "black",
      linewidth = 0.5
    ),
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(0,4,0,0),
    # ------------------------------
    
    plot.background = element_rect(fill = "white", color = NA),
    # Changed fill = NA here to ensure the map isn't covered
    panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA)
  ) +
  # Add black rectangle outlines for each named boundary
  rect_layer(turkoglu_boundary) +
  rect_layer(narli_boundary) +
  rect_layer(pazarcik_boundary) +
  rect_layer(kahramanmaras_boundary) +
  rect_layer(nurdagi_boundary) +
  rect_layer(hassa_boundary) +
  rect_layer(islahiye_boundary) +
  rect_layer(antakya_boundary, linetype = "dashed") +
  rect_layer(kirikhan_boundary) +
  rect_layer(iskendurun_boundary)

# Print the updated plot
print(p_main)

#---------- Plot only kahramanmaras ------------------

kb <- c(xmin = 36.122128, xmax = 36.214871, ymin =  36.164137,  ymax = 36.249347)

# Filter Ministry Data
ministrys_kahramanmaras <- ministrys_df_full[
  ministrys_df_full$longitude >= kb["xmin"] & ministrys_df_full$longitude <= kb["xmax"] &
    ministrys_df_full$latitude >= kb["ymin"] & ministrys_df_full$latitude <= kb["ymax"], 
]
# Sort for visibility (Undamaged -> Collapsed)
ministrys_kahramanmaras <- ministrys_kahramanmaras[order(ministrys_kahramanmaras$GT), ]

# Filter Field Data
field_kahramanmaras <- field_data[
  field_data$longitude >= kb["xmin"] & field_data$longitude <= kb["xmax"] &
    field_data$latitude >= kb["ymin"] & field_data$latitude <= kb["ymax"], 
]

# 2. Filter Province Labels for this specific zoom
# Only labels whose center is inside this tight boundary
labels_local <- turkey_sf[st_coordinates(st_centroid(turkey_sf))[,1] > kb["xmin"] & 
                            st_coordinates(st_centroid(turkey_sf))[,1] < kb["xmax"] &
                            st_coordinates(st_centroid(turkey_sf))[,2] > kb["ymin"] & 
                            st_coordinates(st_centroid(turkey_sf))[,2] < kb["ymax"], ]


p_zoom = ggplot() +
  # Base Map
  geom_sf(data = turkey_sf, fill = "grey92", color = "black", linewidth = 0.3) +
  
  # Layer 1: Ministry Data (Meucc)
  geom_point(data = ministrys_kahramanmaras, 
             aes(x = longitude, y = latitude, color = damage), 
             size = 0.5, alpha = 0.9) +
  scale_color_manual(
    values = gt_colors_meucc, 
    name = "MEUCC Data",
    guide = guide_legend(override.aes = list(size = 6, alpha = 1))
  ) +
  
  # Layer 2: Field Data (Engineering Survey)
  geom_point(data = field_kahramanmaras, 
             aes(x = longitude, y = latitude, fill = damage), 
             shape = 22, color = "black", size = 1.5, stroke = 0.8) +
  scale_fill_manual(
    values = gt_colors_survey, 
    name = "Engineering Survey Data",
    guide = guide_legend(override.aes = list(size = 6, alpha = 1))
  ) +
  
  # Zoom & Clean up
  coord_sf(xlim = c(kb["xmin"], kb["xmax"]), ylim = c(kb["ymin"], kb["ymax"]), expand = FALSE) +
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),
    
    # Positioning
    legend.position = c(1, 0.02),        
    legend.justification = c(1, 0),
    legend.box = "vertical",               
    
    # Alignment Fixes
    legend.box.just = "left",      # Aligns the MEUCC and Engineering sections to the left of the box
    #legend.justification = "left", # Aligns items within each legend
    
    legend.title = element_text(face = "bold", size = 15),
    legend.text  = element_text(size = 15),
    legend.margin = margin(0, 0, 0, 0),
    
    # --- The Unified Legend Box ---
    legend.background = element_blank(),
    legend.box.background = element_rect(
      fill = alpha("white", 0.85),
      color = "black",
      linewidth = 0.5
    ),
    # Tightened margin to prevent the right-side gap
    legend.box.margin = margin(6, -14, 6, 6), 
    
    plot.margin = margin(0,0,0,4),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "red", linewidth = 1.3, linetype = "dashed", fill = NA)
  )

p_zoom

plot_grid(
  p_main, p_zoom, 
  nrow = 1,
  rel_widths = c(0.46, 0.55)#,
  #align = "h",
  #axis = "tb"
)
# Data_Overview.pdf, 11.5 x 7.1

# # 3. Create the Plot
# p_zoom = ggplot() +
#   # Base Map (Province boundaries)
#   geom_sf(data = turkey_sf, fill = "grey92", color = "black", linewidth = 0.3) +
#   
#   # Layer 1: Ministry Data (Meucc)
#   # size is increased slightly since we are zoomed in
#   geom_point(data = ministrys_kahramanmaras, 
#              aes(x = longitude, y = latitude, color = damage), 
#              size = 0.5, alpha = 0.9) +
#   scale_color_manual(
#     values = gt_colors_meucc, 
#     name = "MEUCC Data",
#     guide = guide_legend(override.aes = list(size = 6, alpha = 1))
#   ) +
#   
#   # Layer 2: Field Data (Engineering Survey)
#   geom_point(data = field_kahramanmaras, 
#              aes(x = longitude, y = latitude, fill = damage), 
#              shape = 22, color = "black", size = 1.5, stroke = 0.8) +
#   scale_fill_manual(
#     values = gt_colors_survey, 
#     name = "Engineering Survey Data",
#     guide = guide_legend(override.aes = list(size = 6))
#   ) +
#   
#   # Layer 3: Local Labels
#   geom_sf_text(data = labels_local, aes(label = NAME_1), 
#                size = 5, color = "black", fontface = "bold") +
#   
#   # Zoom & Clean up
#   coord_sf(xlim = c(kb["xmin"], kb["xmax"]), ylim = c(kb["ymin"], kb["ymax"]), expand = FALSE) +
#   theme_void() +
#   theme(
#     legend.position = c(0.98, 0.02),        # bottom-right inside plot
#     legend.justification = c(1, 0),
#     legend.box = "vertical",                # stack legends
#     legend.box.just = "right",
#     legend.title = element_text(face = "bold"),
#     legend.background = element_rect(
#       fill = alpha("white", 0.85),
#       color = "black",
#       linewidth = 0.5
#     ),
#     legend.margin = margin(6, 6, 6, 6),
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.border = element_rect(color = "red", linewidth = 0.8, linetype = "dashed")
#   )
# 
# p_zoom

meucc_levels <- names(gt_colors_meucc)
df_meucc_legend <- data.frame(
  x = seq_along(meucc_levels),
  y = 1,
  damage = factor(meucc_levels, levels = meucc_levels)
)

p_leg_meucc <- ggplot(df_meucc_legend, aes(x = x, y = y, color = damage)) +
  geom_point(size = 4) +
  scale_color_manual(values = gt_colors_meucc, name = "MEUCC Data") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9))

leg_meucc_grob <- get_legend(p_leg_meucc)

# Engineering Survey legend (fill with black outline)
survey_levels <- names(gt_colors_survey)
df_survey_legend <- data.frame(
  x = seq_along(survey_levels),
  y = 1,
  damage = factor(survey_levels, levels = survey_levels)
)

p_leg_survey <- ggplot(df_survey_legend, aes(x = x, y = y, fill = damage)) +
  geom_point(shape = 21, color = "black", size = 4, stroke = 0.7) +
  scale_fill_manual(values = gt_colors_survey, name = "Engineering Survey Data") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9))

leg_survey_grob <- get_legend(p_leg_survey)

p_for_hazard_leg <- ggplot() +
  geom_contour(data = haz_df, aes(x = x, y = y, z = hazard_val, color = after_stat(level)), 
               linewidth = 0.6, alpha = 0.5, bins = 7) +
  scale_color_viridis_c(
    name = "SA[T=0.3] (g)",
    guide = guide_colorbar(barheight = 10) # Makes the hazard scale taller/easier to read
  ) +
  coord_sf(xlim = c(35.82785, 39.36934), ylim = c(35.8, 39.37362), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.margin = margin(6, 6, 6, 6),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.background = element_rect(color = "black", fill = 'white', linewidth = 0.6)
  )

leg_hazard_grob <- get_legend(p_for_hazard_leg)

p_main_overlaid <- ggdraw(p_main + theme(legend.position = "none")) +
  draw_plot(
    leg_hazard_grob, 
    x = 0.65, y = 0.05,  # Coordinates for bottom right (0 to 1 scale)
    width = 0.3, height = 0.3
  )

plots_row <- plot_grid(
  p_main_overlaid + theme(plot.margin = margin(10, 10, 10, 10)),
  p_zoom + theme(legend.position = "none"),
  ncol = 2,
  rel_widths = c(0.5, 0.5)
)


legend_block <- plot_grid(
  leg_meucc_grob, 
  leg_survey_grob, 
  ncol = 1, 
  align = "v"
) + 
  theme(
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(10, 10, 10, 10) # Adds internal padding inside the box
  )

# 2. Final assembly: stack the maps and the boxed legend block
final_plot <- plot_grid(
  plots_row,
  plot_grid(
    NULL, legend_block, NULL, 
    ncol = 3, 
    rel_widths = c(0.1, 0.8, 0.1) 
  ),
  ncol = 1,
  rel_heights = c(1, 0.1) # Increased height slightly to accommodate the box padding
) + 
  theme(plot.background = element_rect(fill = "white", color = NA))


final_plot









# --- Create two shared legends (MEUCC and Engineering Survey) as separate grobs ---
# Build small dummy datasets covering the factor levels so legends show all categories.

# MEUCC legend (color)
meucc_levels <- names(gt_colors_meucc)
df_meucc_legend <- data.frame(
  x = seq_along(meucc_levels),
  y = 1,
  damage = factor(meucc_levels, levels = meucc_levels)
)

p_leg_meucc <- ggplot(df_meucc_legend, aes(x = x, y = y, color = damage)) +
  geom_point(size = 4) +
  scale_color_manual(values = gt_colors_meucc, name = "MEUCC Data") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9))

leg_meucc_grob <- get_legend(p_leg_meucc)

# Engineering Survey legend (fill with black outline)
survey_levels <- names(gt_colors_survey)
df_survey_legend <- data.frame(
  x = seq_along(survey_levels),
  y = 1,
  damage = factor(survey_levels, levels = survey_levels)
)

p_leg_survey <- ggplot(df_survey_legend, aes(x = x, y = y, fill = damage)) +
  geom_point(shape = 21, color = "black", size = 4, stroke = 0.7) +
  scale_fill_manual(values = gt_colors_survey, name = "Engineering Survey Data") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9))

leg_survey_grob <- get_legend(p_leg_survey)

# --- Arrange the two plots side-by-side with legends underneath ---
# Order requested: "second plot to left, first plot to right"
# From your instruction earlier: second plot (kahramanmaras p2) on left, first plot (main map) on right.
# That corresponds to: left = p_zoom, right = p_main

plots_row <- plot_grid(
  p_zoom, p_main,
  labels = c("", ""),   # no labels
  ncol = 2,
  rel_widths = c(0.48, 0.52)  # adjust widths if desired
)

# Combine the two legend grobs horizontally
legend_row <- plot_grid(
  as_ggplot(leg_meucc_grob), as_ggplot(leg_survey_grob),
  nrow = 2
)

# Final assembly: plots on top, legends below
plot_grid(
  plots_row,
  legend_row,
  ncol = 1,
  rel_heights = c(1, 0.12)  # allocate less space to legends
)

#----------------------------------------------------------------------------------------------
#------------------------------- ROC Analysis to compare IMs ----------------------------------
#----------------------------------------------------------------------------------------------

library(pROC)
roc_data_list <- list()
field_data$dam_flag = field_data$GT >=1

for (IM in c('PGV_mean', 'PGA_mean', 'SA(0.3)_mean', 'SA(1.0)_mean')) {
  for (structure in levels(field_data$structure_type)) {
    
    field_data_filt <- field_data %>%
      filter(structure_type == structure, !is.na(.data[[IM]]))
    
    if (length(unique(field_data_filt$dam_flag)) == 2 && nrow(field_data_filt) >= 10) {
      try({
        roc_obj <- roc(field_data_filt$dam_flag, field_data_filt[[IM]], quiet = TRUE)
        roc_df <- data.frame(
          FPR = rev(1 - roc_obj$specificities),
          TPR = rev(roc_obj$sensitivities),
          structure_type = structure,
          IM = IM,
          AUC = as.numeric(roc_obj$auc)
        )
        roc_data_list[[length(roc_data_list) + 1]] <- roc_df
      }, silent = TRUE)
    }
  }
}

roc_plot_df <- do.call(rbind, roc_data_list)

auc_summary <- roc_plot_df %>%
  distinct(structure_type, IM, AUC)


auc_panel <- ggplot(auc_summary, aes(x = IM, y = AUC, fill = IM)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "AUC Comparison",
    x = "Intensity Measure",
    y = "AUC"
  ) +
  facet_wrap(~ structure_type, nrow = 1) +
  theme_minimal(base_family = "Times") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12)
  )

library(patchwork)

roc_panel <- ggplot(roc_plot_df, aes(x = FPR, y = TPR, color = IM)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey50") +
  facet_wrap(~ structure_type, nrow = 1) +
  labs(
    title = "ROC Curves by Structure Type",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Intensity Measure"
  ) +
  theme_minimal(base_family = "Times") +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom"
  )

# Combine plots
combined_plot <- roc_panel / auc_panel + plot_layout(heights = c(3, 1))

combined_plot



IM_levels <- unique(auc_summary$IM)
auc_summary <- auc_summary %>%
  mutate(IM = factor(IM, levels = IM_levels))

# Collapse all AUC values into a single multi-line label
auc_block <- auc_summary %>%
  arrange(IM) %>%
  group_by(structure_type) %>%
  summarise(
    label = paste0(
      "AUC values: \n",
      paste0(sub("_.*", "", IM), ": ", round(AUC, 2), collapse = "\n")
    ),
    #x = 0.56,
    x = 0.65,
    y = 0.03,
    .groups = "drop"
  )

manual_cols <- c("#440154", "#FDE725", "#1FA187", "#39568CFF")

roc_panel <- ggplot(roc_plot_df, aes(x = FPR, y = TPR, color = sub("_.*", "", IM), group = sub("_.*", "", IM), linetype = sub("_.*", "", IM))) +
  geom_line(linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1, linewidth=0.5,color = "grey60",alpha=0.5) +
  scale_color_manual(values = manual_cols) +
  facet_wrap(~ structure_type, nrow = 1, scales = "fixed") +
  # One label box per facet with all AUC values
  geom_label(
    data = auc_block,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 0,
    size = 3,
    family = "Times New Roman",
    fill = "white",
    color = "black",
    label.size = 0.3
  ) +
  scale_x_continuous(expand=expansion(mult=c(0,0)),
                     breaks = function(x) {
                       br <- pretty(x)
                       br[br != 1]
                     }) +
  scale_y_continuous(expand=expansion(mult=c(0,0))) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.title.x = element_text(),
    strip.text = element_text(size = 12, family = "Times New Roman"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)#,
    #panel.grid.minor=element_blank(),
    #panel.grid.minor=element_blank()
  ) + 
  scale_color_manual(values = manual_cols) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "twodash")) +
  facet_wrap(~ structure_type, nrow = 1, scales = "fixed") +
  labs(
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Intensity Measure",
    linetype = "Intensity Measure"
  ) +
  guides(
    color = guide_legend(title = "Intensity Measure"),
    linetype = guide_legend(title = "Intensity Measure")
  )

roc_panel
#IM_comparison.pdf,10 x 3.1

roc_panel + facet_wrap(~ structure_type, nrow = 2, scales = "fixed") 
#IM_comparison.pdf, 7.5 x 6

#----------------------------------------------------------------------------------------------
#-------------------------------- Prepare Ministrys data --------------------------------------
#----------------------------------------------------------------------------------------------

#ministrys_df_full = readRDS(paste0(dir, 'Data/ministrys_df_full_psa'))

#kahramanmaras_df = ministrys_df_full %>% filter(latitude > 36.78380 & latitude < 37.50322 & longitude > 37.03423 & longitude < 37.64026)
#kahramanmaras_df$GT[which(is.na(kahramanmaras_df$GT))] = 0
#kahramanmaras_df$city = which(levels(field_data$City) == "Kahramanmaras")
#set.seed(1)
#ministrys_df = kahramanmaras_df[sample(1:NROW(kahramanmaras_df), 100, replace=F),]

ministrys_df <- readRDS(paste0(dir, 'Data/ministrys_df_filt'))
ministrys_df$City = match(ministrys_df$City, levels(field_data$City))

#----------------------------------------------------------------------------------------------
#-------------------------------- Prior data --------------------------------------------------
#----------------------------------------------------------------------------------------------

library(xml2)
library(dplyr)
library(tibble)

district_city_matches = c('Narli' = 'KAHRAMANMARAŞ',
                          'Pazarcik' = 'KAHRAMANMARAŞ',
                          'Kahramanmaras' = 'KAHRAMANMARAŞ',
                          'Turkoglu' = 'KAHRAMANMARAŞ',
                          'Nurdagi'='GAZİANTEP',
                          'Hassa' = 'HATAY',
                          'Islahiye' = 'GAZİANTEP',
                          'Antakya' = 'HATAY',
                          'Kirikhan' = 'HATAY',
                          'Iskendurun' = 'HATAY')


#---------------------------------------------------------------
#-------------- Prior Zone Probabilities 2 ---------------------
#---------------------------------------------------------------

Exposure_Res <- read.csv(paste0(dir, 'Data/Exposure_Res_Turkey_Adm1.csv'))
Exposure_Com <- read.csv(paste0(dir, 'Data/Exposure_Com_Turkey_Adm1.csv'))
Exposure_Ind <- read.csv(paste0(dir, 'Data/Exposure_Ind_Turkey_Adm1.csv'))

zone_per_city <- matrix(NA, nrow=length(levels(field_data$City)), ncol=4)
colnames(zone_per_city) = c('Residential', 'Commercial', 'Industrial', 'Mixed')
rownames(zone_per_city) = levels(field_data$City)
for (city in levels(field_data$City)){
  district = district_city_matches[which(names(district_city_matches)==city)]
  zone_per_city[city, 'Residential'] = sum((Exposure_Res %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = sum((Exposure_Res %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Commercial'] = sum((Exposure_Com %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = zone_per_city[city, 'Mixed'] + sum((Exposure_Com %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Industrial'] = sum((Exposure_Ind %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = zone_per_city[city, 'Mixed'] + sum((Exposure_Ind %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
}
zone_per_city_probs = zone_per_city / rowSums(zone_per_city)

zone_per_city_probs %<>% as.data.frame()
zone_per_city_probs$district = district_city_matches[rownames(zone_per_city_probs)]

zone_per_district = zone_per_city_probs %>% group_by(district) %>% summarise(Residential=mean(Residential),
                                                                             Commercial =mean(Commercial),
                                                                             Industrial =mean(Industrial),
                                                                             Mixed = mean(Mixed))

alpha_omega = 10
dirichlet_samples <- lapply(seq_len(nrow(zone_per_district)), function(i) {
  props <- as.numeric(zone_per_district[i, 2:5])
  district_name <- zone_per_district$district[i]
  
  samples <- rdirichlet(5000, alpha_omega * props)  # 5000 samples
  colnames(samples) <- colnames(zone_per_district)[2:5]
  
  as_tibble(samples) %>%
    mutate(district = district_name)
}) %>%
  bind_rows()

# Convert to long format for ggplot
plot_df <- dirichlet_samples %>%
  pivot_longer(cols = c("Residential", "Commercial", "Industrial", "Mixed"),
               names_to = "zone", values_to = "value")

plot_df$zone = factor(plot_df$zone, levels=c('Residential', 'Commercial', 'Industrial', 'Mixed'))

# Define colors for each zone
custom_fill_colors <- c(
  "Residential" = "#1FA187",
  "Commercial" = "#440154",
  "Industrial" = "#FDE725FF",
  "Mixed" = "tomato"
)

plot_list <- lapply(unique(plot_df$district), function(d) {
  district_label <- str_to_sentence(d)
  
  ggplot(filter(plot_df, district == d), aes(x = value, fill = zone)) +
    geom_histogram(aes(y = ..density..),
                   position = "identity", alpha = 0.4,
                   bins = 40, color = "black", linewidth = 0.1) +
    labs(
      x = paste0("Proportion of Building Stock (", district_label, ")"),
      y = "Density",
      fill = NULL
    ) +
    scale_fill_manual(values = custom_fill_colors) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
})

# Combine plots
combined_plot <- wrap_plots(plot_list, ncol = 1) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

combined_plot
#Zone_Priors.pdf, 4.5 x 4.5

#------------------------------------------------------------
# ------------- Prior Building Type -------------------------
#------------------------------------------------------------

Exposure_Res <- read.csv(paste0(dir, 'Data/Exposure_Res_Turkey_Adm1.csv'))
Exposure_Com <- read.csv(paste0(dir, 'Data/Exposure_Com_Turkey_Adm1.csv'))
Exposure_Ind <- read.csv(paste0(dir, 'Data/Exposure_Ind_Turkey_Adm1.csv'))

taxonomies_all = c(Exposure_Res$TAXONOMY, 
                   Exposure_Com$TAXONOMY, 
                   Exposure_Ind$TAXONOMY)

heights <- str_extract(taxonomies_all, "H[^/]+")

#heights <- str_extract(struct_fragility$Building_class, "H[^/]+") %>% unique()

# Classify height into storey range
heights <- toupper(str_replace_all(str_extract(taxonomies_all, "H[^/]+"), "\\s+", ""))

storey_class <- dplyr::case_when(
  str_detect(heights, "^H:?[1-3]$") ~ "(1-3 Storeys)",
  str_detect(heights, "^H:?[4-7]$") ~ "(4-7 Storeys)",
  str_detect(heights, "^H:?([8-9]|[1-9][0-9]+)$") |
    str_detect(heights, "^HBET:?7-9$") |
    str_detect(heights, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
  TRUE ~ ""
)

height_distributions = table(storey_class)[c('(1-3 Storeys)', '(4-7 Storeys)', '(8+ Storeys)')]

map_code_to_structure <- function(ids) {
  set.seed(1)
  # Extract height string (H:1, H:2, HBET:7-9, etc.)
  height <- toupper(str_replace_all(str_extract(ids, "H[^/]+"), "\\s+", ""))
  
  storey_class <- dplyr::case_when(
    str_detect(height, "^H:?[1-3]$") ~ "(1-3 Storeys)",
    str_detect(height, "^H:?[4-7]$") ~ "(4-7 Storeys)",
    str_detect(height, "^H:?([8-9]|[1-9][0-9]+)$") |
      str_detect(height, "^HBET:?7-9$") |
      str_detect(height, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
    TRUE ~ ""
  )
  
  # Fill blanks if needed (assumes height_distributions exists in scope)
  miss_idx <- which(storey_class == "")
  if (length(miss_idx) > 0) {
    storey_class[miss_idx] <- sample(
      x = names(height_distributions),
      size = length(miss_idx),
      replace = TRUE,
      prob = as.numeric(height_distributions)
    )
  }
  
  # # ---- minimal addition: vectorised RC choice (avoids case_when recycling) ----
  # is_cr   <- str_detect(ids, "^CR")
  # low     <- is_cr & storey_class == "(1-3 Storeys)"
  # midhigh <- is_cr & storey_class %in% c("(4-7 Storeys)", "(8+ Storeys)")
  # 
  # rc_pick <- rep(NA_character_, length(ids))
  # if (any(low)) {
  #   rc_pick[low] <- sample(c("RC MRF", "RC Wall"),
  #                          sum(low), replace = TRUE, prob = c(0.95, 0.05))
  # }
  # if (any(midhigh)) {
  #   rc_pick[midhigh] <- sample(c("RC MRF", "RC Wall", "RC Dual system"),
  #                              sum(midhigh), replace = TRUE, prob = c(0.90, 0.05, 0.05))
  # }
  # # ---------------------------------------------------------------------------
  # 
  dplyr::case_when(
    str_detect(ids, "^MUR") ~ "Unreinforced masonry",
    str_detect(ids, "^MCF") ~ "Reinforced masonry",
    
    # RC MRF (Moment Frame)
    str_detect(ids, "^CR") &
      !str_detect(ids, "LWAL") &
      !str_detect(ids, "LDUAL") ~ paste("RC MRF", storey_class),
    
    # RC Wall system
    str_detect(ids, "^CR") & str_detect(ids, "LWAL") ~ paste("RC Wall", storey_class),
    
    # RC Dual system
    str_detect(ids, "^CR") & str_detect(ids, "LDUAL") ~ paste("RC Dual system", storey_class),
    
    TRUE ~ "Other"
  )
}

map_code_to_structure_confident <- function(ids) {
  
  # Extract height string (H:1, H:2, HBET:7-9, etc.)
  height <- toupper(str_replace_all(str_extract(ids, "H[^/]+"), "\\s+", ""))
  
  storey_class <- dplyr::case_when(
    str_detect(height, "^H:?[1-3]$") ~ "(1-3 Storeys)",
    str_detect(height, "^H:?[4-7]$") ~ "(4-7 Storeys)",
    str_detect(height, "^H:?([8-9]|[1-9][0-9]+)$") |
      str_detect(height, "^HBET:?7-9$") |
      str_detect(height, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
    TRUE ~ ""
  )
  
  # Now classify main structure type
  dplyr::case_when(
    str_detect(ids, "^MUR") ~ "Unreinforced masonry",
    str_detect(ids, "^MCF") ~ "Reinforced masonry",
    
    # RC MRF (Moment Frame)
    str_detect(ids, "^CR") &
      !str_detect(ids, "LWAL") &
      !str_detect(ids, "LDUAL") ~ paste("RC MRF", storey_class),
    
    # RC Wall system
    str_detect(ids, "^CR") & str_detect(ids, "LWAL") ~ paste("RC Wall", storey_class),
    
    # RC Dual system
    str_detect(ids, "^CR") & str_detect(ids, "LDUAL") ~ paste("RC Dual system", storey_class),
    
    TRUE ~ "Other"
  )
}


#use the GEM exposure data to count the number of buildings of each type in each city+zone
buildings_per_zonecity <- array(NA, dim=c(length(levels(field_data$structure_type)), length(levels(field_data$City)), 4))
dimnames(buildings_per_zonecity) = list(levels(field_data$structure_type),
                                        levels(field_data$City),
                                        c('Residential', 'Commercial', 'Industrial', 'Mixed'))


for (city in levels(field_data$City)){
  district = district_city_matches[which(names(district_city_matches)==city)]
  district_data = (Exposure_Res %>% filter(NAME_1 == toupper(district), !grepl("MIX", TAXONOMY)))
  district_data$structure_type = map_code_to_structure(district_data$TAXONOMY)
  building_counts = district_data %>% group_by(structure_type) %>% summarise(building_count = sum(BUILDINGS))
  buildings_per_zonecity[, city, 'Residential'] = building_counts[match(rownames(buildings_per_zonecity), building_counts$structure_type),]$building_count
  
  district_data = (Exposure_Com %>% filter(NAME_1 == toupper(district), !grepl("MIX", TAXONOMY)))
  district_data$structure_type = map_code_to_structure(district_data$TAXONOMY)
  building_counts = district_data %>% group_by(structure_type) %>% summarise(building_count = sum(BUILDINGS))
  buildings_per_zonecity[, city, 'Commercial'] = building_counts[match(rownames(buildings_per_zonecity), building_counts$structure_type),]$building_count
  
  district_data = (Exposure_Ind %>% filter(NAME_1 == toupper(district), !grepl("MIX", TAXONOMY)))
  district_data$structure_type = map_code_to_structure(district_data$TAXONOMY)
  building_counts = district_data %>% group_by(structure_type) %>% summarise(building_count = sum(BUILDINGS))
  buildings_per_zonecity[, city, 'Industrial'] = building_counts[match(rownames(buildings_per_zonecity), building_counts$structure_type),]$building_count
  
  district_data = rbind(Exposure_Res %>% filter(NAME_1 == toupper(district), grepl("MIX", TAXONOMY)),
                        Exposure_Com %>% filter(NAME_1 == toupper(district), grepl("MIX", TAXONOMY)),
                        Exposure_Ind %>% filter(NAME_1 == toupper(district), grepl("MIX", TAXONOMY)))
  district_data$structure_type = map_code_to_structure(district_data$TAXONOMY)
  building_counts = district_data %>% group_by(structure_type) %>% summarise(building_count = sum(BUILDINGS))
  buildings_per_zonecity[, city, 'Mixed'] = building_counts[match(rownames(buildings_per_zonecity), building_counts$structure_type),]$building_count
  
}

buildings_per_zonecity_probs <- buildings_per_zonecity

for (k in 1:dim(buildings_per_zonecity)[3]) {
  slice <- buildings_per_zonecity_probs[,,k]
  cs <- colSums(slice, na.rm = TRUE)
  # divide each column by its column sum
  buildings_per_zonecity_probs[,,k] <- sweep(slice, 2, cs, FUN = "/")
}

fixed_building_probs = c(
  'RC Wall' = 0.05,
  'RC Dual system' = 0.025,
  'Reinforced masonry' = 0.05
)

height_distributions = height_distributions / sum(height_distributions)
buildings_per_zonecity_probs['RC Wall (1-3 Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(1-3 Storeys)']
buildings_per_zonecity_probs['RC Wall (4-7 Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(4-7 Storeys)']
buildings_per_zonecity_probs['RC Wall (8+ Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(8+ Storeys)']
buildings_per_zonecity_probs['RC Dual system (4-7 Storeys)',,] = fixed_building_probs['RC Dual system'] * height_distributions['(4-7 Storeys)']/sum(height_distributions['(4-7 Storeys)'] + height_distributions['(8+ Storeys)']) #only consider RC Dual Systems 4+ storyes
buildings_per_zonecity_probs['RC Dual system (8+ Storeys)',,] = fixed_building_probs['RC Dual system'] * height_distributions['(8+ Storeys)']/sum(height_distributions['(4-7 Storeys)'] + height_distributions['(8+ Storeys)']) #only consider RC Dual Systems 4+ storyes
buildings_per_zonecity_probs['Reinforced masonry',,] = fixed_building_probs['Reinforced masonry']

non_fixed_rows = !(rownames(buildings_per_zonecity) %in% c('RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)', 
                                                    'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Reinforced masonry'))

buildings_per_zonecity_probs[buildings_per_zonecity_probs < 0.02] = 0.02

for (k in 1:dim(buildings_per_zonecity)[3]) {
  slice <- buildings_per_zonecity_probs[non_fixed_rows,,k]
  cs <- colSums(slice, na.rm = TRUE)
  # divide each column by its column sum
  buildings_per_zonecity_probs[non_fixed_rows,,k] <- sweep(slice, 2, cs, FUN = "/") * (1-sum(fixed_building_probs))
}

#------------------------------------------------------------
# ------------- Prior Fragility Curve -----------------------
#------------------------------------------------------------

# xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
# 
# shake_xml_loc = read_xml(xml_loc)
# grid <- xmlParse(shake_xml_loc)
# 
# xml_data <- xmlToList(grid)
# lines <- strsplit(xml_data$grid_data, "\n")[[1]] #strsplit(xml_data[[20]], "\n")[[1]]
# 
# IMT_vals <- t(sapply(lines, function(x) {
#   parts <- strsplit(x, " ")[[1]]
#   as.numeric(parts[3:8])
# }))
# 
# rownames(IMT_vals) = rep("", nrow(IMT_vals))
# colnames(IMT_vals) = c('MMI', 'pga', 'pgv', 'psa03', 'psa10', 'psa30')
# IMT_vals = IMT_vals[-1,]
# 
# saveRDS(IMT_vals, paste0(dir, 'Data/IMT_compare_shakemap'))

IMT_vals = readRDS(paste0(dir, 'Data/IMT_compare_shakemap'))
IMT_vals %<>% as.data.frame()

struct_fragility <- read.csv(paste0(dir,'Data/struct_fragility.csv'))

struct_fragility$structure_type = map_code_to_structure_confident(struct_fragility$Building_class)
struct_fragility %<>% filter(Damage_state == 'slight')


ref_curves <- struct_fragility %>%
  pivot_longer(
    cols = starts_with("iml."),
    names_to = "IM",
    values_to = "P_Damage"
  ) %>%
  mutate(
    IM = as.numeric(str_remove(IM, "iml\\."))
  )

ref_curves$PSA0.3 = NA
ref_curves$PSA0.3[which(ref_curves$IMT == "SA(0.3s)")] = ref_curves$IM[which(ref_curves$IMT == "SA(0.3s)")]

lm_SA.3_PGV = lm(psa03 ~ pgv-1, data=IMT_vals)
lm_SA.3_PGA = lm(psa03 ~ pga-1, data=IMT_vals)
lm_SA.3_SA1 = lm(psa03 ~ psa10-1, data=IMT_vals)

ref_curves$PSA0.3[which(ref_curves$IMT == "PGA")] = ref_curves$IM[which(ref_curves$IMT == "PGA")] * coef(lm_SA.3_PGA)[1]
ref_curves$PSA0.3[which(ref_curves$IMT == "SA(1.0s)")] = ref_curves$IM[which(ref_curves$IMT == "SA(1.0s)")] * coef(lm_SA.3_SA1)[1]
ref_curves %<>% filter(structure_type != "RC MRF " & structure_type != "RC Wall " & structure_type != 'RC Dual system '
                       & structure_type != 'RC Dual system (1-3 Storeys)')

#ref_curves %<>% filter(IMT=='SA(0.3s)')# | IMT=='SA(1.0s)' & structure_type %in% c('RC MRF (8+ Storeys)', 'RC Wall (8+ Storeys)', 'RC Dual system (8+ Storeys)'))


prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.9,0.6,0.6,0.15), #c(-0.25,0.25,0.55,0.05), 
                                  'RC MRF (4-7 Storeys)' = c(-0.9,0.6,0.6,0.15), #c(-1.25,0.25,0.6,0.05), 
                                  'RC MRF (8+ Storeys)' = c(-0.9,0.6,0.6,0.15), 
                                  'RC Wall (1-3 Storeys)' = c(0.2,0.5,0.6,0.15), 
                                  'RC Wall (4-7 Storeys)' = c(-0.5,0.4,0.6,0.15), 
                                  'RC Wall (8+ Storeys)' = c(-0.6,0.5,0.6,0.15),
                                  'RC Dual system (4-7 Storeys)' = c(-0.7,0.6,0.6,0.15),
                                  'RC Dual system (8+ Storeys)' = c(-0.7,0.6,0.6,0.15), 
                                  'Other' = c(0,0.6,0.6,0.15),
                                  'Reinforced masonry' = c(-0.2,0.6,0.6,0.15),
                                  'Unreinforced masonry' = c(-1.2,0.4,0.6,0.15))

prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]


colnames(prior_frag_structure_type) = c('mu_mean', 'mu_sd', 'sigma_mean', 'sigma_sd')

psa_vals <- seq(0.01, 2.5, by = 0.01)

# Assume prior_frag_structure_type has structure_type rownames
prior_frag_structure_type <- as.data.frame(prior_frag_structure_type)
prior_frag_structure_type$structure_type <- rownames(prior_frag_structure_type)

# Generate 100 samples of fragility curves for each structure type
set.seed(123)
prior_ribbon_data <- prior_frag_structure_type %>%
  rowwise() %>%
  mutate(curves = list({
    replicate(100, {
      mu <- rnorm(1, mu_mean, mu_sd)
      sigma <- rlnorm(1, log(sigma_mean), sigma_sd)
      plnorm(psa_vals, mu, sigma)
    }, simplify = "matrix") %>%
      as.data.frame() %>%
      mutate(PSA = psa_vals) %>%
      pivot_longer(-PSA, names_to = "sample", values_to = "P_Damage")
  })) %>%
  dplyr::select(structure_type, curves) %>%
  unnest(curves) %>%
  group_by(structure_type, PSA) %>%
  summarise(
    lower = quantile(P_Damage, 0.025),
    median = quantile(P_Damage, 0.5),
    upper = quantile(P_Damage, 0.975),
    .groups = "drop"
  )

plot_order <- c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", "RC MRF (8+ Storeys)",
  "RC Wall (1-3 Storeys)", "RC Wall (4-7 Storeys)", "RC Wall (8+ Storeys)",
  "RC Dual system (4-7 Storeys)", "RC Dual system (8+ Storeys)", "Reinforced masonry",
  "Unreinforced masonry", "Other"
)

prior_ribbon_data$structure_type <- factor(prior_ribbon_data$structure_type, levels = plot_order)
ref_curves$structure_type <- factor(ref_curves$structure_type, levels = plot_order)

ref_curves_extended <- ref_curves %>%
  group_by(Building_class, Damage_state, structure_type) %>%
  group_modify(~ {
    df <- .x
    # Only interpolate if there are at least 2 distinct x values
    if(length(unique(df$PSA0.3)) >= 2) {
      # Perform linear interpolation using approx()
      interp <- approx(x = df$PSA0.3, y = df$P_Damage, xout = 2.5, rule = 2)
      
      # Create a new row for PSA0.3 = 2.5
      new_row <- df[1, ]
      new_row$PSA0.3 <- 2.5
      new_row$P_Damage <- interp$y
      
      # Bind and ensure rows are sorted by PSA0.3
      bind_rows(df, new_row) %>% arrange(PSA0.3)
    } else {
      df
    }
  }) %>%
  ungroup() %>%
  mutate(alpha_line = ifelse(structure_type == "Other",  0.05, 0.2),
         linewidth_num = ifelse(structure_type == "Other", 0.2, 0.1))

PriorFragCurves = ggplot(prior_ribbon_data, aes(x = PSA)) +
  geom_line(
    data = ref_curves_extended,
    aes(x = PSA0.3, y = P_Damage, group = Building_class, alpha = alpha_line, linewidth = linewidth_num, linetype = ifelse(IMT=='SA(0.3s)',"solid", "dotted")),
    color = "darkgrey",
    inherit.aes = FALSE
  ) + scale_linetype_identity() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1FA187", alpha = 0.4) +
  geom_line(aes(y = median), color = "#1FA187", linewidth = 0.8) +
  #geom_line(data = ref_curves, aes(x = PSA, y = P_Damage, color = color), size = 1, inherit.aes = FALSE) +
  scale_color_identity() +
  facet_wrap(~structure_type, ncol = 4, scales='free') +
  labs(x = "SA[T=0.3] (g)", y = "Probability of Damage") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.15, "cm"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(expand=expansion(mult = c(0.01,0)),
                     limits=c(0, 2.5)) +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  scale_y_continuous(expand=expansion(mult = c(0.01,0.01)))

PriorFragCurves
# PriorFragCurves.pdf, 8.5 x 5

PriorFragCurves + facet_wrap(~structure_type, ncol = 3, scales='free')
# PriorFragCurves.pdf, 7.5 x 8 (portrait)

prior_frag_structure_type$structure_type = NULL
buildingtypes_priorprob = buildings_per_zonecity_probs 
for (k in 1:dim(buildingtypes_priorprob)[3]) {
  slice <- buildingtypes_priorprob[,,k]
  slice[is.na(slice)] = 0.01
  cs <- colSums(slice, na.rm = TRUE)
  # divide each column by its column sum
  buildingtypes_priorprob[,,k] <- sweep(slice, 2, cs, FUN = "/")
}
prior_frag_URM_dest = as.numeric(prior_frag_structure_type[NROW(prior_frag_structure_type),])
prior_frag_URM_dest[1] = 0.5

mean_PSA_by_city = field_data %>% group_by(City) %>% summarise(mean_PSA=mean(`SA(0.3)_mean`))

#------------------------------------------------------------

# 
# 
# 
# # Load XML
# xml_path <- "vulnerability_structural.xml"
# doc <- read_xml(paste0(dir, 'Data/',xml_path))
# 
# ns <- xml_ns_rename(xml_ns(doc), d1 = "nrml")
# vuln_fns <- xml_find_all(doc, ".//nrml:vulnerabilityFunction", ns)
# 
# # Extract only those with SA(0.3)
# sa03_fns <- Filter(function(fn) {
#   imt_attr <- xml_attr(xml_find_first(fn, "nrml:imls", ns), "imt")
#   imt_attr == "SA(0.3)"
# }, vuln_fns)
# 
# 
# # Parse to a list of data frames
# fragilities <- lapply(sa03_fns, function(fn) {
#   id <- xml_attr(fn, "id")
#   
#   imls <- strsplit(xml_text(xml_find_first(fn, "nrml:imls", ns)), " ")[[1]]
#   imls <- as.numeric(imls[nzchar(imls)])
#   
#   meanLRs <- strsplit(xml_text(xml_find_first(fn, "nrml:meanLRs", ns)), " ")[[1]]
#   meanLRs <- as.numeric(meanLRs[nzchar(meanLRs)])
#   
#   covLRs <- strsplit(xml_text(xml_find_first(fn, "nrml:covLRs", ns)), " ")[[1]]
#   covLRs <- as.numeric(covLRs[nzchar(covLRs)])
#   
#   tibble(
#     id = id,
#     imt = "SA(0.3)",
#     PSA = imls,
#     meanLR = meanLRs,
#     covLR = covLRs
#   )
# })
# 
# # Combine into one data frame
# fragility_df <- bind_rows(fragilities)
# 
# structure_types <- c(
#   'RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
#   'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
#   'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
#   'Reinforced masonry','Unreinforced masonry'
# )
# 
# unique(fragility_df$id)
# 
# structure_mapping <- tribble(
#   ~structure_type, ~fragility_id,
#   "RC MRF (1-3 Storeys)",         "CR/LFM+CDL+DUL+VL100/H1/RES",
#   "RC MRF (4-7 Storeys)",         "CR/LFM+CDL+DUL+VL100/H2/RES",
#   "RC MRF (8+ Storeys)",          "CR/LFM+CDL+DUL+VL100/H3/RES",
#   "RC Wall (1-3 Storeys)",        "CR/LFM+CDL+DUM+VL100/H1/RES",
#   "RC Wall (4-7 Storeys)",        "CR/LFM+CDL+DUM+VL100/H2/RES",
#   "RC Wall (8+ Storeys)",         "CR/LFM+CDL+DUM+VL100/H3/RES",
#   "RC Dual system (4-7 Storeys)", "CR/LFM+CDL+DUL+VL100/H2/COM",
#   "RC Dual system (8+ Storeys)",  "CR/LFM+CDL+DUL+VL100/H3/COM",
#   "Other",                        "CR/LFM+CDL+DUL+VL100/H1/IND",
#   "Reinforced masonry",           "MR/LFM+CDL+DUL+VL100/H2/COM",
#   "Unreinforced masonry",         "MUR/LFM+CDN+DNO+VL100/H1/RES"
# )
# 
# matched_fragilities <- fragility_df %>%
#   inner_join(structure_mapping, by = c("id" = "fragility_id"))
# 
# matched_fragilities
# 
# plot(fragilities[[1]]$PSA, fragilities[[1]]$meanLR, xlim=c(0,3))
# 
# # Prior fragility curves for different building types
# # Model:                P(Slight Damage) = LogNormal(PGV | Mu, Sigma)
# # Prior distributions:  Mu ~ Normal(column1, column2), Sigma ~ Normal(column3, column4) 
# prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.25,0.4,0.6,0.05), #c(-0.25,0.25,0.55,0.05), 
#                                   'RC MRF (4-7 Storeys)' = c(-1.125,0.4,0.625,0.05), #c(-1.25,0.25,0.6,0.05), 
#                                   'RC MRF (8+ Storeys)' = c(-1.6,0.4,0.6,0.05), 
#                                   'RC Wall (1-3 Storeys)' = c(0.4,0.5,0.6,0.05), 
#                                   'RC Wall (4-7 Storeys)' = c(0.2,0.5,0.6,0.05), 
#                                   'RC Wall (8+ Storeys)' = c(0,0.5,0.6,0.05),
#                                   'RC Dual system (4-7 Storeys)' = c(-0.5,0.75,0.6,0.05),
#                                   'RC Dual system (8+ Storeys)' = c(-0.5,0.75,0.6,0.05), 
#                                   'Other' = c(-0.5,0.75,0.6,0.05),
#                                   'Reinforced masonry' = c(-1.2,0.2,0.6,0.05),
#                                   'Unreinforced masonry' = c(-1.5,0.1,0.6,0.05))
# 
# prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]
# 
# 
# colnames(prior_frag_structure_type) = c('mu_mean', 'mu_sd', 'sigma_mean', 'sigma_sd')
# 
# psa_vals <- seq(0.01, 2.5, by = 0.01)
# 
# # Assume prior_frag_structure_type has structure_type rownames
# prior_frag_structure_type <- as.data.frame(prior_frag_structure_type)
# prior_frag_structure_type$structure_type <- rownames(prior_frag_structure_type)
# 
# # Generate 100 samples of fragility curves for each structure type
# set.seed(123)
# prior_ribbon_data <- prior_frag_structure_type %>%
#   rowwise() %>%
#   mutate(curves = list({
#     replicate(100, {
#       mu <- rnorm(1, mu_mean, mu_sd)
#       sigma <- rnorm(1, sigma_mean, sigma_sd)
#       plnorm(psa_vals, mu, sigma)
#     }, simplify = "matrix") %>%
#       as.data.frame() %>%
#       mutate(PSA = psa_vals) %>%
#       pivot_longer(-PSA, names_to = "sample", values_to = "P_Damage")
#   })) %>%
#   dplyr::select(structure_type, curves) %>%
#   unnest(curves) %>%
#   group_by(structure_type, PSA) %>%
#   summarise(
#     lower = quantile(P_Damage, 0.025),
#     median = quantile(P_Damage, 0.5),
#     upper = quantile(P_Damage, 0.975),
#     .groups = "drop"
#   )
# 
# # Reference curves (optional: overlay as lines)
# ref_curves <- tribble(
#   ~structure_type, ~mu, ~sigma, ~color,
#   "RC MRF (1-3 Storeys)",  0.25873007, 0.4868709, "red",
#   "RC MRF (1-3 Storeys)", -0.76894697, 0.6728247, "darkgreen",
#   "RC MRF (4-7 Storeys)", -1.50332700, 0.5926148, "orange",
#   "RC Wall (4-7 Storeys)",  0.20096261, 0.6306427, "blue",
#   "RC Wall (4-7 Storeys)",  0.08368013, 0.6599120, "purple"
# ) %>%
#   mutate(
#     curve = map2(mu, sigma, ~ tibble(PSA = psa_vals, P_Damage = plnorm(psa_vals, .x, .y)))
#   ) %>%
#   unnest(curve)
# 
# prior_ribbon_data <- prior_ribbon_data %>%
#   mutate(structure_type = factor(structure_type, levels = levels(field_data$structure_type)))
# 
# ref_curves <- ref_curves %>%
#   mutate(structure_type = factor(structure_type, levels = levels(field_data$structure_type)))
# 
# # Plot with ribbons
# ggplot(prior_ribbon_data, aes(x = PSA)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "skyblue", alpha = 0.4) +
#   geom_line(aes(y = median), color = "skyblue", linewidth = 0.8) +
#   geom_line(data = ref_curves, aes(x = PSA, y = P_Damage, color = color), size = 1, inherit.aes = FALSE) +
#   scale_color_identity() +
#   facet_wrap(~structure_type, ncol = 4) +
#   labs(x = "SA[T=0.3] (g)", y = "P(Damage)") +
#   theme_minimal(base_family = "Times New Roman") +
#   theme(
#     strip.text = element_text(size = 10, face = "bold"),
#     panel.border = element_rect(color = "black", fill = NA),
#     axis.ticks = element_line(),
#     axis.ticks.length = unit(0.15, "cm")
#   )
# 
# 
# 
# 
# 
# 
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center", legend=c("RC, Infilled frame, High code, \nModerate ductility, 1 storey ", 
#                           "RC, Infilled frame, High code, \nModerate ductility, 3 storey", 
#                           "RC, Infilled frame, High code, \nModerate ductility, 6 storey", 
#                           "RC, Wall, High code, \nHigh ductility, 7 storey RES", 
#                           "RC, Wall, High code, \nModerate ductility, 7 storeys RES"),
#        col=c("red", "darkgreen", "orange", "blue", "purple"), lty=1, lwd=2, cex=1.2, bty="n", 
#        y.intersp = 2)
# 
# par(mfrow=c(1,1))
# par(mfrow=c(1,1))
# #PriorFragCurves.png, 1500 x 750
# 
# 
# building_types_survey_include = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)',
#                                   'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
#                                   'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)')
# 
# 
# 
# prior_frag_structure_type_dest = prior_frag_structure_type
# prior_frag_structure_type_dest[,1] = prior_frag_structure_type_dest[,1] + 1.5
# 
# prior_dist_frag = 'rnorm'
# par(mfrow=c(3,4), mar=c(5.1, 4.1, 4.1, 2.1))
# for (i in 1:NROW(prior_frag_structure_type)){
#   plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type_dest)[i], xlab='PGV', ylab='P(Damage)')
#   for (j in 1:50){
#     mu = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type_dest[i,1]), as.numeric(prior_frag_structure_type_dest[i,2]))))
#     sigma = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type_dest[i,3]), as.numeric(prior_frag_structure_type_dest[i,4]))))
#     lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma), col='skyblue')
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #---------------------------
# # Fragility curve prior data:
# #---------------------------
# 
# field_data$PGA_mean_exp = exp(field_data$PGA_mean)
# field_data$SA.3_exp = exp(field_data$`SA(0.3)_mean`) 
# field_data$SA1_exp = exp(field_data$`SA(1.0)_mean`) 
# 
# lm_PGA_PGV = lm(PGV_mean ~ PGA_mean_exp-1, data=field_data)
# lm_SA.3_PGV = lm(PGV_mean ~ SA.3_exp-1, data=field_data)
# lm_SA1_PGV = lm(PGV_mean ~ SA1_exp-1, data=field_data)
# 
# par(mfrow=c(1,3))
# plot(field_data$PGA_mean_exp, field_data$PGV_mean, xlab='PGA', ylab='PGV')
# lines(seq(0, 1.5, 0.01), predict(lm_PGA_PGV, newdata=data.frame(PGA_mean_exp=seq(0, 1.5, 0.01))))
# plot(field_data$SA.3_exp, field_data$PGV_mean, xlab='SA(0.3s)', ylab='PGV')
# lines(seq(0, 2, 0.01), predict(lm_SA.3_PGV, newdata=data.frame(SA.3_exp=seq(0, 2, 0.01))))
# plot(field_data$SA1_exp, field_data$PGV_mean, xlab='SA(1.0s)', ylab='PGV')
# lines(seq(0, 2, 0.01), predict(lm_SA1_PGV, newdata=data.frame(SA1_exp=seq(0, 2, 0.01))))
# par(mfrow=c(1,1))
# 
# frag_prev <- read.xlsx(paste0(dir, 'Data/Jaiswal_Turkiye_fragilities.xlsx'))
# 
# 
# prior_store = data.frame(structure_type=character(),
#                          plnorm_mu = numeric(),
#                          plnorm_sigma = numeric())
# 
# par(mfrow=c(2,3))
# for (i in seq(1, 20,4)){
#   plot(0, 0, xlab='PGV', ylab=paste0('p damage'), main=frag_prev[i,1], xlim=c(0, 2), ylim=c(0,1))
#   cols = c('gold', 'orange', 'red', 'black')
#   for (j in 0:3){
#     row_interest = i + j
#     dam_seq = frag_prev[row_interest,4:NCOL(frag_prev)]
#     IMT_vals <- as.numeric(names(dam_seq))
#     IMT_name = frag_prev$IMT[row_interest]
#     
#     # Choose the correct model based on IMT_name
#     if (IMT_name == "PGA") {
#       PGV_pred <- predict(lm_PGA_PGV, newdata = data.frame(PGA_mean_exp = IMT_vals))
#     } else if (IMT_name == "SA(0.3s)") {
#       PGV_pred <- predict(lm_SA.3_PGV, newdata = setNames(data.frame(IMT_vals), "SA.3_exp"))
#     } else if (IMT_name == "SA(1.0s)") {
#       PGV_pred <- predict(lm_SA1_PGV, newdata = setNames(data.frame(IMT_vals), "SA1_exp"))
#     } else {
#       stop("Unsupported IMT type: ", IMT_name)
#     }
#     
#     probs = as.numeric(dam_seq)
#     lines(PGV_pred, probs, xlab='PGV', ylab=paste0('p(',frag_prev[row_interest,3], ' damage)'), main=frag_prev[row_interest,1], xlim=c(3, 5), col=cols[j+1], lwd=2)
#     
#     if (j != 0) next 
#     
#     objective_fn <- function(params) {
#       meanlog <- params[1]
#       sdlog <- params[2]
#       fit <- plnorm(PGV_pred, meanlog = meanlog, sdlog = sdlog)
#       sum((fit - probs)^2)
#     }
#     
#     result <- optim(c(meanlog = 0.5, sdlog = 0.1), objective_fn, method = "L-BFGS-B", 
#                     lower = c(-Inf, 1e-6))
#     
#     best_params <- result$par
#     meanlog_fit <- best_params[1]
#     sdlog_fit <- best_params[2]
#     lines(PGV_pred, plnorm(PGV_pred, meanlog_fit, sdlog_fit), col = "blue", lwd = 2, lty=2)
#     
#     prior_store %<>% add_row(structure_type=frag_prev[row_interest,1],
#                              plnorm_mu=meanlog_fit,
#                              plnorm_sigma=sdlog_fit)
#     #legend("bottomright", legend = c("Original", "plnorm Fit"), col = c("blue", "red"), lwd = 2)
#     
#     
#   }
# }
# 
# 
# #---------------------------
# # Building type prior data:
# #---------------------------
# 
# build_type_by_city <- read.xlsx(paste0(dir,'Data/building_type_by_city.xlsx'))
# build_type_by_city <- build_type_by_city[-which(build_type_by_city$Row.Labels =='Grand Total'),]
# build_type_by_city <- build_type_by_city[,-which(names(build_type_by_city) =='Grand.Total')]
# 
# add_building_category = function(df){
#   df %<>%
#     mutate(Building_Type = Building_Type %>%
#              str_replace_all("\u00a0", "")) %>%
#     mutate(building_category = case_when(
#       
#       # --- RC MRF (moment-resisting frame) ---
#       Building_Type %in% c(
#         "RC.frame.with.briquette./.hollow.concrete.block.infill",
#         "RC.frame.with.clay.brick.masonry.infill",
#         "RC.frame.with.concrete.block.infill"
#       ) ~ "RC MRF or RC Dual System",
#       
#       # --- RC Wall (structural walls as primary lateral system) ---
#       Building_Type %in% c(
#         "Tunnel.form.system",
#         "Unknown.wall.construction",
#         "Other.wall.construction",
#         "Unknown.construction",
#         "Other.frame.construction",
#         "Unknown.frame.construction"
#       ) ~ "RC Wall",
#       
#       # --- RC Dual System (frame + wall system) ---
#       #Building_Type %in% c(
#       # 
#       #   ) ~ "RC Dual System",
#       
#       # --- Unreinforced Masonry ---
#       Building_Type %in% c(
#         "RC.or.timber.frame.with.adobe.infill",
#         "Clay.brick.masonry",
#         "Briquette./.hollow.concrete.block.masonry",
#         "Stone.masonry",
#         "RC.or.timber.frame.with.stone.masonry.infill"
#       ) ~ "Unreinforced Masonry",
#       
#       Building_Type %in% c(
#         
#       ) ~ "Composite",
#       
#       Building_Type %in% c(
#         "Structural.steel.frame",
#         "Prefabricated.structure",
#         "Timber.frame",
#         "Timber.wall",
#         "Adobe"
#       ) ~  "Other",
#       
#       # Fallback
#       TRUE ~ "Unclasssified"
#     ))
#   return(df)
# }
# 
# plot_stacked_bar <- function(build_type_by_city){
#   # df_clean <- build_type_by_city %>%
#   #   filter(Row.Labels != "Grand Total") %>%
#   #   distinct()
#   
#   # Pivot longer
#   
#   df_long <- build_type_by_city %>%
#     pivot_longer(-Row.Labels, names_to = "Building_Type", values_to = "Count") %>%
#     filter(!is.na(Count))  # Optional: drop NA counts
#   
#   df_long %<>% add_building_category()
#   
#   df_long = df_long %>% group_by(Row.Labels, building_category) %>% summarise(Count=sum(Count))
#   
#   
#   df_prop <- df_long %>%
#     group_by(Row.Labels) %>%
#     mutate(Proportion = Count / sum(Count)) %>%
#     ungroup()
#   
#   p1 <- ggplot(df_prop, aes(x = Row.Labels, y = Proportion, fill = building_category)) +
#     geom_bar(stat = "identity") +
#     labs(title = "Proportional Building Types by City",
#          x = "City", y = "Proportion") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   p2 <- ggplot(df_long, aes(x = Row.Labels, y = Count, fill = building_category)) +
#     geom_bar(stat = "identity") +
#     labs(title = "Building Type Counts by City (Stacked)",
#          x = "City", y = "Number of Buildings") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   grid.arrange(p1,p2, nrow=2)
# }
# 
# plot_stacked_bar(build_type_by_city)
# 
# field_data$City %>% unique()
# 
# match_ups = c('Narli' = 'Kahramanmaras',
#               'Pazarcik' = 'Kahramanmaras',
#               'Kahramanmaras' = 'Kahramanmaras',
#               'Turkoglu' = 'Kahramanmaras',
#               'Nurdagi'='Nurdagi',
#               'Hassa' = 'Hassa',
#               'Islahiye' = 'Gaziantep',
#               'Antakya' = 'Antakya',
#               'Kirikhan' = 'Kirikhan',
#               'Iskendurun' = 'Iskendurun')
# 
# 
# build_type_overall <- read.xlsx(paste0(dir, 'Data/building_type_overall.xlsx'))
# build_type_overall$Structure.type.Labels= gsub(" ", ".", build_type_overall$Structure.type.Labels)
# names(build_type_overall)[1] = 'Building_Type'
# total_buildings = build_type_overall[nrow(build_type_overall),2]
# build_type_overall = build_type_overall[-nrow(build_type_overall),]
# build_type_overall %<>% add_building_category()
# 
# build_categories_overall = build_type_overall %>% group_by(building_category) %>% summarise(proportion=sum(Sum.of.BUILDINGS.in.Turkey)/total_buildings)

#----------------------------------------------------------------------------------------------
#----------------------------------- Select priors --------------------------------------------
#----------------------------------------------------------------------------------------------
# 
# print(build_categories_overall)
# 
# prior_probs_structure_type = c('RC MRF (1-3 Storeys)' = 0.25, #should be higher in older city
#                 'RC MRF (4-7 Storeys)' = 0.15, #should be higher in new city
#                 'RC MRF (8+ Storeys)' = 0.1, #should be higher in new city
#                 'RC Wall (1-3 Storeys)' = 0.005, #should be higher in older city
#                 'RC Wall (4-7 Storeys)' = 0.005, #should be higher in new city
#                 'RC Wall (8+ Storeys)' = 0.005,
#                 'RC Dual system (4-7 Storeys)' = 0.015,
#                 'RC Dual system (8+ Storeys)' = 0.015, 
#                 'Other' = 0.05,
#                 'Reinforced masonry' = 0.03,
#                 'Unreinforced masonry' = 0.38)
# 
# 
# buildingtypes_priorprob = prior_probs_structure_type[levels(field_data$structure_type)] * 10
# 
# prior_samps = rdirichlet(10000, buildingtypes_priorprob)
# par(mfrow=c(4,3))
# for (i in 1:length(levels(field_data$structure_type))){
#   hist(prior_samps[,i], xlab='Proportion of building stock', main=levels(field_data$structure_type)[i], freq=F, ylab='', yaxt='n')
# }
# par(mfrow=c(1,1))
# 
# 
# build_categories_overall
# 
# 
# # Prior fragility curves for different building types
# # Model:                P(Slight Damage) = LogNormal(PGV | Mu, Sigma)
# # Prior distributions:  Mu ~ Normal(column1, column2), Sigma ~ Normal(column3, column4) 
# prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.25,0.4,0.6,0.05), #c(-0.25,0.25,0.55,0.05), 
#                                'RC MRF (4-7 Storeys)' = c(-1.125,0.4,0.625,0.05), #c(-1.25,0.25,0.6,0.05), 
#                                'RC MRF (8+ Storeys)' = c(-1.6,0.4,0.6,0.05), 
#                                'RC Wall (1-3 Storeys)' = c(0.4,0.5,0.6,0.05), 
#                                'RC Wall (4-7 Storeys)' = c(0.2,0.5,0.6,0.05), 
#                                'RC Wall (8+ Storeys)' = c(0,0.5,0.6,0.05),
#                                'RC Dual system (4-7 Storeys)' = c(-0.5,0.75,0.6,0.05),
#                                'RC Dual system (8+ Storeys)' = c(-0.5,0.75,0.6,0.05), 
#                                'Other' = c(-0.5,0.75,0.6,0.05),
#                                'Reinforced masonry' = c(-1.2,0.2,0.6,0.05),
#                                'Unreinforced masonry' = c(-1.5,0.1,0.6,0.05))
# 
# prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]
# 
# 
# print(prior_store)
# 
# prior_dist_frag = 'rnorm'
# par(mfrow=c(3,4), mar=c(5.1, 4.1, 4.1, 2.1))
# for (i in 1:NROW(prior_frag_structure_type)){
#   plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type)[i], xlab='PGV', ylab='P(Damage)')
#   for (j in 1:50){
#     mu = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,1]), as.numeric(prior_frag_structure_type[i,2]))))
#     sigma = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,3]), as.numeric(prior_frag_structure_type[i,4]))))
#     lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma), col='skyblue')
#   }
#   if (i == 1){
#     lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.25873007, 0.4868709), col='red', lwd=2) #Reinforced concrete, Infilled frame, High code, Moderate ductility, 1 storey 
#     lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), -0.76894697, 0.6728247), col='darkgreen', lwd=2) #Reinforced concrete, Infilled frame, High code, Moderate ductility, 3 storey
#   } else if (i == 2){
#     lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), -1.50332700, 0.5926148), col='orange', lwd=2) #Reinforced concrete, Infilled frame, High code, Moderate ductility, 6 storey
#   } else if (i == 5){
#     lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.20096261, 0.6306427), col='blue', lwd=2) #Reinforced concrete, Wall, High code, High ductility, 7 storey RES
#     lines(seq(0, 1.5, 0.01), plnorm(seq(0, 1.5, 0.01), 0.08368013, 0.6599120), col='purple', lwd=2) #Reinforced concrete, Wall, High code, Moderate ductility, 7 storeys RES
#   }
# }
# par(mar=c(0, 0, 0, 0))
# plot.new()
# legend("center", legend=c("RC, Infilled frame, High code, \nModerate ductility, 1 storey ", 
#                           "RC, Infilled frame, High code, \nModerate ductility, 3 storey", 
#                           "RC, Infilled frame, High code, \nModerate ductility, 6 storey", 
#                           "RC, Wall, High code, \nHigh ductility, 7 storey RES", 
#                           "RC, Wall, High code, \nModerate ductility, 7 storeys RES"),
#        col=c("red", "darkgreen", "orange", "blue", "purple"), lty=1, lwd=2, cex=1.2, bty="n", 
#        y.intersp = 2)
# 
# par(mfrow=c(1,1))
# par(mfrow=c(1,1))
# #PriorFragCurves.png, 1500 x 750
# 
# #LN_mu_prior = list(dist='rnorm', params=c(-0.5,0.5))
# #LN_sigma_prior = list(dist='rnorm', params=c(0.6,0.05))#list(dist='rgamma', params=c(5,10))
# # plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white')
# # for (i in 1:50){
# #   mu = do.call(get(LN_mu_prior$dist), as.list(c(1,LN_mu_prior$params)))
# #   sigma = do.call(get(LN_sigma_prior$dist), as.list(c(1,LN_sigma_prior$params)))
# #   lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma))
# # }
# 
# building_types_survey_include = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)',
#                                   'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
#                                   'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)')
# 
# mean_PGV_by_city = field_data %>% group_by(City) %>% summarise(mean_PGV=mean(PGV_mean/100))
# 
# 
# prior_frag_structure_type_dest = prior_frag_structure_type
# prior_frag_structure_type_dest[,1] = prior_frag_structure_type_dest[,1] + 1.5
# 
# prior_dist_frag = 'rnorm'
# par(mfrow=c(3,4), mar=c(5.1, 4.1, 4.1, 2.1))
# for (i in 1:NROW(prior_frag_structure_type)){
#   plot(x=0, y=0,xlim=c(0,1.5), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type_dest)[i], xlab='PGV', ylab='P(Damage)')
#   for (j in 1:50){
#     mu = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type_dest[i,1]), as.numeric(prior_frag_structure_type_dest[i,2]))))
#     sigma = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type_dest[i,3]), as.numeric(prior_frag_structure_type_dest[i,4]))))
#     lines(seq(0,1.5,0.01), plnorm(seq(0,1.5,0.01), mu, sigma), col='skyblue')
#   }
# }

#----------------------------------------------------------------------------------------------
#--------------------------------------- Fit Model --------------------------------------------
#----------------------------------------------------------------------------------------------

set.seed(1)
ministrys_df_sampled <- ministrys_df %>%
  group_by(City) %>%
  group_modify(~ slice_sample(.x, n = pmin(nrow(.x), 1000))) %>%
  ungroup()

stan_model_compiled <- stan_model(paste0(dir, "StanModels/FullModel.stan"))


stan_data <- list(
  N_buildingTypes = length(unique(buildingTypes)),
  N_cities = length(unique(cities)),
  N_zones = 4, 
  
  # Field Survey Data:
  N_buildings = NROW(field_data),
  PSA = field_data$`SA(0.3)_mean`,
  damage_flag = ifelse(field_data$GT >= 1, 1, 0),
  buildingTypes = buildingTypes,
  cities = cities,
  buildingtype_counts = buildingtype_counts,
  mean_PSA = mean_PSA_by_city$mean_PSA[match(levels(field_data$City), mean_PSA_by_city$City)],
  
  # Ministrys Data:
  N_buildings_ministrys = NROW(ministrys_df_sampled),
  PSA_ministrys = ministrys_df_sampled$psa03_mean/100,
  cities_ministrys = ministrys_df_sampled$City,
  damage_flag_ministrys = ifelse(ministrys_df_sampled$GT > 0, 1, 0),
  build_dens = ministrys_df_sampled$build_dens,
  
  # Priors:
  prior_frag_structure_type = prior_frag_structure_type,
  prior_frag_URM_dest =  prior_frag_URM_dest,
  buildingtypes_priorprob = aperm(buildingtypes_priorprob, c(2,3,1)),
  zone_priorprob = zone_per_city_probs[,1:4],
  alpha_D = 5,
  alpha_pi = 10,
  alpha_omega=10
  #p_ministrys_unobserved_param= 1900
)

#control = list(adapt_delta = 0.999, max_treedepth = 15)

#options(mc.cores = 1) 

fit <- sampling(
  stan_model_compiled,  # Use the precompiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  pars = c("mu","beta","nu_0","nu_1","kappa_0","kappa_1",
           "sigma_city","sigma_obs","zone_probs","buildingTypeProbs"),
  #cores = 1,
  #control = list(adapt_delta = 0.98),
  seed = 123
)


#mcmc_trace(fit, pars=c('nu_0', 'nu_1'))

pairs(fit, pars=c('beta[10]', 'buildingTypeProbs[1,1,1]'))

saveRDS(fit,paste0(dir, 'StanFits/fullfit', Sys.Date()))

#fit <- readRDS('/home/manderso/Documents/GitHub/TUR2023_02_06/StanFits/fullfit2025-09-10')

write.csv(summary(fit)$summary, paste0(dir, 'PosteriorSummary'))

#----------------------------------------------------------------------------------------------
#----------------------------------- MCMC Diagnostics -----------------------------------------
#----------------------------------------------------------------------------------------------


rhats <- summary(fit)$summary[,"Rhat"]
rhats[which(rhats > 1.01)]
rstan::traceplot(fit, pars=c('nu_0', 'nu_1', 'kappa_0', 'kappa_1'))


samples <- rstan::extract(fit, pars = "p_ministrys_unobserved")$p_ministrys_unobserved


#----------------------------------------------------------------------------------------------
#--------------------------------- Overall param plot -----------------------------------------
#----------------------------------------------------------------------------------------------

#nu_0, nu_1, kappa_0, kappa_1, sigma_city, sigma_obs
library(bayesplot)
mcmc_dens(fit, pars = c('nu_0', 'nu_1', 'kappa_0', 'kappa_1', 'sigma_city', 'sigma_obs'))
summary(fit, pars='zone_probs')

# 3 x 1 pairplots: nu_0 vs nu_1, kappa_0 vs kappa_1, sigma_city vs sigma_obs

set.seed(123)
draws <- as_draws_df(fit) %>% as_tibble()


# --- Colors to match your style ---
fill_colors <- c(
  "Prior"     = "#1FA187", # teal
  "Posterior" = "#440154"  # purple
)

# --- 1) Posterior: take 500 draws from `fit` ---
set.seed(123)
draws <- as_draws_df(fit) %>% as_tibble()


post_500 <- draws %>%
  dplyr::select(any_of(c("nu_0","nu_1","kappa_0","kappa_1","sigma_city","sigma_obs"))) %>%
  slice_sample(n = 500) %>%
  mutate(type = "Posterior")

# --- 2) Priors: sample from the *model you provided* ---
# In your Stan code: nu_0, nu_1 have bounds (0,1) but no priors → Uniform(0,1)
# kappa_0 ~ Uniform(0.5,1), kappa_1 ~ Uniform(30,300)
# log_sigma_city ~ normal(log(0.1), 0.2), log_sigma_obs ~ normal(log(0.2), 0.2)
n <- 500
prior_500 <- tibble(
  nu_0      = rbeta(n, 20, 5),
  nu_1      = rbeta(n, 5, 20),
  kappa_0   = runif(n, 0.5, 1),
  kappa_1   = runif(n, 10, 300),
  sigma_city = rlnorm(n, meanlog = log(0.1), sdlog = 0.2),
  sigma_obs  = rlnorm(n, meanlog = log(0.2), sdlog = 0.2),
  type = "Prior"
)

# (If you later switch to Beta priors from the paper, just replace the two runif() with:)
# nu_0 = rbeta(n, 4, 2),  nu_1 = rbeta(n, 2, 2)

# --- 3) Combine ---
dd <- bind_rows(prior_500, post_500) %>%
  mutate(type = factor(type, levels = c("Prior","Posterior")))

# --- 4) Theme to match your example ---
base_thm <- theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_blank(),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      color = "black",
      linewidth = 0.4
    ),
    legend.text = element_text(size = 12)
  )

# --- 5) Plots ---

p_nu <- ggplot(dd, aes(x = nu_0, y = nu_1, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(nu[0]), y = expression(nu[1])) +
  base_thm

p_kappa <- ggplot(dd, aes(x = kappa_0, y = kappa_1, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(kappa[0]), y = expression(kappa[1])) +
  base_thm

p_sigma <- ggplot(dd, aes(x = sigma_city, y = sigma_obs, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(tau[c]), y = expression(tau[b])) +
  base_thm

# Show:
p_nu; p_kappa; p_sigma

combined_plot <- p_nu + p_kappa + p_sigma +
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Show side by side
combined_plot

# PriorPostSuppParams.pdf, 10 x 3


p_nu + p_kappa + p_sigma + plot_layout(guides = "collect") & theme(legend.position = "bottom")
#PriorPostSuppParams.pdf, 8 x 3.25

#----------------------------------------------------------------------------------------------
#----------------------------------- Posterior Analysis ---------------------------------------
#----------------------------------------------------------------------------------------------
library(MCMCpack)

#Prior vs posterior probability of building types (in a city)
posterior_samples <- as_draws_df(fit)
structure_types <- levels(field_data$structure_type)

prior_samples1 <- rdirichlet(1000, buildingtypes_priorprob[,'Kahramanmaras', 'Residential'] * stan_data$alpha_pi)
prior_samples2 <- rdirichlet(1000, buildingtypes_priorprob[,'Nurdagi', 'Residential']  * stan_data$alpha_pi)

# Sample from Dirichlet prior
#prior_samps <- rdirichlet(10000, buildingtypes_priorprob)
colnames(prior_samples1) <- structure_types
colnames(prior_samples2) <- structure_types
# n_samples = 10000
# zone_means_prior = rdirichlet(n_samples, 10*buildingtypes_priorprob)
# city_means_prior = rdirichlet(n_samples, 10*buildingtypes_priorprob)
# lambda_prior <- runif(n_samples, 0.5, 0.8)#stan_data$lambda  # or sample from its prior
# dirmult_dispersion <- 10#rinvgamma(n_samples, 6, 50)  # adjust to match your prior, or sample from gamma(2, 0.1)

# Storage for buildingTypeProbs (softmax of sampled logits)
#prior_samps <- array(NA, dim = c(n_samples, stan_data$N_buildingTypes))

# for (i in 1:n_samples) {
#   # Mixture mean per Stan model
#   mixture_mean <- lambda_prior[i] * zone_means_prior[i, ] + (1 - lambda_prior[i]) * city_means_prior[i, ]
#   
#   # Equivalent Dirichlet mean scaled by concentration
#   alpha <- dirmult_dispersion * mixture_mean
#   
#   # Logit-normal approximation: sample logits ~ normal(log(alpha), sd)
#   #logit_sd <- 0.5  # Matches Stan model: raw_logits ~ normal(log(alpha), 0.5)
#   #logits <- rnorm(stan_data$N_buildingTypes, mean = log(alpha), sd = logit_sd)
#   
#   # Softmax to map to simplex
#   #probs <- exp(logits) / sum(exp(logits))
#   
#   prior_samps[i, ] <- rdirichlet(1,alpha)
# }
# colnames(prior_samps) <- structure_types

prior_df <- as_tibble(prior_samples1) %>%
  pivot_longer(cols = everything(), names_to = "structure_type", values_to = "value") %>%
  mutate(source = "Prior")

# Extract posterior samples
city_indices <- c("Kahramanmaras" = 3, "Turkoglu" = 4)

posterior_list <- lapply(names(city_indices), function(city_name) {
  city_index <- city_indices[city_name]
  lapply(seq_along(structure_types), function(i) {
    col_name <- grep(paste0("^buildingTypeProbs\\[", city_index, ",1,", i, "\\]"), names(posterior_samples), value = TRUE)
    tibble(
      structure_type = structure_types[i],
      value = posterior_samples[[col_name]],
      source = paste("Posterior - Residential,", city_name)
    )
  }) %>% bind_rows()
}) %>% bind_rows()

plot_df <- bind_rows(prior_df, posterior_list)


fill_colors <- c(
  "Prior" = "#1FA187",      # Teal-green (viridis mid)
  "Posterior - Residential, Kahramanmaras" = "#FDE725FF",  # Bright yellow (viridis high)
  "Posterior - Residential, Turkoglu" = "#440154"  # Deep purple (viridis low)
)

plot_list <- lapply(unique(plot_df$structure_type), function(st) {
  ggplot(filter(plot_df, structure_type == st), aes(x = value, fill = source, color = source)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.4, bins = 40, linewidth = 0.3) +
    labs(
      x = "Proportion of building stock",
      y = "Density",
      fill = NULL,
      title = st
    ) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = fill_colors, guide='none') +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm"),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = c(0.98, 0.02),
      axis.title.y = element_blank(),
      #axis.text.y = element_blank(),
      #axis.ticks.y = element_blank(),
      legend.justification = c(1, 0),
      legend.box = "vertical",
      legend.background = element_rect(
        fill = alpha("white", 0.8),
        color = "black",
        linewidth = 0.4
      ),
      legend.text = element_text(size = 10),
      legend.key.height = unit(1.5, "lines")
    )
})

# Combine plots
combined_plot <- wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot
#PostObsModel.pdf, 8 x 5

combined_plot <- wrap_plots(plot_list, ncol = 3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot
#PostObsModel.pdf, 7.5 x 8 (portrait)

#--------------------- PLOT 2: ------------------------------------

city_indices <- c("Kahramanmaras" = 3, "Narli" = 1)

posterior_list <- lapply(names(city_indices), function(city_name) {
  city_index <- city_indices[city_name]
  lapply(seq_along(structure_types), function(i) {
    col_name <- grep(paste0("^buildingTypeProbs\\[", city_index, ",1,", i, "\\]"), names(posterior_samples), value = TRUE)
    tibble(
      structure_type = structure_types[i],
      value = posterior_samples[[col_name]],
      source = paste("Posterior - Zone 1,", city_name)
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
      "Posterior - Zone 1, Kahramanmaras" = "tomato",
      "Posterior - Zone 1, Narli" = "darkgreen"
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

#--------------------- PLOT 3: ------------------------------------

zone_indices <- c("Zone 1" = 1, "Zone 2" = 2)

posterior_list <- lapply(names(zone_indices), function(zone_name) {
  zone_index <- zone_indices[zone_name]
  lapply(seq_along(structure_types), function(i) {
    col_name <- grep(paste0("^buildingTypeProbs\\[3,", zone_index, ",", i, "\\]"), names(posterior_samples), value = TRUE)
    tibble(
      structure_type = structure_types[i],
      value = posterior_samples[[col_name]],
      source = paste0("Posterior - ", zone_name, ", Kahramanmaras")
    )
  }) %>% bind_rows()
}) %>% bind_rows()

plot_df <- bind_rows(prior_df, posterior_list)


# plot_list <- lapply(unique(plot_df$structure_type), function(st) {
#   ggplot(filter(plot_df, structure_type == st), aes(x = value, fill = source)) +
#     geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 40) +
#     labs(
#       x = "Proportion of building stock",
#       y = "Density",
#       fill = "Source",
#       title = st
#     ) +
#     scale_fill_manual(values = c(
#       "Prior" = "skyblue",
#       "Posterior - Zone 1, Kahramanmaras" = "tomato",
#       "Posterior - Zone 2, Kahramanmaras" = "darkgreen"
#     )) +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(hjust = 0.5, size = 10),
#       axis.title = element_text(size = 9),
#       axis.text = element_text(size = 8),
#       legend.position = "bottom"
#     )
# })
# 
# library(patchwork)
# combined_plot <- wrap_plots(plot_list, ncol = 4) + 
#   plot_layout(guides = "collect") & 
#   theme(legend.position = "bottom")
# combined_plot

custom_fill_colors <- c(
  "Prior" = "#1FA187",
  "Posterior - Zone 1, Kahramanmaras" = "#440154",
  "Posterior - Zone 2, Kahramanmaras" = "#FDE725FF"
)

# Generate the plot list with updated styling
plot_list <- lapply(unique(plot_df$structure_type), function(st) {
  ggplot(filter(plot_df, structure_type == st), aes(x = value, fill = source)) +
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.4, bins = 40, color = "black", linewidth = 0.1) +
    labs(
      x = "Proportion of building stock",
      y = "Density",
      fill = NULL,
      title = st
    ) +
    scale_fill_manual(values = custom_fill_colors) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = "bottom",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    )
})

# Combine the plots
combined_plot <- wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

combined_plot


#---------------------------------------------------------------
#----------------- Prior vs Posterior Fragility curves ---------
#---------------------------------------------------------------

psa_grid <- seq(0, 1.7, 0.01)

prior_dist_frag = 'rnorm'

# Create a long dataframe of prior samples
prior_curves <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
  map_dfr(1:2000, function(j) {
    mu_prior <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 1]), as.numeric(prior_frag_structure_type[i, 2]))))
    sigma_prior <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 3]), as.numeric(prior_frag_structure_type[i, 4]))))
    tibble(
      structure_type = rownames(prior_frag_structure_type)[i],
      PSA = psa_grid,
      prob = plnorm(psa_grid, mu_prior, sigma_prior),
      type = "Prior",
      curve_id = paste0("prior_", i, "_", j)
    )
  })
})


# Posterior samples
posterior_samples <- as_draws_df(fit)


posterior_curves <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
  mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
  beta_post <- pull(posterior_samples[, grep(paste0("^beta\\[", i, "\\]"), names(posterior_samples))])
  
  #mu_dest_post <- pull(posterior_samples[, grep(paste0("^mu_dest\\[", i, "\\]"), names(posterior_samples))])
  #beta_dest_post <- pull(posterior_samples[, grep(paste0("^beta_dest\\[", i, "\\]"), names(posterior_samples))])
  
  map_dfr(1:2000, function(j) {
    mu_post_sample <- sample(mu_post, 1)
    beta_post_sample <- sample(beta_post, 1)
    tibble(
      structure_type = rownames(prior_frag_structure_type)[i],
      PSA = psa_grid,
      prob = plnorm(psa_grid, mu_post_sample, beta_post_sample),
      type = "Posterior",
      curve_id = paste0("post_", i, "_", j)
    )
  })
})

# Survey data
field_data$rounded_PSA <- round(field_data$`SA(0.3)_mean` * 200) / 200
survey_points <- field_data %>%
  group_by(structure_type, rounded_PSA) %>%
  summarise(prop_dam = mean(GT >= 1), Build = n(), .groups = "drop") %>%
  rename(PSA = rounded_PSA)

# Combine curves
all_curves <- bind_rows(prior_curves, posterior_curves)

# Plot
all_curves$type <- factor(all_curves$type, levels = c("Prior", "Posterior"))
all_curves$structure_type <- factor(all_curves$structure_type, levels = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                                                                          'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                                                                          'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)',
                                                                          'Reinforced masonry','Unreinforced masonry','Other'))

curve_summary <- all_curves %>%
  group_by(structure_type, type, PSA) %>%
  summarise(
    lower = quantile(prob, 0.025, na.rm = TRUE),
    median = median(prob, na.rm = TRUE),
    upper = quantile(prob, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

fill_colors <- c(
  "Prior" = "#1FA187",      # Teal
  "Posterior" = "#440154" # Yellow
)

ggplot() +
  # Ribbons for uncertainty
  geom_ribbon(
    data = curve_summary,
    aes(
      x = PSA, ymin = lower, ymax = upper,
      fill = type, color = type
    ),
    alpha = 0.3,
    linewidth=0.1
  ) +
  # Median (or mean) dashed lines
  geom_line(
    data = curve_summary,
    aes(
      x = PSA, y = median,
      color = type
    ),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  # Survey points
  geom_point(
    data = survey_points,
    aes(x = PSA, y = prop_dam, size = Build),
    color = "black", alpha = 0.7
  ) +
  facet_wrap(~ structure_type, scales = "free") +
  scale_fill_manual(values = fill_colors, name = NULL) +
  scale_color_manual(values = fill_colors, name = NULL, guide = "none") +
  scale_size_continuous(range = c(1, 6)) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(
    x = "PSA[T=0.3s] (g)",
    y = "Probability of Damage",
    title = "Fragility Curves with 95% Intervals: Prior vs Posterior"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_blank(), #element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = c(0.99, 0.005),
    legend.justification = c(1, 0),
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.4
    ),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "lines")
  )+ guides(
    size = guide_legend(
      title = "Sample size",
      nrow = 3,
      byrow = TRUE
    )
  )

#PriorVsPosteriorFrag.pdf, 10 x 5.5


ggplot() +
  # Ribbons for uncertainty
  geom_ribbon(
    data = curve_summary,
    aes(
      x = PSA, ymin = lower, ymax = upper,
      fill = type, color = type
    ),
    alpha = 0.3,
    linewidth=0.1
  ) +
  # Median (or mean) dashed lines
  geom_line(
    data = curve_summary,
    aes(
      x = PSA, y = median,
      color = type
    ),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  # Survey points
  geom_point(
    data = survey_points,
    aes(x = PSA, y = prop_dam, size = Build),
    color = "black", alpha = 0.7
  ) +
  facet_wrap(~ structure_type, nrow=4, scales = "free") +
  scale_fill_manual(values = fill_colors, name = NULL) +
  scale_color_manual(values = fill_colors, name = NULL, guide = "none") +
  scale_size_continuous(range = c(1, 6)) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(
    x = "PSA[T=0.3s] (g)",
    y = "Probability of Damage",
    title = "Fragility Curves with 95% Intervals: Prior vs Posterior"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_blank(), #element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.position = c(0.99, 0.005),
    legend.justification = c(1, 0),
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = alpha("white", 0.8),
      color = "black",
      linewidth = 0.4
    ),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.5, "lines")
  )+ guides(
    size = guide_legend(
      title = "Sample size",
      nrow = 3,
      byrow = TRUE
    )
  )
#PriorVsPosteriorFrag.pdf, 7.5 x 8 (portrait)



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
  beta_post <- pull(posterior_samples[, grep(paste0("^beta\\[", i, "\\]"), names(posterior_samples))])
  
  for (j in 1:50){
    mu_post_sample = sample(mu_post, 1)
    beta_post_sample = sample(beta_post, 1)
    lines(seq(0,1.7,0.01), plnorm(seq(0,1.7,0.01), mu_post_sample, beta_post_sample), col='red')
  }
  survey_dat = field_data %>% filter(structure_type == levels(field_data$structure_type)[i]) %>% group_by(rounded_PGV) %>% summarise(prop_dam = mean(GT >=1), Build = n())
  for (j in 1:NROW(survey_dat)){
    points(survey_dat[j,1], survey_dat[j,2], pch=19, cex=as.numeric(log(survey_dat[j,3]+1)))
  }
}
par(mfrow=c(1,1))






#------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------- Posterior predictive checks---------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------

# 
# field_data$rounded_PGV = round(field_data$PGV_mean * 2) / 200
# 
# #Prior vs Posterior fragility curves
# prior_dist_frag = 'rnorm'
# 
# posterior_samples <- as_draws_df(fit)
# 
# field_data_with_preds = data.frame()
# post_samples_i = sample(1:NROW(posterior_samples), 100)
# for (j in 1:stan_data$N_cities){
#   survey_data_city = field_data %>% filter(City == levels(field_data$City)[j])
#   survey_data_city_grouped = survey_data_city %>% group_by(rounded_PGV, structure_type) %>% summarise(prop_dam = mean(GT >=1), Build = n())
#   survey_data_city_grouped[, paste0('sampled.', 1:length(post_samples_i))] = 0
#   survey_data_city_grouped$min_samp = NA
#   survey_data_city_grouped$max_samp = NA
#   survey_data_city_grouped$median_samp = NA
#   eps_city_error = pull(posterior_samples[paste0('eps_city_error')])
#   error_samp = rnorm(length(post_samples_i), 0, eps_city_error[post_samples_i])
#   for (i in 1:NROW(survey_data_city_grouped)){
#     k = match(survey_data_city_grouped$structure_type[i], levels(field_data$structure_type))
#     plnorm_means =  pull(posterior_samples[paste0('mu[',k,']')])[post_samples_i]
#     plnorm_stds =  pull(posterior_samples[paste0('sigma[',k,']')])[post_samples_i]
#     dam_probs = plnorm(survey_data_city_grouped$rounded_PGV[i], plnorm_means + error_samp, plnorm_stds)
#     obs_samples = rbinom(length(dam_probs), survey_data_city_grouped$Build[i], prob=dam_probs)
#     survey_data_city_grouped[i, grep('sampled.',names(survey_data_city_grouped))] = t(obs_samples)
#     survey_data_city_grouped$min_samp[i] = quantile(obs_samples, 0.05)/survey_data_city_grouped$Build[i]
#     survey_data_city_grouped$max_samp[i] = quantile(obs_samples, 0.95)/survey_data_city_grouped$Build[i]
#     survey_data_city_grouped$median_samp[i] = median(obs_samples)/survey_data_city_grouped$Build[i]
#   }
#   survey_data_city_grouped$City = levels(field_data$City)[j]
#   field_data_with_preds %<>% rbind(survey_data_city_grouped)
# }
# field_data_with_preds$obs_count = field_data_with_preds$Build * field_data_with_preds$prop_dam
# 
# par(mfrow=c(3,4))
# field_data_with_preds_grouped = group_by(field_data_with_preds,structure_type) %>% group_split()
# for (i in 1:length(field_data_with_preds_grouped)){
#   dat = field_data_with_preds_grouped[[i]]
#   plot(x=0, y=0, col='white',xlim=c(0, 2), ylim=c(0, 1), xlab='PGV', ylab='Proportion damaged', main=dat$structure_type[1])
#   for (j in 1:NROW(dat)){
#     arrows(dat$rounded_PGV[j],dat$min_samp[j], dat$rounded_PGV[j], dat$max_samp[j], length=0.05, angle=90, code=3, col='red')
#     points(dat$rounded_PGV[j], dat$median_samp[j], col='red')
#     points(dat$rounded_PGV[j], dat$prop_dam[j], pch=19)
#   }
# }
# 
# row1=11
# row2=13
# plot(t(field_data_with_preds[row1,grep('sampled.', names(field_data_with_preds))]),t(field_data_with_preds[row2,grep('sampled.', names(field_data_with_preds))]))
# points(field_data_with_preds$obs_count[row1], field_data_with_preds$obs_count[row2], col='red', pch=19)
# 
# #Need to fix
# pairplot_regions <- function(dam_counts){
#   
#   sampled_df <- (dam_counts %>%
#                    dplyr::select(polygon_name, starts_with("sampled.")) %>%
#                    pivot_longer(cols = -polygon_name, names_to = "sample", values_to = "value") %>%
#                    pivot_wider(names_from = polygon_name, values_from = value) %>%
#                    mutate(type = "Sampled"))[,-1]
#   
#   sampled_df[1:(ncol(sampled_df)-1)] <- lapply(sampled_df[1:(ncol(sampled_df)-1)], as.numeric)
#   
#   # Step 2: Get observed values, same format
#   observed_row <- impact_mort %>%
#     dplyr::select(polygon_name, observed) %>%
#     pivot_wider(names_from = polygon_name, values_from = observed) %>%
#     mutate(type = "Observed")
#   
#   # Step 3: Ensure column types match
#   # Make sure all except `type` are numeric
#   observed_row[1:(ncol(observed_row)-1)] <- lapply(observed_row[1:(ncol(observed_row)-1)], as.numeric)
#   
#   # Step 4: Combine
#   plot_df <- bind_rows(sampled_df, observed_row)
#   
#   obs_vals <- filter(plot_df, type == "Observed")
#   
#   # Function to overlay observed points
#   overlay_observed <- function(data, mapping, ...) {
#     ggplot(data = data, mapping = mapping) +
#       geom_point(alpha = 0.7, size = 0.5, color = "#440154", ...) +
#       geom_point(data = obs_vals, mapping = mapping, color = "red", size = 3, shape=4, stroke = 1.5)
#   }
#   
#   gpairs_lower <- function(g){
#     g$plots <- g$plots[-(1:g$nrow)]
#     g$yAxisLabels <- g$yAxisLabels[-1]
#     g$nrow <- g$nrow -1
#     
#     g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
#     g$xAxisLabels <- g$xAxisLabels[-g$ncol]
#     g$ncol <- g$ncol - 1
#     
#     g
#   }
#   
#   # Pair plot with custom overlay
#   g <- ggpairs(plot_df %>% dplyr::select(-type),
#                upper = list(continuous = "blank"),
#                lower = list(continuous = overlay_observed),
#                diag = list(continuous = "blankDiag"), 
#                showStrips=T
#   ) +
#     theme_minimal() +
#     theme(strip.text = element_text(size = 10),
#           panel.border = element_rect(color = "black", fill = NA, linewidth = 1)) + 
#     scale_x_continuous(
#       trans = scales::pseudo_log_trans(sigma = 0.5, base = 10),
#       breaks = c(0, 10, 100, 1000),
#       labels = scales::comma_format(),
#       minor_breaks = NULL
#     ) + 
#     scale_y_continuous(
#       trans = scales::pseudo_log_trans(sigma = 0.5, base = 10),
#       breaks = c(0, 10, 100, 1000),
#       labels = scales::comma_format(),
#       minor_breaks = NULL
#     )
#   
#   gpairs_lower(g)
# }
# 
# 
# 
# for (i in 1:NROW(prior_frag_structure_type)){
#   survey_dat = field_data %>% filter(structure_type == levels(field_data$structure_type)[i]) %>% group_by(rounded_PGV) %>% summarise(prop_dam = mean(GT >=1), Build = n())
#   
#   
#   plot(x=0, y=0,xlim=c(0,1.7), ylim=c(0,1), col='white', main=rownames(prior_frag_structure_type)[i], xlab='PGV', ylab='P(Damage)')
#   for (j in 1:50){
#     mu_prior = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,1]), as.numeric(prior_frag_structure_type[i,2]))))
#     sigma_prior = do.call(get(prior_dist_frag), as.list(c(1,as.numeric(prior_frag_structure_type[i,3]), as.numeric(prior_frag_structure_type[i,4]))))
#     lines(seq(0,1.7,0.01), plnorm(seq(0,1.7,0.01), mu_prior, sigma_prior), col='blue')
#   }
#   mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
#   sigma_post <- pull(posterior_samples[, grep(paste0("^sigma\\[", i, "\\]"), names(posterior_samples))])
#   
#   for (j in 1:50){
#     mu_post_sample = sample(mu_post, 1)
#     sigma_post_sample = sample(sigma_post, 1)
#     lines(seq(0,1.7,0.01), plnorm(seq(0,1.7,0.01), mu_post_sample, sigma_post_sample), col='red')
#   }
#   
#   for (j in 1:NROW(survey_dat)){
#     points(survey_dat[j,1], survey_dat[j,2], pch=19, cex=as.numeric(log(survey_dat[j,3]+1)))
#   }
# }
# par(mfrow=c(1,1))
# 
# #-------------------------------------------------------------------------------------------
# #---------------- Check posterior building type by city vs true ----------------------------
# #-------------------------------------------------------------------------------------------
# 
# type_match =  c('RC MRF (1-3 Storeys)' = 'RC MRF or RC Dual System', #should be higher in older city
#                 'RC MRF (4-7 Storeys)' = 'RC MRF or RC Dual System', #should be higher in new city
#                 'RC MRF (8+ Storeys)' = 'RC MRF or RC Dual System', #should be higher in new city
#                 'RC Wall (1-3 Storeys)' = 'RC Wall', #should be higher in older city
#                 'RC Wall (4-7 Storeys)' = 'RC Wall', #should be higher in new city
#                 'RC Wall (8+ Storeys)' = 'RC Wall',
#                 'RC Dual system (4-7 Storeys)' = 'RC MRF or RC Dual System',
#                 'RC Dual system (8+ Storeys)' = 'RC MRF or RC Dual System', 
#                 'Other' = 'Other',
#                 'Reinforced masonry' = 'Other',
#                 'Unreinforced masonry' = 'Unreinforced Masonry')
# 
# type_match_df = data.frame(Type_Index = 1:length(type_match),
#                            building_type = names(type_match),
#                            building_category = c(type_match))
# 
# city_match_df = data.frame(City_Index = 1:length(levels(field_data$City)),
#                            City = levels(field_data$City))
# 
# 
# df_long <- build_type_by_city %>%
#   pivot_longer(-Row.Labels, names_to = "Building_Type", values_to = "Count") %>%
#   filter(!is.na(Count))  # Optional: drop NA counts
# 
# df_long %<>% add_building_category()
# 
# 
# df_percent <- df_long %>%
#   group_by(building_category, Row.Labels) %>%
#   summarise(Count_New = sum(Count)) %>%
#   group_by(Row.Labels) %>%
#   reframe(building_category=building_category, Percent = Count_New / sum(Count_New) * 100)
# 
# posterior_df <- as_draws_df(fit) %>%
#   dplyr::select(starts_with("p_buildingtype"))
# 
# # Pivot longer: assuming cities are indexed i and building types j in Stan
# posterior_long <- posterior_df %>%
#   mutate(.draw = row_number()) %>%  # Adds draw index
#   pivot_longer(cols = starts_with("p_buildingtype"), names_to = "name", values_to = "value") %>%
#   tidyr::extract(name, into = c("city", "type"), regex = "p_buildingtype\\[(\\d+),(\\d+)\\]", convert = TRUE) %>%
#   rename(City_Index = city, Type_Index = type)
# 
# posterior_long_with_info <- posterior_long %>%
#   left_join(type_match_df, by = "Type_Index") %>%
#   left_join(city_match_df, by = "City_Index")
# 
# # Now, sum draws by City and building_category
# posterior_draws_summed <- posterior_long_with_info %>%
#   group_by(.draw, City, building_category) %>%
#   summarise(value = sum(value), .groups = "drop")
# 
# # Then, summarize
# posterior_summary_categories <- posterior_draws_summed %>%
#   group_by(City, building_category) %>%
#   summarise(
#     p_mean = mean(value),
#     p_lower = quantile(value, 0.025),
#     p_upper = quantile(value, 0.975),
#     .groups = "drop"
#   )
# 
# # Summarize posterior means
# # posterior_summary <- posterior_long %>%
# #   group_by(City_Index, Type_Index) %>%
# #   summarise(
# #     p_mean = mean(value),
# #     p_lower = quantile(value, 0.025),
# #     p_upper = quantile(value, 0.975),
# #     .groups = "drop"
# #   )
# 
# # posterior_summary_full = posterior_summary %>% merge(city_match_df, by='City_Index') %>% merge(type_match_df, by='Type_Index')
# 
# # posterior_summary_categories <- posterior_summary_full %>%
# #   group_by(City, building_category) %>%
# #   summarise(
# #     p_mean = sum(p_mean),
# #     p_lower = sum(p_lower),
# #     p_upper = sum(p_upper),
# #     .groups = "drop"
# #   )
# 
# df_percent_clean <- df_percent %>%
#   rename(City = `Row.Labels`) %>%
#   mutate(
#     City = str_trim(str_to_upper(City)),
#     building_category = str_trim(str_to_lower(building_category)),
#     p_mean = Percent / 100,
#     Source = "Observed"
#   ) %>%
#   dplyr::select(City, building_category, p_mean, Source)
# 
# df_posterior_clean <- posterior_summary_categories %>%
#   mutate(
#     City = str_trim(str_to_upper(City)),
#     building_category = str_trim(str_to_lower(building_category)),
#     Source = "Posterior"
#   )
# 
# df_posterior_clean$City[df_posterior_clean$City=="NURDAƑÛI"] = "NURDAGI"
# df_posterior_clean$City[df_posterior_clean$City=="ISKENDURUN"] = "ISKENDERUN"
# df_posterior_clean$City[df_posterior_clean$City=="ISLAHIYE"] = "GAZIANTEP"
# 
# # Keep only matching combinations
# combined_df <- df_percent_clean %>%
#   inner_join(df_posterior_clean, by = c("City", "building_category")) %>%
#   rename(p_mean_obs = p_mean.x, p_mean_post = p_mean.y) %>%
#   pivot_longer(
#     cols = starts_with("p_mean"),
#     names_to = "Source",
#     values_to = "p_mean",
#     names_prefix = "p_mean_"
#   ) %>%
#   mutate(
#     p_lower = if_else(Source == "post", p_lower, NA_real_),
#     p_upper = if_else(Source == "post", p_upper, NA_real_)
#   )
# 
# # Plot
# ggplot(combined_df, aes(x = City, y = p_mean, fill = Source, group = Source)) +
#   geom_col(position = position_dodge2(width = -0.5), width = 0.7) +
#   geom_errorbar(
#     data = subset(combined_df, Source == "post"),
#     aes(ymin = p_lower, ymax = p_upper),
#     position = position_dodge2(width = 10),
#     width = 0.2
#   ) +
#   facet_wrap(~building_category, scales = "free") +
#   scale_fill_manual(
#     values = c("post" = "tomato", "obs" = "skyblue"),
#     labels = c("post" = "Posterior", "obs" = "Observed")
#   ) +
#   labs(
#     title = "Observed vs Posterior Building Category Proportions by Building Type",
#     x = "City",
#     y = "Proportion",
#     fill = "Source"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ylim(0, 1)
# 
# #------------------------------------------------------------------------------------------------------------------------------------
# 
# field_data$rounded_PGV = round(field_data$PGV_mean) / 100
# plot_df = field_data %>% filter(structure_type == 'Unreinforced masonry') %>% group_by(rounded_PGV, City) %>% summarise(prop_dam = mean(GT >=1), nBuild = n())
# 
# ggplot(plot_df, aes(x=rounded_PGV, y=prop_dam, size=nBuild, color=City)) + geom_point() + xlim(0, 1.6)
# 
# # x_vals <- seq(0, 1.5, 0.01)
# # n_samples <- 50
# # 
# # # Sample from priors
# # prior_df <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
# #   map_dfr(1:n_samples, function(j) {
# #     mu <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 1]), as.numeric(prior_frag_structure_type[i, 2]))))
# #     sigma <- do.call(get(prior_dist_frag), as.list(c(1, as.numeric(prior_frag_structure_type[i, 3]), as.numeric(prior_frag_structure_type[i, 4]))))
# #     tibble(
# #       x = x_vals,
# #       y = plnorm(x_vals, meanlog = mu, sdlog = sigma),
# #       group = j,
# #       type = "Prior",
# #       structure = rownames(prior_frag_structure_type)[i]
# #     )
# #   })
# # })
# # 
# # # Sample from posterior
# # posterior_samples <- as_draws_df(fit)
# # 
# # posterior_df <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
# #   mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
# #   sigma_post <- pull(posterior_samples[, grep(paste0("^sigma\\[", i, "\\]"), names(posterior_samples))])
# #   
# #   map_dfr(1:n_samples, function(j) {
# #     mu <- sample(mu_post, 1)
# #     sigma <- sample(sigma_post, 1)
# #     tibble(
# #       x = x_vals,
# #       y = plnorm(x_vals, meanlog = mu, sdlog = sigma),
# #       group = j,
# #       type = "Posterior",
# #       structure = rownames(prior_frag_structure_type)[i]
# #     )
# #   })
# # })
# # 
# # # Combine both
# # full_df <- bind_rows(prior_df, posterior_df)
# # 
# # # Plot
# # ggplot(full_df, aes(x = x, y = y, group = interaction(type, group), color = type)) +
# #   geom_line(alpha = 0.4) +
# #   facet_wrap(~structure) +
# #   labs(x = "PGV", y = "P(Damage)", color = "Distribution Type") +
# #   scale_color_manual(values = c("Prior" = "blue", "Posterior" = "red")) +
# #   theme_minimal() +
# #   theme(panel.spacing = unit(4, "lines"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# kahranmaras_df_sample$PGV_mean[1]/100
# kahranmaras_df_sample$GT
# 
# 
# traceplot(fit, pars='mu')
# 
# for (i in 1:11){
#   plot_df_name <- paste0("plot_df_structure", i)
#   assign(plot_df_name, generate_plot_df(fit, pars=c(paste0('mu[',i,']'),paste0('sigma[',i,']'))))
# }
# levels(field_data$structure_type)
# (field_data %>% filter(structure_type==levels(field_data$structure_type)[1]))[,c('structure_type', 'PGA_mean', 'GT')]
# (field_data %>% filter(structure_type==levels(field_data$structure_type)[3]))[,c('structure_type', 'PGA_mean', 'GT')]
# (field_data %>% filter(structure_type==levels(field_data$structure_type)[4]))[,c('structure_type', 'PGA_mean', 'GT')]
# 
# propDamGroup = field_data %>% group_by(PGV_mean, structure_type) %>% summarise(prop_dam = mean(GT >=1),
#                                                                                nBuild = n()) %>% filter(structure_type %in% c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", 'RC MRF (8+ Storeys)', 'Unreinforced masonry'))
# 
# kahranmaras_df_sample$PGV_mean_rounded = round(kahranmaras_df_sample$PGV_mean) / 100
# propDamMinistrys = kahranmaras_df_sample %>% group_by(PGV_mean_rounded) %>% summarise(prop_dam = mean(GT >=1),
#                                                                        nBuild = n())
# 
# ggplot() +
#   geom_line(data = plot_df_structure3$cdf_samples, aes(x = x, y = prob, group = interaction(mu.3., sigma.3.), 
#                                                        color = "RC MRF (1-3 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
#   geom_line(data = plot_df_structure3$cdf_mean, aes(x = x, y = prob, color = "RC MRF (1-3 Storeys)"), size = 1.5) +
#   geom_line(data = plot_df_structure1$cdf_samples, aes(x = x, y = prob, group = interaction(mu.1., sigma.1.), 
#                                                        color = "RC MRF (4-7 Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
#   geom_line(data = plot_df_structure1$cdf_mean, aes(x = x, y = prob, color = "RC MRF (4-7 Storeys)"), size = 1.5) +
#   geom_line(data = plot_df_structure4$cdf_samples, aes(x = x, y = prob, group = interaction(mu.4., sigma.4.), 
#                                                        color = "RC MRF (8+ Storeys)"), alpha = 0.2) +  # Semi-transparent for uncertainty
#   geom_line(data = plot_df_structure4$cdf_mean, aes(x = x, y = prob, color = "RC MRF (8+ Storeys)"), size = 1.5) +
#   geom_line(data = plot_df_structure7$cdf_samples, aes(x = x, y = prob, group = interaction(mu.7., sigma.7.), 
#                                                        color = "Unreinforced masonry"), alpha = 0.2) +  # Semi-transparent for uncertainty
#   geom_line(data = plot_df_structure7$cdf_mean, aes(x = x, y = prob, color = "Unreinforced masonry"), size = 1.5) +
#   
#   xlab('PGV') + ylab('Probability of Damage') +
#   theme_minimal() +  scale_color_manual(name='',values = c('RC MRF (8+ Storeys)'='blue','RC MRF (4-7 Storeys)'='yellow',  'RC MRF (1-3 Storeys)'='green', 'Unreinforced masonry'='red')) + xlim(0, 1.3) + 
#   geom_point(data=propDamGroup, aes(x=PGV_mean/100, y=prop_dam, size=nBuild, col=structure_type)) +
#   geom_point(data=propDamMinistrys, aes(x=PGV_mean_rounded, y=prop_dam, size=nBuild))
#   
# 
# 
# posterior_samples <- as_draws_df(fit)
# 
# 
# # Subset the columns for building 1
# build_interest = 8
# ministrys_df$GT[build_interest]
# ministrys_df$PGV_mean[build_interest] / 100
# building2_cols <- posterior_samples[, grep(paste0("^buildingtype_probs_ministrys\\[",build_interest,","), names(posterior_samples))]
# 
# building2_cols <- posterior_samples[, grep("^p_buildingtype\\[3,", names(posterior_samples))]
# 
# posterior_means <- colMeans(building2_cols)
# structure_labels <- levels(field_data$structure_type)
# names(posterior_means) <- structure_labels
# 
# df_plot <- tibble(
#   structure_type = factor(names(posterior_means), levels = names(posterior_means)),
#   probability = as.numeric(posterior_means)
# )
# 
# # Plot as a horizontal stacked bar
# ggplot(df_plot, aes(x = 1, y = probability, fill = structure_type)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   coord_flip() +
#   labs(
#     x = NULL,
#     y = "Posterior Probability",
#     fill = "Structure Type",
#     title = "Posterior Building Type Probabilities for Building 2"
#   ) +
#   theme_minimal() +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# 
# 
# 
# 
# 
# stan_data$
#   
#   
# PGV_seq = seq(0, 1.5, 0.01)
# dam_prob = plnorm(PGV_seq,-1.5,0.6)
# collapse_prob = plnorm(PGV_seq, -1.2, 0.6)
# eff_prob = array(0, length(collapse_prob))
# plot(PGV_seq, dam_prob)
# lines(PGV_seq, collapse_prob, col='red')
# 
# for (i in 1:length(eff_prob)){
#   eff_prob[i] = (10000* dam_prob[i] - 10000* collapse_prob[i])/(10000 - 10000* collapse_prob[i])
# }
# 
# plot(PGV_seq, dam_prob, type='l')
# lines(PGV_seq, collapse_prob, col='red')
# lines(PGV_seq, eff_prob, col='blue')
# 
# lines(PGV_seq, plnorm(PGV_seq, -1.5+1, 0.6), col='green')
# 
# 


