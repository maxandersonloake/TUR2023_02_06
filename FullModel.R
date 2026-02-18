dir = '/home/manderso/Documents/GitHub/TUR2023_02_06/'
source(paste0(dir, 'Functions.R'))

# ----------------------------------------------------------------------------------------------
# ----------------------------------- Prepare survey data --------------------------------------
# ----------------------------------------------------------------------------------------------

# Read raw field survey data from Excel
field_data = read.xlsx(paste0(dir, 'Data/Jaiswal_2023TurkiyeEQ_us6000jllz_field_str_damage_data.xlsx'))

# Rename the City/Town column to 'City' for consistency
names(field_data)[which(names(field_data)=='City/Town')] = 'City'

# Create GT (ground truth damage) numeric code from textual damage_condition
# 0 = None, 1 = Minor, 2 = Moderate, 3 = Severe/Partial collapse, 4 = Collapsed
field_data$GT <- ifelse(field_data$damage_condition=='None', 0, 
                        ifelse(field_data$damage_condition=='Minor (few cracks)', 1, 
                               ifelse(field_data$damage_condition=='moderate damage but repaired', 2, 
                                      ifelse(field_data$damage_condition=='Moderate (extensive cracks in walls)', 2, 
                                             ifelse(field_data$damage_condition=='Partial collapse (portion collapsed)', 3, 
                                                    ifelse(field_data$damage_condition=='Severe (structural damage to system)', 3, 4)
                                             )))))

# Remove rows with missing GT or missing structure_type (we can't use them)
field_data %<>% filter(!is.na(GT))
field_data %<>% filter(!is.na(structure_type))

# Normalise structure_type strings (convert factors to trimmed character)
field_data$structure_type %<>% as.character() %>% trimws()

# Replace a specific malformed structure_type string with 'Other'
field_data$structure_type[which(field_data$structure_type=='Column with slab corrugated Asgolan')] = 'Other'

# Define an ordered factor for structure types to control plotting/faceting order
structure_type_ordered = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                           'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                           'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Other',
                           'Reinforced masonry','Unreinforced masonry')
field_data$structure_type %<>% factor(levels = structure_type_ordered)

# Numeric encoding for building types and cities (useful for Stan / index arrays)
buildingTypes  = as.numeric(field_data$structure_type)

field_data$City %<>% as.character()
field_data$City %<>% factor(levels = unique(field_data$City))
cities = as.numeric(field_data$City)

# Table of building type counts per city (used later for sampling or priors)
buildingtype_counts = table(field_data$City, field_data$structure_type)

# Quick aggregated damage proportions by IM and structure type (for exploratory plotting)
dam_props = field_data %>% group_by(`SA(0.3)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())

# Example commented-out ggplot that would show logistic fits by structure type
# p1 <- ggplot(field_data, aes(x = `SA(0.3)_mean`, y = as.numeric(GT >= 1), 
#                              col = as.factor(structure_type))) +
#   geom_jitter(width = 0.02, height = 0.02, alpha = 0.3, size = 1) +
#   stat_smooth(method = "glm", method.args = list(family = "binomial"),
#               se = FALSE, fullrange = TRUE) +
#   labs(y = "Probability of Damage", x = "SA(0.3)", 
#        color = "Structure Type") +
#   theme_minimal()
# p1

# Prepare colors for plotting by structure type
structure_types <- unique(field_data$structure_type)
set.seed(1)  # reproducible color sampling
colors <- sample(rainbow(length(structure_types)))

# Build aggregated scatter points (size = number of buildings), plotting damage proportion against IM for different IMs
dam_props = field_data %>% group_by(PGV_mean, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p1 = ggplot(dam_props, aes(x=exp(PGV_mean), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point() + 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`SA(0.3)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p2 = ggplot(dam_props, aes(x=exp(`SA(0.3)_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`SA(1.0)_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p3 = ggplot(dam_props, aes(x=exp(`SA(1.0)_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))

dam_props = field_data %>% group_by(`PGA_mean`, structure_type) %>% summarise(propDam = mean(GT >=1), nBuild=n())
p4 = ggplot(dam_props, aes(x=exp(`PGA_mean`), y=propDam, col=as.factor(structure_type), size=nBuild)) + geom_point()+ 
  scale_color_manual(values = setNames(colors, structure_types))

# Arrange the four scatter plots in a 2x2 grid
grid.arrange(p1, p2, p3, p4, nrow=2, ncol=2)

# Transform back to original scale
field_data$`SA(0.3)_mean` = exp(field_data$`SA(0.3)_mean`)
field_data$`PGV_mean` = exp(field_data$`PGV_mean`)
field_data$`PGA_mean` = exp(field_data$`PGA_mean`)
field_data$`SA(1.0)_mean` = exp(field_data$`SA(1.0)_mean`)

# ----------------------------------------------------------------------------------------------
# -------------------------------------- Plot full map -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Location of ShakeMap XML; get_IMT_from_xml defined in your Functions.R
xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
meanhaz <- get_IMT_from_xml(xml_loc, 'psa03')
names(meanhaz)= 'psa03_mean'

# Get admin-level (provinces) geometry for Turkey for mapping
turkey_sf <- st_as_sf(gadm(country = "TUR", level = 1, path = tempdir()))

# Read ministry-collected dataset (MEUCC)
ministrys_df_full = readRDS(paste0(dir, 'Data_NonPublic/ministrys_df_full_psa'))

# Smooth the hazard raster to reduce pixelation / noise
# Create Gaussian focal matrix and apply focal filter
w <- focalMat(meanhaz, d=0.02, type = "Gauss")
meanhaz_smooth <- focal(meanhaz, w = w)

# Convert raster to data frame for ggplot contours
haz_df <- as.data.frame(meanhaz_smooth, xy = TRUE)
# Ensure the correct column name for the hazard values
names(haz_df)[3] <- "hazard_val"

# Define plotting colors for different GT levels for ministries and survey data
gt_colors_meucc <- c("Undamaged" = "darkgrey", "Slightly damaged" = "yellow", "Partially collapsed" = "orange", "Moderately/severely collapsed" = "red", "Collapsed" = "darkred")
gt_colors_survey <- c("Undamaged" = "darkgrey", "Minor" = "yellow", "Moderate damage" = "orange", "Severe damage/partial collapse" = "red", "Collapsed" = "darkred")

# Map the GT numeric codes to factor labels for ministrys_df_full
ministrys_df_full$damage <- factor(ministrys_df_full$GT, 
                                   levels = c(0, 1, 2, 3, 4),
                                   labels = names(gt_colors_meucc))

# Map GT to factor labels for field survey
field_data$damage <- factor(field_data$GT, 
                            levels = c(0, 1, 2, 3, 4),
                            labels = names(gt_colors_survey))

# Sample a subset of ministry data for plotting (reduce plotting load)
ministrys_plot_data <- ministrys_df_full[sample(1:nrow(ministrys_df_full), 100000), ]
# Order by damage so plotting order shows undamaged first (for visibility)
ministrys_plot_data <- ministrys_plot_data[order(ministrys_plot_data$damage), ]

# Filter province labels to those whose centroids fall inside the plotting frame
map_centers <- st_centroid(turkey_sf)
labels_filtered <- turkey_sf[st_coordinates(map_centers)[,1] > 35.89 & 
                               st_coordinates(map_centers)[,1] < 39.36 &
                               st_coordinates(map_centers)[,2] > 35.8 & 
                               st_coordinates(map_centers)[,2] < 39.37, ]

# Fix a province label name mismatch if needed
labels_filtered$NAME_1[4] = 'Kahramanmaras'

# -----------------------------------------------------
# Define bounding boxes (rectangles) around named districts
# Each vector is c(xmin, xmax, ymin, ymax)
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

# Helper function to add a rectangle annotation for a boundary box
rect_layer <- function(b, color = "red", size = 0.6, linetype = "solid") {
  annotate("rect",
           xmin = b[1], xmax = b[2],
           ymin = b[3], ymax = b[4],
           fill = NA, color = color, size = size, linetype = linetype)
}

# Adjust hazard values scale if needed (here dividing by 100)
haz_df$hazard_val = haz_df$hazard_val / 100

# Rebuild p_main without the field_data layer and with rectangle annotations
# (This version omits the survey points and is styled differently)
p_main <- ggplot() +
  geom_sf(data = turkey_sf, fill = "grey92", color = "darkgrey", linewidth = 0.2) +
  geom_contour(data = haz_df, aes(x = x, y = y, z = hazard_val, color = after_stat(level)), 
               linewidth = 0.6, alpha = 0.7, bins = 7) +
  scale_color_viridis_c(
    name = "SA[T=0.3] (g)",
    guide = guide_colorbar(barheight = 10)
  ) +
  
  # Province labels (with small nudge for Sanliurfa to avoid overlap)
  geom_sf_text(data = labels_filtered, aes(label = NAME_1), 
               size = 4.8, color = "black", family='Times',
               nudge_x = ifelse(labels_filtered$NAME_1 == "Sanliurfa", -0.2, 0)) +
  
  geom_sf(data = turkey_sf, fill = NA, color = "black", linewidth = 0.4) +
  coord_sf(xlim = c(35.82785, 39.10934), ylim = c(35.8, 39.37362), expand = FALSE) +
  theme_void() +
  theme(
    text = element_text(family = "serif"),
    # Legend placement and styling
    legend.position = c(0.97, 0.02),        
    legend.justification = c(1, 0),
    legend.box = "vertical",               
    legend.box.just = "right",
    legend.title = element_text(face = "bold", size = 15),
    legend.text  = element_text(size = 15),
    # Unified legend box styling
    legend.background = element_blank(),
    legend.box.background = element_rect(
      fill = alpha("white", 0.85),
      color = "black",
      linewidth = 0.5
    ),
    legend.box.margin = margin(6, 6, 6, 6),
    plot.margin = margin(0,4,0,0),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", linewidth = 0.8, fill = NA)
  ) +
  # Add black rectangle outlines for each named boundary (dashed for Antakya)
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

# Print the updated map plot
print(p_main)


# ---------- Plot only Antakya (zoomed-in inset) ------------------
# Define tighter bounding box zb for zoomed inset
zb <- c(xmin = 36.122128, xmax = 36.214871, ymin =  36.164137,  ymax = 36.249347)

# Filter ministry data to the zoomed rectangle
ministrys_kahramanmaras <- ministrys_df_full[
  ministrys_df_full$longitude >= zb["xmin"] & ministrys_df_full$longitude <= zb["xmax"] &
    ministrys_df_full$latitude >= zb["ymin"] & ministrys_df_full$latitude <= zb["ymax"], 
]
# Sort for visibility by GT so plotting order is predictable
ministrys_kahramanmaras <- ministrys_kahramanmaras[order(ministrys_kahramanmaras$GT), ]

# Filter survey (field) data to zoom box
field_kahramanmaras <- field_data[
  field_data$longitude >= zb["xmin"] & field_data$longitude <= zb["xmax"] &
    field_data$latitude >= zb["ymin"] & field_data$latitude <= zb["ymax"], 
]

# Filter province labels to only those centroids that fall inside the local zoom
labels_local <- turkey_sf[st_coordinates(st_centroid(turkey_sf))[,1] > zb["xmin"] & 
                            st_coordinates(st_centroid(turkey_sf))[,1] < zb["xmax"] &
                            st_coordinates(st_centroid(turkey_sf))[,2] > zb["ymin"] & 
                            st_coordinates(st_centroid(turkey_sf))[,2] < zb["ymax"], ]


p_zoom = ggplot() +
  # Base map
  geom_sf(data = turkey_sf, fill = "grey92", color = "black", linewidth = 0.3) +
  
  # Layer 1: Ministry Data (MEUCC) — larger markers for zoomed view
  geom_point(data = ministrys_kahramanmaras, 
             aes(x = longitude, y = latitude, color = damage), 
             size = 0.5, alpha = 0.9) +
  scale_color_manual(
    values = gt_colors_meucc, 
    name = "MEUCC Data",
    guide = guide_legend(override.aes = list(size = 6, alpha = 1))
  ) +
  
  # Layer 2: Field Data (Engineering Survey) — square markers with black outlines
  geom_point(data = field_kahramanmaras, 
             aes(x = longitude, y = latitude, fill = damage), 
             shape = 22, color = "black", size = 1.5, stroke = 0.8) +
  scale_fill_manual(
    values = gt_colors_survey, 
    name = "Engineering Survey Data",
    guide = guide_legend(override.aes = list(size = 6, alpha = 1))
  ) +
  
  # Zoom to local bounding box
  coord_sf(xlim = c(zb["xmin"], zb["xmax"]), ylim = c(zb["ymin"], zb["ymax"]), expand = FALSE) +
  theme_void() +
  theme(
    text = element_text(family = "Times New Roman"),
    # Positioning the legend inside the plot
    legend.position = c(1, 0.02),        
    legend.justification = c(1, 0),
    legend.box = "vertical",               
    legend.box.just = "left",
    legend.title = element_text(face = "bold", size = 15),
    legend.text  = element_text(size = 15),
    legend.margin = margin(0, 0, 0, 0),
    # Unified legend box
    legend.background = element_blank(),
    legend.box.background = element_rect(
      fill = alpha("white", 0.85),
      color = "black",
      linewidth = 0.5
    ),
    legend.box.margin = margin(6, -14, 6, 6), 
    plot.margin = margin(0,0,0,4),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "red", linewidth = 1.3, linetype = "dashed", fill = NA)
  )

# Print zoom plot
p_zoom

# Put main map and zoom map side-by-side using plot_grid
plot_grid(
  p_main, p_zoom, 
  nrow = 1,
  rel_widths = c(0.46, 0.55)
)
# Save as : Data_Overview.pdf, 11.5 x 7.1


# ----------------------------------------------------------------------------------------------
# ------------------------------- ROC Analysis to compare IMs ----------------------------------
# ----------------------------------------------------------------------------------------------

roc_data_list <- list()
field_data$dam_flag = field_data$GT >=1  # logical damage flag used in ROC

# Loop through intensity measures (IMs) and structure types to build ROC curves
for (IM in c('PGV_mean', 'PGA_mean', 'SA(0.3)_mean', 'SA(1.0)_mean')) {
  for (structure in levels(field_data$structure_type)) {
    
    # Filter to structure-specific rows with a non-missing IM
    field_data_filt <- field_data %>%
      filter(structure_type == structure, !is.na(.data[[IM]]))
    
    # Only compute ROC if there are both classes and at least 10 observations
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

# Combine ROC pieces into a single data frame
roc_plot_df <- do.call(rbind, roc_data_list)

# Summarise AUCs (one AUC per structure_type x IM)
auc_summary <- roc_plot_df %>%
  distinct(structure_type, IM, AUC)


# Bar panel showing AUC comparison by structure type and IM
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

# ROC curves panel (TPR vs FPR) faceted by structure type
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

# Combine ROC curves and AUC bars in a vertical layout
combined_plot <- roc_panel / auc_panel + plot_layout(heights = c(3, 1))
combined_plot


# Reorder IM levels for readable AUC labels if needed
IM_levels <- unique(auc_summary$IM)
auc_summary <- auc_summary %>%
  mutate(IM = factor(IM, levels = IM_levels))

# Create text blocks (per facet) showing all AUC values for quick reference
auc_block <- auc_summary %>%
  arrange(IM) %>%
  group_by(structure_type) %>%
  summarise(
    label = paste0(
      "AUC values: \n",
      paste0(sub("_.*", "", IM), ": ", round(AUC, 2), collapse = "\n")
    ),
    x = 0.65,
    y = 0.03,
    .groups = "drop"
  )

# Manual color choices for IM curves (custom)
manual_cols <- c("#440154", "#FDE725", "#1FA187", "#39568CFF")

# Redefine roc_panel with custom colours and label boxes for facets
roc_panel <- ggplot(roc_plot_df, aes(x = FPR, y = TPR, color = sub("_.*", "", IM), group = sub("_.*", "", IM), linetype = sub("_.*", "", IM))) +
  geom_line(linewidth = 0.9) +
  geom_abline(intercept = 0, slope = 1, linewidth=0.5,color = "grey60",alpha=0.5) +
  scale_color_manual(values = manual_cols) +
  facet_wrap(~ structure_type, nrow = 1, scales = "fixed") +
  # Place one label box per facet containing AUC values
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
    strip.text = element_text(face = "bold", size = 12, family = "Times New Roman"),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
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

# Show final roc panel
roc_panel
# Save as : IM_comparison.pdf,10 x 3.1

# Also show ROC panel in 2-row layout (alternative size)
roc_panel + facet_wrap(~ structure_type, nrow = 2, scales = "fixed") 
# Save as : IM_comparison.pdf, 7.5 x 6

#----------------------------------------------------------------------------------------------
#------------------- Additional checks to justify use of PSA (0.3s) ---------------------------
#----------------------------------------------------------------------------------------------

library(caret)

target_structures <- c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", "RC MRF (8+ Storeys)", 'RC Wall (4-7 Storeys)', 'Unreinforced masonry')
all_ims <- c('PGV_mean', 'PGA_mean', 'SA(0.3)_mean', 'SA(1.0)_mean')

master_results <- list()

for (st in target_structures) {
  df_st <- field_data %>%
    filter(structure_type == st) %>%
    dplyr::select(dam_flag, all_of(all_ims)) %>%
    filter(if_all(all_of(all_ims), ~ !is.na(.x) & .x > 0))
  
  if (nrow(df_st) < 10 || length(unique(df_st$dam_flag)) < 2) next
  
  n <- nrow(df_st)
  actuals <- df_st$dam_flag
  model_names <- c(paste0("Uni_", all_ims), 
                   paste0("Bi_SA0.3_", setdiff(all_ims, "SA(0.3)_mean")))
  
  # --- PART A: LOOCV for Predictive Metrics ---
  oos_probs <- matrix(NA, nrow = n, ncol = length(model_names))
  colnames(oos_probs) <- model_names
  
  for (i in 1:n) {
    train <- df_st[-i, ]
    test  <- df_st[i, ]
    if(length(unique(train$dam_flag)) < 2) next
    
    # Univariate LOOCV
    for (im in all_ims) {
      form <- as.formula(paste0("dam_flag ~ log(`", im, "`)"))
      mod <- tryCatch(glm(form, data = train, family = binomial(link = "probit")), error = function(e) NULL)
      if (!is.null(mod)) oos_probs[i, paste0("Uni_", im)] <- predict(mod, newdata = test, type = "response")
    }
    # Bivariate LOOCV
    other_ims <- setdiff(all_ims, "SA(0.3)_mean")
    for (im in other_ims) {
      form <- as.formula(paste0("dam_flag ~ log(`SA(0.3)_mean`) + log(`", im, "`)"))
      mod <- tryCatch(glm(form, data = train, family = binomial(link = "probit")), error = function(e) NULL)
      if (!is.null(mod)) oos_probs[i, paste0("Bi_SA0.3_", im)] <- predict(mod, newdata = test, type = "response")
    }
  }
  
  # --- PART B: Full Fit for AIC ---
  full_fit_metrics <- sapply(model_names, function(m_name) {
    # Determine formula based on name
    if (grepl("Uni_", m_name)) {
      im_name <- gsub("Uni_", "", m_name)
      form <- as.formula(paste0("dam_flag ~ log(`", im_name, "`)"))
    } else {
      im_name <- gsub("Bi_SA0.3_", "", m_name)
      form <- as.formula(paste0("dam_flag ~ log(`SA(0.3)_mean`) + log(`", im_name, "`)"))
    }
    
    full_mod <- tryCatch(glm(form, data = df_st, family = binomial(link = "probit")), error = function(e) NULL)
    
    if (is.null(full_mod)) return(c(AIC = NA))
    
    # Calculate predictive metrics from Part A
    probs <- oos_probs[, m_name]
    brier <- mean((probs - actuals)^2, na.rm = TRUE)
    acc <- mean(ifelse(probs > 0.5, 1, 0) == actuals, na.rm = TRUE)
    
    return(c(AIC = AIC(full_mod), Brier_OOS = brier, Acc_OOS = acc))
  })
  
  # --- PART C: Final Aggregation ---
  st_table <- as.data.frame(t(full_fit_metrics)) %>%
    rownames_to_column("Model") %>%
    mutate(structure = st, NIR = max(mean(actuals == 1), mean(actuals == 0)))
  
  master_results[[st]] <- st_table
}

final_output <- bind_rows(master_results) %>% 
  group_by(structure) %>%
  mutate(Delta_AIC = AIC - min(AIC, na.rm = TRUE)) %>% # AIC relative to best model in group
  dplyr::select(structure, Model, Acc_OOS, NIR, Brier_OOS, AIC, Delta_AIC)

print(final_output)

as.data.frame(final_output[,c('structure', 'Model', 'AIC')])

# ----------------------------------------------------------------------------------------------
# -------------------------------- Prepare Ministrys data --------------------------------------
# ----------------------------------------------------------------------------------------------
# Read filtered ministrys dataset (smaller / pre-filtered used for Stan)
ministrys_df <- readRDS(paste0(dir, 'Data_NonPublic/ministrys_df_filt'))

# Convert City names in ministrys_df to numeric matching factor levels from field_data
ministrys_df$City = match(ministrys_df$City, levels(field_data$City))


# ----------------------------------------------------------------------------------------------
# -------------------------------- Prior data --------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Map district names used in field_data to NAME_1 labels in exposure datasets (upper-case)
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


# ---------------------------------------------------------------
# -------------- Prior Zone Probabilities 2 ---------------------
# ---------------------------------------------------------------

# ------------------------------------------------------------------------------
# Load exposure CSVs (residential, commercial, industrial)
# If files are missing, stop with clear download instructions
# ------------------------------------------------------------------------------

Exposure_Res <- check_and_read(
  "Exposure_Res_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Res_Turkey_Adm1.csv"
)

Exposure_Com <- check_and_read(
  "Exposure_Com_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Com_Turkey_Adm1.csv"
)

Exposure_Ind <- check_and_read(
  "Exposure_Ind_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Ind_Turkey_Adm1.csv"
)

# Initialize matrix to accumulate zone counts per city
zone_per_city <- matrix(NA, nrow=length(levels(field_data$City)), ncol=4)
colnames(zone_per_city) = c('Residential', 'Commercial', 'Industrial', 'Mixed')
rownames(zone_per_city) = levels(field_data$City)

# For each city in the survey, sum buildings by zone (and by taxonomy mixing)
for (city in levels(field_data$City)){
  district = district_city_matches[which(names(district_city_matches)==city)]
  zone_per_city[city, 'Residential'] = sum((Exposure_Res %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = sum((Exposure_Res %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Commercial'] = sum((Exposure_Com %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = zone_per_city[city, 'Mixed'] + sum((Exposure_Com %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Industrial'] = sum((Exposure_Ind %>% filter(NAME_1 == toupper(district) & !str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
  zone_per_city[city, 'Mixed'] = zone_per_city[city, 'Mixed'] + sum((Exposure_Ind %>% filter(NAME_1 == toupper(district) & str_detect(TAXONOMY, 'MIX')))$BUILDINGS)
}

# Convert counts to probabilities per city
zone_per_city_probs = zone_per_city / rowSums(zone_per_city)

zone_per_city_probs %<>% as.data.frame()
zone_per_city_probs$district = district_city_matches[rownames(zone_per_city_probs)]

# Group to district-level averages (district = NAME_1)
zone_per_district = zone_per_city_probs %>% group_by(district) %>% summarise(Residential=mean(Residential),
                                                                             Commercial =mean(Commercial),
                                                                             Industrial =mean(Industrial),
                                                                             Mixed = mean(Mixed))

# Dirichlet concentration parameter for sampling zone proportions
alpha_omega = 10
# Sample from Dirichlet per district to capture uncertainty in zone proportions
dirichlet_samples <- lapply(seq_len(nrow(zone_per_district)), function(i) {
  props <- as.numeric(zone_per_district[i, 2:5])
  district_name <- zone_per_district$district[i]
  
  samples <- rdirichlet(5000, alpha_omega * props)  # 5000 samples
  colnames(samples) <- colnames(zone_per_district)[2:5]
  
  as_tibble(samples) %>%
    mutate(district = district_name)
}) %>%
  bind_rows()

# Convert to long format for plotting histograms of zone proportions
plot_df <- dirichlet_samples %>%
  pivot_longer(cols = c("Residential", "Commercial", "Industrial", "Mixed"),
               names_to = "zone", values_to = "value")

plot_df$zone = factor(plot_df$zone, levels=c('Residential', 'Commercial', 'Industrial', 'Mixed'))

# Colour palette for zone types
custom_fill_colors <- c(
  "Residential" = "#1FA187",
  "Commercial" = "#440154",
  "Industrial" = "#FDE725FF",
  "Mixed" = "tomato"
)

# Create one histogram per district showing the sampled zone proportions
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
      legend.position = "right",
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm")
    ) +
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
})

# Combine district histograms into one vertical layout (guides collected)
combined_plot <- wrap_plots(plot_list, ncol = 1) + 
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "right",
                                legend.direction = "vertical"))

combined_plot
# Save as : Zone_Priors.pdf, 4.5 x 4.5


# ------------------------------------------------------------
# ------------- Prior Building Type -------------------------
# ------------------------------------------------------------

# Reload exposures
Exposure_Res <- check_and_read(
  "Exposure_Res_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Res_Turkey_Adm1.csv"
)

Exposure_Com <- check_and_read(
  "Exposure_Com_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Com_Turkey_Adm1.csv"
)

Exposure_Ind <- check_and_read(
  "Exposure_Ind_Turkey_Adm1.csv",
  "https://github.com/gem/global_exposure_model/blob/main/Europe/Turkey/Exposure_Ind_Turkey_Adm1.csv"
)

# Combine taxonomies across exposure types for height extraction
taxonomies_all = c(Exposure_Res$TAXONOMY, 
                   Exposure_Com$TAXONOMY, 
                   Exposure_Ind$TAXONOMY)

# Extract height substring token (like "H:1", "H:4", etc.)
heights <- str_extract(taxonomies_all, "H[^/]+")

# Normalize heights to uppercase and remove spaces
heights <- toupper(str_replace_all(str_extract(taxonomies_all, "H[^/]+"), "\\s+", ""))

# Map height strings to storey class labels
storey_class <- dplyr::case_when(
  str_detect(heights, "^H:?[1-3]$") ~ "(1-3 Storeys)",
  str_detect(heights, "^H:?[4-7]$") ~ "(4-7 Storeys)",
  str_detect(heights, "^H:?([8-9]|[1-9][0-9]+)$") |
    str_detect(heights, "^HBET:?7-9$") |
    str_detect(heights, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
  TRUE ~ ""
)

# Distribution of storey classes (used for later heuristics / priors)
height_distributions = table(storey_class)[c('(1-3 Storeys)', '(4-7 Storeys)', '(8+ Storeys)')]

# Map GEM-style taxonomy codes to your high-level structure_type names
map_code_to_structure <- function(ids) {
  set.seed(1)
  # Extract height string again, normalized
  height <- toupper(str_replace_all(str_extract(ids, "H[^/]+"), "\\s+", ""))
  
  storey_class <- dplyr::case_when(
    str_detect(height, "^H:?[1-3]$") ~ "(1-3 Storeys)",
    str_detect(height, "^H:?[4-7]$") ~ "(4-7 Storeys)",
    str_detect(height, "^H:?([8-9]|[1-9][0-9]+)$") |
      str_detect(height, "^HBET:?7-9$") |
      str_detect(height, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
    TRUE ~ ""
  )
  
  # Fill missing storey_class by sampling according to observed height_distributions
  miss_idx <- which(storey_class == "")
  if (length(miss_idx) > 0) {
    storey_class[miss_idx] <- sample(
      x = names(height_distributions),
      size = length(miss_idx),
      replace = TRUE,
      prob = as.numeric(height_distributions)
    )
  }
  
  # Map taxonomy prefix codes to structure type string
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

# Variant function that doesn't attempt to sample missing heights (more conservative)
map_code_to_structure_confident <- function(ids) {
  
  height <- toupper(str_replace_all(str_extract(ids, "H[^/]+"), "\\s+", ""))
  
  storey_class <- dplyr::case_when(
    str_detect(height, "^H:?[1-3]$") ~ "(1-3 Storeys)",
    str_detect(height, "^H:?[4-7]$") ~ "(4-7 Storeys)",
    str_detect(height, "^H:?([8-9]|[1-9][0-9]+)$") |
      str_detect(height, "^HBET:?7-9$") |
      str_detect(height, "^HBET:?10\\+$")            ~ "(8+ Storeys)",
    TRUE ~ ""
  )
  
  dplyr::case_when(
    str_detect(ids, "^MUR") ~ "Unreinforced masonry",
    str_detect(ids, "^MCF") ~ "Reinforced masonry",
    str_detect(ids, "^CR") &
      !str_detect(ids, "LWAL") &
      !str_detect(ids, "LDUAL") ~ paste("RC MRF", storey_class),
    str_detect(ids, "^CR") & str_detect(ids, "LWAL") ~ paste("RC Wall", storey_class),
    str_detect(ids, "^CR") & str_detect(ids, "LDUAL") ~ paste("RC Dual system", storey_class),
    TRUE ~ "Other"
  )
}


# Use the GEM exposure data to count number of buildings of each type in each city + zone
buildings_per_zonecity <- array(NA, dim=c(length(levels(field_data$structure_type)), length(levels(field_data$City)), 4))
dimnames(buildings_per_zonecity) = list(levels(field_data$structure_type),
                                        levels(field_data$City),
                                        c('Residential', 'Commercial', 'Industrial', 'Mixed'))

# Loop over cities and sum counts for each zone and structure mapping
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

# Convert raw counts to probabilities per column
buildings_per_zonecity_probs <- buildings_per_zonecity

for (k in 1:dim(buildings_per_zonecity)[3]) {
  slice <- buildings_per_zonecity_probs[,,k]
  cs <- colSums(slice, na.rm = TRUE)
  # divide each column by its column sum
  buildings_per_zonecity_probs[,,k] <- sweep(slice, 2, cs, FUN = "/")
}

# Some fixed prior probabilities for certain building types (domain knowledge)
fixed_building_probs = c(
  'RC Wall' = 0.05,
  'RC Dual system' = 0.025,
  'Reinforced masonry' = 0.05
)

# Normalize the height distribution to sum to 1
height_distributions = height_distributions / sum(height_distributions)

# Enforce fixed fractional probabilities for some building types across heights (heuristic)
buildings_per_zonecity_probs['RC Wall (1-3 Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(1-3 Storeys)']
buildings_per_zonecity_probs['RC Wall (4-7 Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(4-7 Storeys)']
buildings_per_zonecity_probs['RC Wall (8+ Storeys)',,] = fixed_building_probs['RC Wall'] * height_distributions['(8+ Storeys)']
buildings_per_zonecity_probs['RC Dual system (4-7 Storeys)',,] = fixed_building_probs['RC Dual system'] * height_distributions['(4-7 Storeys)']/sum(height_distributions['(4-7 Storeys)'] + height_distributions['(8+ Storeys)']) # only consider RC Dual 4+ storeys
buildings_per_zonecity_probs['RC Dual system (8+ Storeys)',,] = fixed_building_probs['RC Dual system'] * height_distributions['(8+ Storeys)']/sum(height_distributions['(4-7 Storeys)'] + height_distributions['(8+ Storeys)']) # only consider RC Dual 4+ storeys
buildings_per_zonecity_probs['Reinforced masonry',,] = fixed_building_probs['Reinforced masonry']

# Normalize small probabilities and then renormalize non-fixed rows to sum to remaining mass
non_fixed_rows = !(rownames(buildings_per_zonecity) %in% c('RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)', 
                                                           'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)', 'Reinforced masonry'))

# Floor extremely small probabilities to 0.02 (avoid zeros)
buildings_per_zonecity_probs[buildings_per_zonecity_probs < 0.02] = 0.02

# Re-normalize non-fixed rows to sum to 1 - sum(fixed_building_probs)
for (k in 1:dim(buildings_per_zonecity)[3]) {
  slice <- buildings_per_zonecity_probs[non_fixed_rows,,k]
  cs <- colSums(slice, na.rm = TRUE)
  # divide each column by its column sum and rescale to remaining mass
  buildings_per_zonecity_probs[non_fixed_rows,,k] <- sweep(slice, 2, cs, FUN = "/") * (1-sum(fixed_building_probs))
}

# ------------------------------------------------------------
# ------------- Prior Fragility Curve -----------------------
# ------------------------------------------------------------

if (file.exists(paste0(dir, 'Data/IMT_compare_shakemap'))) {
  # Load the precomputed values
  IMT_vals <- readRDS(paste0(dir, 'Data/IMT_compare_shakemap'))

} else {
  # Perform extraction and comparison
  xml_loc <- paste0(dir, 'Data/ShakeMapUpd.xml.gz')
  
  # Ensure the XML file actually exists before trying to read it
  if(!file.exists(xml_loc)) stop("Source XML file not found at: ", xml_loc)
  
  shake_xml_loc <- read_xml(xml_loc)
  grid <- xmlParse(shake_xml_loc)
  xml_data <- xmlToList(grid)
  
  # Extract grid data
  lines <- strsplit(xml_data$grid_data, "\n")[[1]]
  
  IMT_vals <- t(sapply(lines, function(x) {
    parts <- strsplit(x, " ")[[1]]
    as.numeric(parts[3:8])
  }))
  
  # Formatting
  rownames(IMT_vals) <- rep("", nrow(IMT_vals))
  colnames(IMT_vals) <- c('MMI', 'pga', 'pgv', 'psa03', 'psa10', 'psa30')
  IMT_vals <- IMT_vals[-1, ] # Remove header/empty first row
  
  # Save for next time
  saveRDS(IMT_vals, paste0(dir, 'Data/IMT_compare_shakemap'))

}

IMT_vals %<>% as.data.frame()

# Structural fragility reference dataset
struct_fragility = read.csv(paste0(dir, 'Data/struct_fragility.csv'))

# Map building classes in fragility table to your structure_type labels
struct_fragility$structure_type = map_code_to_structure_confident(struct_fragility$Building_class)
# Filter to slight damage (or the Damage_state you're modelling as 'GT >= 1')
struct_fragility %<>% filter(Damage_state == 'slight')


# Pivot the fragility table from wide iml.* columns to long format
ref_curves <- struct_fragility %>%
  pivot_longer(
    cols = starts_with("iml."),
    names_to = "IM",
    values_to = "P_Damage"
  ) %>%
  mutate(
    IM = as.numeric(str_remove(IM, "iml\\."))
  )

# Build PSA(0.3) equivalents for reference curves (via regressions between IMs)
ref_curves$PSA0.3 = NA
ref_curves$PSA0.3[which(ref_curves$IMT == "SA(0.3s)")] = ref_curves$IM[which(ref_curves$IMT == "SA(0.3s)")]

# Fit linear relationships between IMs in IMT_vals (no intercept)
lm_SA.3_PGV = lm(psa03 ~ pgv-1, data=IMT_vals)
lm_SA.3_PGA = lm(psa03 ~ pga-1, data=IMT_vals)
lm_SA.3_SA1 = lm(psa03 ~ psa10-1, data=IMT_vals)

# Use these regressions to express other IM reference curves in PSA0.3 space
ref_curves$PSA0.3[which(ref_curves$IMT == "PGA")] = ref_curves$IM[which(ref_curves$IMT == "PGA")] * coef(lm_SA.3_PGA)[1]
ref_curves$PSA0.3[which(ref_curves$IMT == "SA(1.0s)")] = ref_curves$IM[which(ref_curves$IMT == "SA(1.0s)")] * coef(lm_SA.3_SA1)[1]

# Filter out ambiguous structural entries
ref_curves %<>% filter(structure_type != "RC MRF " & structure_type != "RC Wall " & structure_type != 'RC Dual system '
                       & structure_type != 'RC Dual system (1-3 Storeys)')

# Define prior fragility parameter means/sds per structure type (mu_mean, mu_sd, sigma_mean, sigma_sd)
prior_frag_structure_type = rbind('RC MRF (1-3 Storeys)' = c(-0.9,0.6,0.6,0.15),
                                  'RC MRF (4-7 Storeys)' = c(-0.9,0.6,0.6,0.15),
                                  'RC MRF (8+ Storeys)' = c(-0.9,0.6,0.6,0.15), 
                                  'RC Wall (1-3 Storeys)' = c(0.2,0.4,0.6,0.15), 
                                  'RC Wall (4-7 Storeys)' = c(-0.5,0.4,0.6,0.15), 
                                  'RC Wall (8+ Storeys)' = c(-0.6,0.5,0.6,0.15),
                                  'RC Dual system (4-7 Storeys)' = c(-0.7,0.6,0.6,0.15),
                                  'RC Dual system (8+ Storeys)' = c(-0.7,0.6,0.6,0.15), 
                                  'Other' = c(0,0.4,0.6,0.15),
                                  'Reinforced masonry' = c(-0.2,0.5,0.6,0.15),
                                  'Unreinforced masonry' = c(-1.2,0.4,0.6,0.15))

# Match row order to factor levels of field_data structure_type
prior_frag_structure_type = prior_frag_structure_type[match(rownames(prior_frag_structure_type),levels(field_data$structure_type)),]

colnames(prior_frag_structure_type) = c('mu_mean', 'mu_sd', 'sigma_mean', 'sigma_sd')

# PSA grid for constructing continuous curves from lognormal CDF
psa_vals <- seq(0.01, 2.5, by = 0.01)

# Convert to data frame with structure_type column for tidy operations
prior_frag_structure_type <- as.data.frame(prior_frag_structure_type)
prior_frag_structure_type$structure_type <- rownames(prior_frag_structure_type)

# Generate 100 samples of fragility curves per structure type from the prior distributions
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

# Define plot order for faceting
plot_order <- c("RC MRF (1-3 Storeys)", "RC MRF (4-7 Storeys)", "RC MRF (8+ Storeys)",
                "RC Wall (1-3 Storeys)", "RC Wall (4-7 Storeys)", "RC Wall (8+ Storeys)",
                "RC Dual system (4-7 Storeys)", "RC Dual system (8+ Storeys)", "Reinforced masonry",
                "Unreinforced masonry", "Other"
)

prior_ribbon_data$structure_type <- factor(prior_ribbon_data$structure_type, levels = plot_order)
ref_curves$structure_type <- factor(ref_curves$structure_type, levels = plot_order)

# Extend reference curves to PSA=2.5 by interpolation if necessary
ref_curves_extended <- ref_curves %>%
  group_by(Building_class, Damage_state, structure_type) %>%
  group_modify(~ {
    df <- .x
    if(length(unique(df$PSA0.3)) >= 2) {
      interp <- approx(x = df$PSA0.3, y = df$P_Damage, xout = 2.5, rule = 2)
      new_row <- df[1, ]
      new_row$PSA0.3 <- 2.5
      new_row$P_Damage <- interp$y
      bind_rows(df, new_row) %>% arrange(PSA0.3)
    } else {
      df
    }
  }) %>%
  ungroup() %>%
  mutate(alpha_line = ifelse(structure_type == "Other",  0.05, 0.2),
         linewidth_num = ifelse(structure_type == "Other", 0.2, 0.1))

# Plot prior ribbons with ref curves overlaid
PriorFragCurves = ggplot(prior_ribbon_data, aes(x = PSA)) +
  geom_line(
    data = ref_curves_extended,
    aes(x = PSA0.3, y = P_Damage, group = Building_class, alpha = alpha_line, linewidth = linewidth_num, linetype = ifelse(IMT=='SA(0.3s)',"solid", "dotted")),
    color = "darkgrey",
    inherit.aes = FALSE
  ) + scale_linetype_identity() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1FA187", alpha = 0.4) +
  geom_line(aes(y = median), color = "#1FA187", linewidth = 0.8) +
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
# Save as : PriorFragCurves.pdf, 8.5 x 5

# Alternative facet arrangement for portrait layout
PriorFragCurves + facet_wrap(~structure_type, ncol = 3, scales='free')
# Save as : PriorFragCurves.pdf, 7.5 x 8 (portrait)


# Normalize buildingtype prior probabilities and floor small values
prior_frag_structure_type$structure_type = NULL
buildingtypes_priorprob = buildings_per_zonecity_probs 
for (k in 1:dim(buildingtypes_priorprob)[3]) {
  slice <- buildingtypes_priorprob[,,k]
  slice[is.na(slice)] = 0.01
  cs <- colSums(slice, na.rm = TRUE)
  buildingtypes_priorprob[,,k] <- sweep(slice, 2, cs, FUN = "/")
}

# Some ad-hoc prior override for URM dest
prior_frag_URM_dest = as.numeric(prior_frag_structure_type[NROW(prior_frag_structure_type),])
prior_frag_URM_dest[1] = 0.5

# Compute mean PSA by city in field data (used later as covariate)
mean_PSA_by_city = field_data %>% group_by(City) %>% summarise(mean_PSA=mean(`SA(0.3)_mean`))


# ----------------------------------------------------------------------------------------------
# --------------------------------------- Fit Model --------------------------------------------
# ----------------------------------------------------------------------------------------------

# Sample up-to 1000 ministrys observations per city to reduce size for Stan
set.seed(1)
ministrys_df_sampled <- ministrys_df %>%
  group_by(City) %>%
  group_modify(~ slice_sample(.x, n = pmin(nrow(.x), 1000))) %>%
  ungroup()

# Compile the Stan model (FullModel.stan)
stan_model_compiled <- stan_model(paste0(dir, "StanModels/FullModel.stan"))

# Prepare the data list required by the Stan model
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

# Run HMC sampling (mark: adapt control commented out in case you want to tweak)
fit <- sampling(
  stan_model_compiled,  # compiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  pars = c("mu","beta","nu_0","nu_1","kappa_0","kappa_1",
           "sigma_city","sigma_obs","zone_probs","buildingTypeProbs"),
  seed = 123
)

# Pairplot for two parameters to inspect potential correlations
pairs(fit, pars=c('beta[10]', 'buildingTypeProbs[1,1,1]'))

# Save full fit object with date stamp
saveRDS(fit,paste0(dir, 'StanFits/fullfit', Sys.Date()))

# Write posterior summary as CSV
write.csv(summary(fit)$summary, paste0(dir, 'PosteriorSummaryFeb12'))


#------------------------------------ Fit sensitivity analysis --------------------------------

field_data_sensitivityAnalysis = field_data
field_data_sensitivityAnalysis$GT[which(field_data_sensitivityAnalysis$structure_type == 'Other')] = 0
field_data_sensitivityAnalysis$GT[which(field_data_sensitivityAnalysis$structure_type == 'RC Wall (1-3 Storeys)')][1] = 0

stan_data <- list(
  N_buildingTypes = length(unique(buildingTypes)),
  N_cities = length(unique(cities)),
  N_zones = 4, 
  
  # Field Survey Data:
  N_buildings = NROW(field_data_sensitivityAnalysis),
  PSA = field_data_sensitivityAnalysis$`SA(0.3)_mean`,
  damage_flag = ifelse(field_data_sensitivityAnalysis$GT >= 1, 1, 0),
  buildingTypes = buildingTypes,
  cities = cities,
  buildingtype_counts = buildingtype_counts,
  mean_PSA = mean_PSA_by_city$mean_PSA[match(levels(field_data_sensitivityAnalysis$City), mean_PSA_by_city$City)],
  
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

# Run HMC sampling (mark: adapt control commented out in case you want to tweak)
fit <- sampling(
  stan_model_compiled,  # compiled model
  data = stan_data,
  iter = 2000,
  chains = 3,
  warmup = 1000, 
  pars = c("mu","beta","nu_0","nu_1","kappa_0","kappa_1",
           "sigma_city","sigma_obs","zone_probs","buildingTypeProbs"),
  seed = 123
)

# Save full fit object with date stamp
saveRDS(fit,paste0(dir, 'StanFits/fullfit_sensitivityAnalysis', Sys.Date()))



#-----------------------------------------------------------------------------------------------
#----------------------------- Sensitivity analysis of posteriors ------------------------------
#-----------------------------------------------------------------------------------------------

# Options: 
#   - A) Stronger prior
#   - B) 


# fit_og = readRDS('/home/manderso/Documents/GitHub/TUR2023_02_06/StanFits/FullModelFit_2026-02-11')
# fit_sensitivity = readRDS('/home/manderso/Documents/GitHub/TUR2023_02_06/StanFits/fullfit_sensitivityAnalysis2026-02-12')

fit_og = readRDS('/home/manderso/Documents/GitHub/TUR2023_02_06/StanFits/fullfit2026-02-12')
fit_sensitivity = readRDS('/home/manderso/Documents/GitHub/TUR2023_02_06/StanFits/fullfit_sensitivityAnalysis2026-02-13')


which(stan_data$buildingTypes == 9)
stan_data$cities[77]

field_data$GT[field_data$City==levels(field_data$City)[7]]

summary(fit_og, pars=c('mu[9]', 'beta[9]'))$summary
summary(fit_sensitivity, pars=c('mu[9]', 'beta[9]'))$summary

summary(fit_og, pars=c('mu[2]', 'beta[2]'))$summary
summary(fit_sensitivity, pars=c('mu[2]', 'beta[2]'))$summary

psa_grid <- seq(0, 2.1, 0.01)
n_post_samples <- 2000   # number of posterior curve realisations per structure per fit

# Target structures to plot (panel order)
target_structs <- c("Other", "RC Wall (1-3 Storeys)")

# -----------------------------------------------------------------------------
# Helper: extract mu & beta vectors for a given structure index from a draw df
# Note: this assumes Stan parameters are named like "mu[1]" and "beta[1]".
# If your parameter names differ, change the grep patterns below.
# -----------------------------------------------------------------------------
extract_mu_beta <- function(draws_df, struct_row_index) {
  mu_name   <- grep(paste0("^mu\\[", struct_row_index, "\\]$"), names(draws_df), value = TRUE)
  beta_name <- grep(paste0("^beta\\[", struct_row_index, "\\]$"), names(draws_df), value = TRUE)
  if (length(mu_name) == 0 || length(beta_name) == 0) {
    stop("Could not locate mu/beta parameters for structure index ", struct_row_index,
         ". Check parameter names in the fitted object (use names(as_draws_df(fit))[1:200]).")
  }
  mu_vec   <- draws_df[[mu_name[1]]]
  beta_vec <- draws_df[[beta_name[1]]]
  list(mu = mu_vec, beta = beta_vec)
}

# -----------------------------------------------------------------------------
# Build posterior curve frames for a given fit object
# -----------------------------------------------------------------------------
make_post_curves <- function(fit_obj, label, target_structs, psa_grid, n_post_samples, prior_frag_structure_type) {
  draws_df <- as_draws_df(fit_obj)  # from 'posterior' package
  # determine which rows in prior_frag_structure_type correspond to target_structs
  struct_rows_idx <- which(rownames(prior_frag_structure_type) %in% target_structs)
  # map each requested structure row
  map_dfr(struct_rows_idx, function(i_row) {
    struct_name <- rownames(prior_frag_structure_type)[i_row]
    mu_beta <- extract_mu_beta(draws_df, i_row)
    mu_vec <- mu_beta$mu
    beta_vec <- mu_beta$beta
    # sample n_post_samples times from posterior vectors and build fragility curves
    map_dfr(1:n_post_samples, function(j) {
      mu_s   <- sample(mu_vec, 1)
      beta_s <- sample(beta_vec, 1)
      tibble(
        structure_type = struct_name,
        PSA = psa_grid,
        prob = plnorm(psa_grid, meanlog = mu_s, sdlog = beta_s),
        model = label,
        curve_id = paste0(label, "_", i_row, "_", j)
      )
    })
  })
}

# -----------------------------------------------------------------------------
# Generate posterior curves for original and sensitivity fits
# -----------------------------------------------------------------------------
# NOTE: prior_frag_structure_type must be defined in your environment and have rownames
# matching the structure names used in the model.
if (!exists("prior_frag_structure_type")) {
  stop("Object 'prior_frag_structure_type' not found. This must be present with rownames matching model structure ordering.")
}

posterior_curves_og   <- make_post_curves(fit_og, "Original Posterior", target_structs, psa_grid, n_post_samples, prior_frag_structure_type)
posterior_curves_sens <- make_post_curves(fit_sensitivity, "Adjusted Posterior", target_structs, psa_grid, n_post_samples, prior_frag_structure_type)

# -----------------------------------------------------------------------------
# Combine and summarise posterior draws
# -----------------------------------------------------------------------------
all_curves <- bind_rows(posterior_curves_og, posterior_curves_sens) %>%
  filter(structure_type %in% target_structs)

curve_summary <- all_curves %>%
  group_by(structure_type, model, PSA) %>%
  summarise(
    lower = quantile(prob, 0.025, na.rm = TRUE),
    median = median(prob, na.rm = TRUE),
    upper = quantile(prob, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Prepare survey (observed) points aggregated by rounded PSA
# -----------------------------------------------------------------------------
# Assumes field_data exists with columns: structure_type, `SA(0.3)_mean`, GT (damage indicator)
if (!exists("field_data")) {
  stop("Object 'field_data' not found. Please load your field survey dataframe into variable 'field_data'.")
}
field_data <- field_data %>%
  mutate(rounded_PSA = round(`SA(0.3)_mean` * 200) / 200)

survey_points <- field_data %>%
  filter(structure_type %in% target_structs) %>%
  group_by(structure_type, rounded_PSA) %>%
  summarise(prop_dam = mean(GT >= 1), Build = n(), .groups = "drop") %>%
  rename(PSA = rounded_PSA)

# -----------------------------------------------------------------------------
# Plotting
# -----------------------------------------------------------------------------

fill_colors <- c("Original Posterior" = "#440154", "Adjusted Posterior" = "#2b8cbe")

curve_summary$structure_type = factor(curve_summary$structure_type, levels = c('RC Wall (1-3 Storeys)', 'Other'))

p <- ggplot() +
  geom_ribbon(
    data = curve_summary,
    aes(x = PSA, ymin = lower, ymax = upper,
        fill = model),
    alpha = 0.25
  ) +
  geom_line(
    data = curve_summary,
    aes(x = PSA, y = median,
        color = model),
    linewidth = 0.8, linetype='dashed'
  ) +
  facet_wrap(~ structure_type, scales = "free") +
  scale_fill_manual(values = fill_colors, name = "Model") +
  scale_color_manual(values = fill_colors, name = "Model") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  labs(
    x = "PSA[T=0.3s] (g)",
    y = "Probability of Damage",
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    strip.text = element_text(size = 11),
    legend.title = element_blank(),
    legend.position = 'right',
    #legend.justification = c(1, 0),
    legend.box = "horizontal",
    legend.background = element_rect(
      fill = alpha("white", 0.85),
      color = "black",
      linewidth = 0.4
    ),
    legend.text = element_text(size = 10),
    legend.key.height = unit(1.2, "lines"),
    legend.key.width = unit(1.5, "lines")
  )

print(p)
# Save as: PostSensitivity.pdf, 3 x 8.5

# ----------------------------------------------------------------------------------------------
# ----------------------------------- MCMC Diagnostics -----------------------------------------
# ----------------------------------------------------------------------------------------------

# Check R-hat values > 1.01 (indicative of potential non-convergence)
rhats <- summary(fit)$summary[,"Rhat"]
rhats[which(rhats > 1.01)]

# Traceplots for key parameters to visually inspect mixing
rstan::traceplot(fit, pars=c('nu_0', 'nu_1', 'kappa_0', 'kappa_1'))


# ----------------------------------------------------------------------------------------------
# --------------------------------- Overall param plot -----------------------------------------
# ----------------------------------------------------------------------------------------------

library(bayesplot)

# Density plots for handful of hyperparameters
mcmc_dens(fit, pars = c('nu_0', 'nu_1', 'kappa_0', 'kappa_1', 'sigma_city', 'sigma_obs'))
summary(fit, pars='zone_probs')

# Pull draws from posterior into a tibble for lightweight plotting
set.seed(123)
draws <- as_draws_df(fit) %>% as_tibble()

# Colors: match prior/posterior palettes for comparison panels
fill_colors <- c(
  "Prior"     = "#1FA187", # teal (prior)
  "Posterior" = "#440154"  # purple (posterior)
)

# Create posterior point sample (500 draws) for plotted parameters
post_500 <- draws %>%
  dplyr::select(any_of(c("nu_0","nu_1","kappa_0","kappa_1","sigma_city","sigma_obs"))) %>%
  slice_sample(n = 500) %>%
  mutate(type = "Posterior")

# Create prior pseudo-samples for the same parameters (to visualise prior vs posterior)
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

# Combine prior and posterior draws into one dataset for scatter comparisons
dd <- bind_rows(prior_500, post_500) %>%
  mutate(type = factor(type, levels = c("Prior","Posterior")))

# Common theme for panels
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

# Plot nu0 vs nu1 (prior vs posterior scatter)
p_nu <- ggplot(dd, aes(x = nu_0, y = nu_1, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(nu[0]), y = expression(nu[1])) +
  base_thm

# Plot kappa0 vs kappa1 (prior vs posterior scatter)
p_kappa <- ggplot(dd, aes(x = kappa_0, y = kappa_1, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(kappa[0]), y = expression(kappa[1])) +
  base_thm

# Plot sigma_city vs sigma_obs (prior vs posterior scatter)
p_sigma <- ggplot(dd, aes(x = sigma_city, y = sigma_obs, color = type)) +
  geom_point(alpha = 0.7, size = 1.6) +
  scale_color_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = expression(tau[c]), y = expression(tau[b])) +
  base_thm

# Display the three panels individually
p_nu; p_kappa; p_sigma

# Combine into a single row, collect legends
combined_plot <- p_nu + p_kappa + p_sigma +
  plot_layout(guides = "collect") & theme(legend.position = "right")

# Show side by side
combined_plot
# Save as : PriorPostSuppParams.pdf, 10 x 3

# Alternative layout with legend at bottom
p_nu + p_kappa + p_sigma + plot_layout(guides = "collect") #& theme(legend.position = "bottom")
# Save as : PriorPostSuppParams.pdf, 8 x 3.25 # 10 x 3


# ----------------------------------------------------------------------------------------------
# ----------------------------------- Posterior Analysis ---------------------------------------
# ----------------------------------------------------------------------------------------------
library(MCMCpack)

# Convert posterior to tibble for extraction
posterior_samples <- as_draws_df(fit)
structure_types <- levels(field_data$structure_type)

# Draw samples from Dirichlet prior for two example cities (Kahramanmaras & Nurdagi)
prior_samples1 <- rdirichlet(1000, buildingtypes_priorprob[,'Kahramanmaras', 'Residential'] * stan_data$alpha_pi)
prior_samples2 <- rdirichlet(1000, buildingtypes_priorprob[,'Nurdagi', 'Residential']  * stan_data$alpha_pi)

# Label prior samples columns with structure types
colnames(prior_samples1) <- structure_types
colnames(prior_samples2) <- structure_types

# Convert prior samples into long tibble for plotting
prior_df <- as_tibble(prior_samples1) %>%
  pivot_longer(cols = everything(), names_to = "structure_type", values_to = "value") %>%
  mutate(source = "Prior")

# Build posterior distributions for city-level building type probabilities
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

# Set up fill colors for prior and posterior overlays
fill_colors <- c(
  "Prior" = "#1FA187",      # Teal-green (prior)
  "Posterior - Residential, Kahramanmaras" = "#FDE725FF",  # yellow
  "Posterior - Residential, Turkoglu" = "#440154"  # purple
)

# Generate histogram per structure type to compare prior vs posterior
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
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.15, "cm"),
      plot.title = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.position = 'bottom',
      axis.title.y = element_blank(),
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

wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )) &
  guides(fill = guide_legend(nrow = 1))
# Save as : PostObsModel.pdf, 8 x 5

# Combine into grid with shared legend
combined_plot <- wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot
# Save as : PostObsModel.pdf, 8 x 5

# Alternate layout portrait
combined_plot <- wrap_plots(plot_list, ncol = 3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot
# Save as : PostObsModel.pdf, 7.5 x 8 (portrait)


# --------------------- PLOT 2: Compare two cities (Kahramanmaras vs Narli) ---------------------

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
# combined_plot <- wrap_plots(plot_list, ncol = 4) + 
#   plot_layout(guides = "collect") & 
#   theme(legend.position = "bottom")
# combined_plot

wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )) &
  guides(fill = guide_legend(nrow = 1))

# --------------------- PLOT 3: Zone-level posterior comparisons ------------------------------

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

custom_fill_colors <- c(
  "Prior" = "#1FA187",
  "Posterior - Zone 1, Kahramanmaras" = "#440154",
  "Posterior - Zone 2, Kahramanmaras" = "#FDE725FF"
)

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

wrap_plots(plot_list, ncol = 4) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.box = "horizontal"
  )) &
  guides(fill = guide_legend(nrow = 1))

# Combine the zone plots into a grid
# combined_plot <- wrap_plots(plot_list, ncol = 4) +
#   plot_layout(guides = "collect") & 
#   theme(legend.position = "bottom")
# 
# combined_plot


# ---------------------------------------------------------------
# ----------------- Prior vs Posterior Fragility curves ---------
# ---------------------------------------------------------------

psa_grid <- seq(0, 1.7, 0.01)
prior_dist_frag = 'rnorm'

# Create long dataframe of prior curve samples (many realizations)
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

# Posterior samples: generate many posterior fragility curves by sampling mu and beta (sigma)
posterior_samples <- as_draws_df(fit)

posterior_curves <- map_dfr(1:nrow(prior_frag_structure_type), function(i) {
  mu_post <- pull(posterior_samples[, grep(paste0("^mu\\[", i, "\\]"), names(posterior_samples))])
  beta_post <- pull(posterior_samples[, grep(paste0("^beta\\[", i, "\\]"), names(posterior_samples))])
  
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

# Prepare survey (observed) points aggregated by rounded PSA for plotting
field_data$rounded_PSA <- round(field_data$`SA(0.3)_mean` * 200) / 200
survey_points <- field_data %>%
  group_by(structure_type, rounded_PSA) %>%
  summarise(prop_dam = mean(GT >= 1), Build = n(), .groups = "drop") %>%
  rename(PSA = rounded_PSA)

# Combine prior and posterior curves into one dataset
all_curves <- bind_rows(prior_curves, posterior_curves)

# Order factors for plotting
all_curves$type <- factor(all_curves$type, levels = c("Prior", "Posterior"))
all_curves$structure_type <- factor(all_curves$structure_type, levels = c('RC MRF (1-3 Storeys)', 'RC MRF (4-7 Storeys)', 'RC MRF (8+ Storeys)', 
                                                                          'RC Wall (1-3 Storeys)', 'RC Wall (4-7 Storeys)', 'RC Wall (8+ Storeys)',
                                                                          'RC Dual system (4-7 Storeys)', 'RC Dual system (8+ Storeys)',
                                                                          'Reinforced masonry','Unreinforced masonry','Other'))

# Summarise curves into 95% intervals and median for plotting ribbons and dashed medians
curve_summary <- all_curves %>%
  group_by(structure_type, type, PSA) %>%
  summarise(
    lower = quantile(prob, 0.025, na.rm = TRUE),
    median = median(prob, na.rm = TRUE),
    upper = quantile(prob, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

fill_colors <- c(
  "Prior" = "#1FA187",
  "Posterior" = "#440154"
)

# Plot prior vs posterior ribbons + survey points (sized by sample count)
ggplot() +
  geom_ribbon(
    data = curve_summary,
    aes(
      x = PSA, ymin = lower, ymax = upper,
      fill = type, color = type
    ),
    alpha = 0.3,
    linewidth=0.1
  ) +
  geom_line(
    data = curve_summary,
    aes(
      x = PSA, y = median,
      color = type
    ),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
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
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_blank(),
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

# Save as : PriorVsPosteriorFrag.pdf, 10 x 5.5


# Portrait variant (4 rows)
ggplot() +
  geom_ribbon(
    data = curve_summary,
    aes(
      x = PSA, ymin = lower, ymax = upper,
      fill = type, color = type
    ),
    alpha = 0.3,
    linewidth=0.1
  ) +
  geom_line(
    data = curve_summary,
    aes(
      x = PSA, y = median,
      color = type
    ),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
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
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),
    plot.title = element_blank(),
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
# Save as : PriorVsPosteriorFrag.pdf, 7.5 x 8 (portrait)
