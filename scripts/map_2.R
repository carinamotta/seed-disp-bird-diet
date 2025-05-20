# -------------------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# -------------------------------------------------------------------------

packages <- c("sf", "terra", "ggplot2", "ggspatial", "tmap", "dplyr", "geobr")

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

invisible(lapply(packages, library, character.only = TRUE))


# -------------------------------------------------------------------------
# 2. LOAD AND PREPARE DATA
# -------------------------------------------------------------------------

# Load site coordinates
points <- readr::read_csv(here::here("data", "external_data", "sites_coordinates.csv"))
points_sf <- st_as_sf(points, coords = c("long_y", "lat_x"), crs = 4326)

# Load municipality boundaries and define study area bounding box
sp <- read_municipality(code_muni = 35, year = 2020)
study_area <- st_as_sfc(st_bbox(c(xmin = -47.63, xmax = -47.55,
                                  ymin = -22.365, ymax = -22.21),
                                crs = st_crs(sp)))

# Load raster data
landcover <- rast(here::here("data", "external_data", "brasil_coverage_2023.tif"))
secveg_age <- rast(here::here("data", "external_data", "secondary_vegetation_age_2023.tif"))


# -------------------------------------------------------------------------
# 3. PROCESS RASTER DATA
# -------------------------------------------------------------------------

# Reproject study area to raster CRS
study_area_proj <- st_transform(study_area, crs(landcover))

# Crop and mask land cover raster
landcover_masked <- landcover |>
  crop(vect(study_area_proj)) |>
  mask(vect(study_area_proj))

# Process secondary vegetation raster
secveg_masked <- crop(secveg_age, study_area)
secveg_class <- secveg_masked
secveg_class[secveg_class > 0] <- 5
secveg_class[secveg_class == 0] <- NA  # Optional: remove youngest regrowth

# Combine with land cover
landcover_combined <- cover(secveg_class, landcover_masked)

# Reclassify into categories
rcl <- matrix(c(
  3, 1,    # Grassland -> Native vegetation
  5, 1,    # Secondary veg -> Native Secondary vegetation
  9, 2,    # Silviculture
  15, 3,   # Other non-vegetated areas
  20, 4,   # Water
  21, 5,   # Agriculture
  24, 6,   # Urban
  25, 5,   # Agriculture
  33, 5,   # Agriculture, Pasture
  39, 6,   # Urban
  41, 3,   # Other non-vegetated areas, Mining
  48, 3    # Other non-vegetated areas, Aquaculture
), ncol = 2, byrow = TRUE)


landcover_grouped <- classify(landcover_combined, rcl, others = NA)


# -------------------------------------------------------------------------
# 4. PREPARE DATA FOR PLOTTING
# -------------------------------------------------------------------------

# Convert to data frame for ggplot
landcover_df <- as.data.frame(landcover_grouped, xy = TRUE)
colnames(landcover_df)[3] <- "group"

# Assign factor levels and labels
landcover_df$group <- factor(landcover_df$group,
                             levels = 1:6,
                             labels = c(
                               "Native Vegetation",
                               "Silviculture",
                               "Other non-vegetated areas",
                               "Water",
                               "Agriculture",
                               "Urban"
                             )
)


# Transform points to raster CRS
points_proj <- st_transform(points_sf, crs = crs(landcover))


# -------------------------------------------------------------------------
# 5. PLOTTING
# -------------------------------------------------------------------------

# Custom colors
custom_colors <- c(
  "Native Vegetation" = "#1B9E77",
  "Silviculture" = "#A6761D",
  "Other non-vegetated areas" = "gray",
  "Water" = "gray",
  "Agriculture" = "#FFD92F",
  "Urban" = "maroon"
)

# Plot with ggplot2 - option 1
ggplot() +
  geom_raster(data = landcover_df, aes(x = x, y = y, fill = group)) +
  scale_fill_manual(values = custom_colors, name = "Land Cover") +
  geom_sf(data = study_area_proj, fill = NA, color = "black") +
  geom_sf(data = points_proj, color = "red", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80")) +
  scale_x_continuous(
  breaks = seq(-47.64, -47.55, by = 0.02),            # every ~0.02°
  labels = function(x) sprintf("%.2f°W", abs(x))      # nicer formatting
)

## FULL MAP ----

# Get a binary raster of NA pixels from the classified map
na_mask <- is.na(landcover_grouped)

# Mask the original rasters using NA mask
na_landcover <- mask(landcover_masked, na_mask, maskvalues = 0, updatevalue = NA)
na_secveg    <- mask(secveg_class, na_mask, maskvalues = 0, updatevalue = NA)

# Tabulate the land cover classes in the NA areas
freq(na_landcover)
freq(na_secveg)

# Load full, unfiltered raster
landcover_full <- rast(here::here("data", "external_data", "brasil_coverage_2023.tif"))

# Crop to the extent of your study area (optional, to focus the map)
landcover_cropped <- crop(landcover_full, vect(study_area_proj))

# Convert to data frame for plotting
landcover_full_df <- as.data.frame(landcover_cropped, xy = TRUE)
colnames(landcover_full_df)[3] <- "class"

# Optional: convert class column to factor (helps ggplot assign colors)
landcover_full_df$class <- as.factor(landcover_full_df$class)

# Lookup table (simplified example)
mapbiomas_lookup <- data.frame(
  class = as.factor(c(3, 4, 5, 9, 15, 20, 21, 24, 25, 33, 39, 41, 48)),
  name = c("Forest", "Savanna", "Grassland", "Silviculture", 
           "Other Non-Vegetated", "Water", "Agriculture", 
           "Mosaic of Uses", "Sugarcane", "Pasture", 
           "Urban", "Mining", "Aquaculture")
)

# Merge to include names
landcover_full_df <- landcover_full_df |>
  left_join(mapbiomas_lookup, by = "class")

# Plot with names
ggplot() +
  geom_raster(data = landcover_full_df, aes(x = x, y = y, fill = name)) +
  scale_fill_viridis_d(name = "Land Cover Class", option = "turbo") +
  geom_sf(data = study_area_proj, fill = NA, color = "black") +
  geom_sf(data = points_proj, color = "red", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE) +
  theme_minimal()



