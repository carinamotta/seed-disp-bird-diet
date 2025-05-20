
# Load packages

# 1. LOAD PACKAGES -------------------------------------------------------------

# a vector listing package names needed 

package.list <- c("sf",
                  "terra",
                  "ggplot2",
                  "ggspatial",
                  "tmap",
                  "dplyr",
                  "geobr")

#creating another list of new packages (if there are any)
new.packages <- package.list[!(package.list %in% installed.packages()
                               [,"Package"])]

#installing the packages if they aren't already on the computer
if(length(new.packages)) install.packages(new.packages)

#and loading the packages into R with a for loop
for(i in package.list){library(i, character.only = T)}



# Replace with your actual file path
points <- readr::read_csv(here::here("data", "external_data",
                                       "sites_coordinates.csv"))

# Convert to sf object
points_sf <- st_as_sf(points, coords = c("long_y", "lat_x"), crs = 4326)


# Load municipality boundaries
sp <- read_municipality(code_muni = 35, year = 2020)
#

study_area <- st_as_sfc(st_bbox(c(xmin = -47.63, xmax = -47.55,
                                                   ymin = -22.36, ymax = -22.21), crs = st_crs(sp)))




landcover <- rast(here::here("data", "external_data","brasil_coverage_2023.tif"))

secveg_age <- rast(here::here("data", "external_data",
                             "secondary_vegetation_age_2023.tif"))

# Reproject study area to match raster CRS
study_area_proj <- st_transform(study_area, crs(landcover))

# Crop and mask raster
landcover_crop <- crop(landcover, vect(study_area_proj))
landcover_masked <- mask(landcover_crop, vect(study_area_proj))

secveg_masked <- crop(secveg_age, study_area)

secveg_class <- secveg_masked
secveg_class[secveg_class > 0] <- 5
secveg_class[secveg_class == 0] <- NA  # optional, if you want to discard very young regrowth

# Step 2: Merge with land cover
landcover_combined <- cover(secveg_class, landcover_masked)

# Step 3: Reclassify into final groupings
rcl <- matrix(c(
  3, 1,    # Grassland -> Native vegetation
  5, 2,    # Secondary veg -> Secondary vegetation
  9, 3,    # Silviculture
  21, 4,   # Agriculture
  24, 4,
  25, 4,
  33, 5,   # Pasture
  39, 6    # Urban
), ncol = 2, byrow = TRUE)

landcover_grouped <- classify(landcover_combined, rcl, others = NA)

# Step 4: Convert to data frame for ggplot
landcover_df <- as.data.frame(landcover_grouped, xy = TRUE)
colnames(landcover_df)[3] <- "group"

# Step 5: Assign factor levels and labels
landcover_df$group <- factor(landcover_df$group,
                             levels = 1:6,
                             labels = c(
                               "Native Vegetation",
                               "Secondary Vegetation",
                               "Silviculture",
                               "Agriculture",
                               "Pasture",
                               "Urban"
                             )
)



# Transform points to match raster CRS
points_proj <- st_transform(points_sf, crs = crs(landcover))


ggplot() +
  geom_raster(data = landcover_df, aes(x = x, y = y, fill = as.factor(landcover))) +
  scale_fill_viridis_d(name = "Land Cover") +
  geom_sf(data = study_area_proj, fill = NA, color = "black") +
  geom_sf(data = points_proj, color = "red", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80"))

custom_colors <- c(
  "Native Vegetation" = "#1B9E77",
  "Secondary Vegetation" = "#66C2A5",
  "Silviculture" = "#A6761D",
  "Agriculture" = "#FFD92F",
  "Pasture" = "#E6AB02",
  "Urban" = "#E7298A"
)


landcover_raster <- raster::raster(landcover_grouped)
landcover_stars <- st_as_stars(landcover_raster)

ggplot() +
  geom_stars(data = landcover_stars) +
  scale_fill_manual(values = custom_colors, name = "Land Cover") +
  geom_sf(data = points_proj, color = "red", size = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering()) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80"))

