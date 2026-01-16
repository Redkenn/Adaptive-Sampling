library(sf)  
library(raster)
library(terra)
library(mapview)



### BIOCLIMATIC VARIABLES (Res: 30 arc-seconds)


# Set the path to your raster files 
files_bioclim <- list.files(path = "Adaptive_Sampling/Climate/Bioclimatic", pattern = "\\.tif$", full.names = TRUE)


# Load rasters into a SpatRaster object
bioclim <- rast(files_bioclim)


# Study area
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()

# Function to process the bioclimatic raster to fit the study area
process_bioclim_layer <- function(raster, area_mask) {
  cropped_layer <- crop(raster, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  return(masked_layer)
}

# Get the current names of the raster
current_names <- names(bioclim)

# Remove “CHELSA_” and rename as “Bio1”, “Bio2”, etc.
new_names <- gsub("CHELSA_bio", "Bio", current_names)


# Assign the new names to the raster
names(bioclim) <- new_names


# Output directory
output_dir <- "Adaptive_Sampling/Inputs_900m"  # Output directory


# Apply the processing to each bioclimatic raster and save results
lapply(1:nlyr(bioclim), function(i) {
  processed_layer <- process_bioclim_layer(bioclim[[i]], region)
  output_path <- file.path(output_dir, paste0(new_names[i], ".tif"))
  writeRaster(processed_layer, filename = output_path, overwrite = TRUE)
  return(output_path)
})




### EVAPO-TRANSPIRATION and ARIDITy INDEX (Res: 30 arc-seconds)  ###

# Set the path to your raster files 
files_clim <- list.files(path = "Adaptive_Sampling/Climate/Global-AI_ET0_v3_annual", pattern = "\\.tif$", full.names = TRUE)


# Load rasters into a SpatRaster object
clim <- rast(files_clim)


# Study area
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()


# Function to process the AI and ETo raster to fit the study area
process_clim_layer <- function(raster, area_mask) {
  cropped_layer <- crop(raster, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  return(masked_layer)
}


# Rename the layers
layer_names <- "AI"

# Output directory
output_dir <- "Adaptive_Sampling/Inputs_900m"  # Output directory

# Apply the processing to each climatic variable and save results
lapply(1:nlyr(clim), function(i) {
  processed_layer <- process_clim_layer(clim[[i]], region)
  names(processed_layer) <- layer_names[i]
  output_path <- file.path(output_dir, paste0(layer_names[i], ".tif"))
  writeRaster(processed_layer, datatype = "FLT4S", filename = output_path, overwrite = TRUE)
  return(output_path)
})


### GLOBAL HORIZONTAL IRRADIATION and DIRECT NORMAL IRRADIATION  (Res: 9 arc-seconds)  ###

# Set the path to your raster files 
files_clim2 <- list.files(path = "Adaptive_Sampling/Climate/Italy_GISdata_LTAy_AvgDailyTotals_GlobalSolarAtlas-v2_GEOTIFF", pattern = "\\.tif$", full.names = TRUE)


# Load rasters into a SpatRaster object
clim2 <- rast(files_clim2)


# Study area
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()


# Function to process the DNI and GHI raster to fit the study area
process_clim2_layer <- function(raster, area_mask) {
  cropped_layer <- crop(raster, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  return(masked_layer)
}


# Rename the layers
layer_names <- c("DNI","GHI")

# Output directory
output_dir <- "Adaptive_Sampling/Inputs_280m"  # Output directory

# Apply the processing to each raster and save results
lapply(1:nlyr(clim2), function(i) {
  processed_layer <- process_clim2_layer(clim2[[i]], region)
  names(processed_layer) <- layer_names[i]
  output_path <- file.path(output_dir, paste0(layer_names[i], ".tif"))
  writeRaster(processed_layer, filename = output_path, overwrite = TRUE)
  return(output_path)
})


### SOIL  (Res: about 8 arc-seconds)  ###

# Set the path to your raster files 
files_soil <- list.files(path = "Adaptive_Sampling/Soil", pattern = "\\.tif$", full.names = TRUE)


# Load the rasters one at a time and standardize them
reference_raster <- rast(files_soil[1]) # Use the first file as a reference
aligned_rasters <- lapply(files_soil, function(file) {
  r <- rast(file)
  r <- resample(r, reference_raster, method = "bilinear") # Align resolution and extension
  return(r)
})


# Combines aligned rasters into a single SpatRaster
soil <- rast(aligned_rasters)

# Study area
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()

# Function to process the soil raster to fit the study area
process_soil_layers <- function(files, area_mask, prefix) {
  rasters <- lapply(files, rast)
  layers <- sprc(rasters)
  merged_layers <- merge(layers)
  cropped_layer <- crop(merged_layers, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  names(masked_layer) <- prefix
  return(masked_layer)
}

# Get prefixes (basenames without numbers and extensions).
prefixes <- gsub("(\\d+\\.tif)$", "", basename(files_soil))


# Group files based on prefixes
groups <- split(files_soil, prefixes)



# Output directory
output_dir <- "Adaptive_Sampling/Inputs_250m"  # Output directory


# Process each group of soil raster
lapply(names(groups), function(prefix) {
  files <- groups[[prefix]]
  processed_layer <- process_soil_layers(files, area_mask = region, prefix = prefix)
  output_path <- file.path(output_dir, paste0(prefix, ".tif"))
  writeRaster(processed_layer, filename = output_path, overwrite = TRUE)
  return(output_path)
})


# Read the new files and check extensions
processed_soil <- list.files(path = "Adaptive_Sampling/Inputs_250m", pattern = "\\.tif$", full.names = TRUE)
lapply(processed_soil, function(file) ext(rast(file)))


# some layers have different extent, so we standardize them
# Load the rasters one at a time and standardize them
reference_raster <- rast(processed_soil[1]) # Use the first file as a reference
aligned_rasters <- lapply(processed_soil, function(file) {
  r <- rast(file)
  r <- resample(r, reference_raster, method = "bilinear") # Align resolution and extension
  return(r)
})

# Save the aligned rasters with the original names
lapply(seq_along(aligned_rasters), function(i) {
  output_path <- file.path(output_dir, basename(processed_soil[i]))  # Keep original filename
  writeRaster(aligned_rasters[[i]], filename = output_path, overwrite = TRUE)
})



### DEM and SLOPE (Res: 3 arc-seconds)  ###

# Set the path to your raster files 
files_topo <- list.files(path = "Adaptive_Sampling/Topography", pattern = "\\.tif$", full.names = TRUE)


# Load rasters into a SpatRaster object
topo <- rast(files_topo)


# Study area
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()


# Function to process the topography raster to fit the study area
process_topo_layer <- function(raster, area_mask) {
  cropped_layer <- crop(raster, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  return(masked_layer)
}


# Rename the layers
layer_names <- c("Dem","Slope")

# Output directory
output_dir <- "Adaptive_Sampling/Inputs_90m"  # Output directory

# Apply the processing to each topography raster and save results
lapply(1:nlyr(topo), function(i) {
  processed_layer <- process_topo_layer(topo[[i]], region)
  names(processed_layer) <- layer_names[i]
  output_path <- file.path(output_dir, paste0(layer_names[i], ".tif"))
  writeRaster(processed_layer, filename = output_path, overwrite = TRUE)
  return(output_path)
})



### Distance from the coast (Res: 30 arc-seconds)  ###

# Load study area raster into a SpatRaster object
region_r <- rast("Adaptive_Sampling/Study_area/Sardinia_study_area.tif")


# Load study area shp
region <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>%
  st_transform(4326) %>% 
  vect()


# Value 1 is land and value NA is sea.
# Let's create a binary mask
r_bin <- region_r
r_bin[!is.na(r_bin)] <- 1 
r_bin[is.na(r_bin)] <- 0 

# Calculate the distance from the coast
dist_r <- distance(r_bin, target = 1) 


# Function to process the distance from the coast raster to fit the study area
process_dist_layer <- function(raster, area_mask) {
  cropped_layer <- crop(raster, ext(area_mask))
  masked_layer <- mask(cropped_layer, area_mask)
  return(masked_layer)
}


# Rename the layer
layer_name <- "Dist_c"

# Output directory
output_dir <- "Study_area/Inputs_900m"  # Output directory

# Apply the processing to distance from the coast raster and save results
lapply(1:nlyr(dist_r), function(i) {
  processed_layer <- process_dist_layer(dist_r[[i]], region)
  names(processed_layer) <- layer_name[i]
  output_path <- file.path(output_dir, paste0(layer_name[i], ".tif"))
  writeRaster(processed_layer, filename = output_path, overwrite = TRUE)
  return(output_path)
})


