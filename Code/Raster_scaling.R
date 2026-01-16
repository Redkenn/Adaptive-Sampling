library(sf)  
library(raster)
library(terra)
library(mapview)
library(usdm)


# List all raster files in the folder
list_raster <- list.files(path = "Adaptive_Sampling/Inputs_raster", pattern = "\\.tif$", full.names = TRUE)


# Read the reference raster
raster_reference <- rast("Adaptive_Sampling/Inputs_raster/Slope.tif")


# Output directory
output_dir <- "Adaptive_Sampling/Inputs_raster_scaled"  # Output directory


# Loop to scale rasters
for (file in list_raster) {
  # Load the current raster
  raster_current <- rast(file)
   # Scale the raster to the resolution and extent of the reference raster
  raster_scaled <- resample(raster_current, raster_reference, method = "bilinear")
  # Generate output file name
  output_name <- file.path(output_dir, basename(file))
  # Save the scaled raster
  writeRaster(raster_scaled, output_name, overwrite = TRUE)
  # Completion message
  cat("Raster scaled and saved:", output_name, "\n")
}


# Load raster scaled 
list_raster_scaled <- list.files(path = "Adaptive_Sampling/Inputs_raster_scaled", pattern = "\\.tif$", full.names = TRUE)


# Load rasters into a SpatRaster object
raster_scaled <- rast(list_raster_scaled)

# Total number of layers
n_layers <- nlyr(raster_scaled)

# Group size (number of layers to plot at a time).
group_size <- 10

# Get the names of the layers
names_layer <- names(raster_scaled)

# Loop to plot layers in groups
for (i in seq(1, n_layers, by = group_size)) {
  # Select the layers for the current group
  layers_to_plot <- i:min(i + group_size - 1, n_layers)
  
  # Plot of the current group
  plot(raster_scaled[[layers_to_plot]], main = names_layer[layers_to_plot])
  
  # Wait for input before continuing to next group (optional).
  readline(prompt = "Press enter to continue...")
}


