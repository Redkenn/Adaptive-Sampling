library(terra)
library(sf)
library(ggplot2)
library(readr)
library(ggmap)
library(tidyterra)
library(ggspatial)
library(patchwork)

# Folders to process
folders <- c("df_pb_ratio1_buffer0km", "df_pb_ratio1_buffer5km", "df_pb_ratio1_buffer10km", "df_pb_ratio1_buffer20km", 
             "df_pb_ratio10_buffer0km", "df_pb_ratio10_buffer5km", "df_pb_ratio10_buffer10km", "df_pb_ratio10_buffer20km")




# Main path
base_path <- "Adaptive_Sampling/Results"


for (folder in folders) {
  # Paths to files and folders
  input_folder <- file.path(base_path, folder)
  predictions_file <- file.path(input_folder, "predictions.rds")
  
  if (file.exists(predictions_file)) {
    # Upload predictions
    predictions <- readRDS(predictions_file)
    # Convert to shapefile
    predictions_shp <- sf::st_as_sf(predictions, coords = c("X", "Y"), crs = 4326)
    # Creates a raster with the coordinate extension
    r <- rast(ext(predictions_shp), 2945, 2037) 
    # Rasterize data
    predictions_r <- rasterize(as(predictions_shp, "SpatVector"), r, field = "Prob")
    # Fix any imperfections
    raster_interpolated <- focal(predictions_r, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)
    # Save the raster in the same folder
    output_raster <- file.path(input_folder, "predictions_raster.tif")
    writeRaster(raster_interpolated, output_raster, overwrite = TRUE)
  } else {
    message("File not found: ", predictions_file)
  }
}

# List to save uploaded rasters
raster_list <- list()

for (folder in folders) {
  # Paths to files and folders
  input_folder <- file.path(base_path, folder)
  raster_file <- file.path(input_folder, "predictions_raster.tif")
  
  if (file.exists(raster_file)) {
    # Load the raster
    raster_data <- rast(raster_file)
    # Create the new name by replacing “df_pb_ratio” with “pred”
    raster_name <- gsub("df_pb_ratio", "pred", folder)
    # Save the raster to the list with the new name
    raster_list[[raster_name]] <- raster_data
  } else {
    message("File not found: ", raster_file)
  }
}


p1  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred1_buffer0km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "No buffer",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p2  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred1_buffer5km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 5 km",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 

p3  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred1_buffer10km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 10 km",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p4  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred1_buffer20km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 20 km",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 



p5  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred10_buffer0km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "No buffer",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p6  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred10_buffer5km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 5 km",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 



p7  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred10_buffer10km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 10 km",
       x = "", y = "") +
  labs(fill = "Habitat Suitability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm')) 


p8  <- 
  ggplot() +
  geom_spatraster(data = raster_list[["pred10_buffer20km"]], aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 20 km",
       x = "", y = "") +
  labs(fill = "Habitat Probability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.justification = c(2, 0),
        plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 10, angle = 0), 
        legend.key.size = unit(0.7, 'cm'))
        #legend.margin = margin(t = -9, b = 0, unit = "pt"))




ptot1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "Ratio 1:1 (181 presences and 181 pseudo-absences)",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))


ptot2 <- p5 + p6 + p7 + p8  + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "Ratio 1:10 (181 presences and 1810 pseudo-absences)",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))





