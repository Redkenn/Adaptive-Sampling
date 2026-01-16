library(sf)
library(tidyr)
library(terra)
library(ggplot2)
library(ggspatial)
library(patchwork)


# Load study area shapefile
region_shp <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>% st_transform(4326)

# Load study area shapefile with administrative boundaries
region_prov_shp <- st_read("Adaptive_Sampling/Study_area/region_prov.shp") %>% st_transform(4326)



# Load study area raster (region)
region <- rast('Adaptive_Sampling/Study_area/Sardinia_study_area.tif')


# Read presence points
presences <- st_read("Adaptive_Sampling/Sample_points/ponds.shp") %>% st_transform(4326)
presences <- presences[, c('TOPONIMO', 'geometry','HABITAT')]
presences$TOPONIMO <- presences$TOPONIMO %>% replace_na('Unknown')



# Extract coordinates
coords <- st_coordinates(presences)
df_pr <- data.frame(x = coords[, 1], y = coords[, 2], occ = 1)

# Create SpatVector for presence points
pop_imag <- terra::vect(as.matrix(df_pr[1:2]), crs = crs(region))

# Define buffer sizes
buffer_sizes <- c(0, 5, 10, 20) 

# Calculate buffers and save them as sf
buffers_sf <- list()
for (buffer_size in buffer_sizes) {
  if (buffer_size > 0) {
    v_buf <- terra::buffer(pop_imag, width = buffer_size * 1000)  # Convert km to meters
    buffers_sf[[paste0("buffer_", buffer_size, "km")]] <- st_as_sf(v_buf)  # Save buffer as sf
  }
}


# Define function to generate pseudo-absences and save buffers as sf
generate_pseudo_absences <- function(ratio, buffer_sizes) {
  results <- list()  # List for collecting generated dataframes
  buffers_list <- list()  # List for collecting sf buffers
  
  for (buffer_size in buffer_sizes) {
    if (buffer_size > 0) {  
      v_buf <- terra::buffer(pop_imag, width = buffer_size * 1000)  # Convert km to meters
      v_buf_sf <- st_as_sf(v_buf)  # Converts in sf
      buffers_list[[paste0("buffer_", buffer_size, "km")]] <- v_buf_sf  # Save buffer as sf
      
      region_outbuf <- terra::mask(region, v_buf, inverse = TRUE)
    } else {
      region_outbuf <- region  # No buffer, use the original raster directly
    }
    
    # Sample pseudo-absences
    bg_rand_outbuf <- terra::spatSample(region_outbuf, nrow(presences) * ratio, "random", na.rm = TRUE, as.points = TRUE, exhaustive = TRUE)
    df_bg <- as.data.frame(bg_rand_outbuf, geom = "XY")
    df_bg <- df_bg[-1]  # Remove the layer column
    df_bg$occ <- 0  # Set 0 for pseudo-absences
    
    # Combines presences and pseudo-absences
    df_pb <- rbind(df_pr, df_bg)
    df_pb <- df_pb[sample(1:nrow(df_pb)), ]  # Shuffle the dataset
    row.names(df_pb) <- NULL
    
    # Save the dataset
    output_folder <- "Adaptive_Sampling/Sample_points"
    file_name <- paste0("df_pb_ratio", ratio, "_buffer", buffer_size, "km.csv")
    write.csv(df_pb, file = file.path(output_folder, file_name), row.names = FALSE)
    
    # Save the dataframe to the list
    results[[paste0("df_pb_ratio", ratio, "_buffer", buffer_size, "km")]] <- df_pb
  }
  
  return(list(pseudo_absences = results, buffers = buffers_list))  
}

# Define ratios and buffer sizes
ratios <- c(1, 10)


# Generate datasets and store them in lists
all_datasets <- list()


for (ratio in ratios) {
  res <- generate_pseudo_absences(ratio, buffer_sizes)
  all_datasets <- c(all_datasets, res$pseudo_absences)  
  
}


folder <- "Adaptive_Sampling/Sample_points"

# Get the list of CSV files in the folder
file_list <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files and save them in a list of dataframes
all_datasets <- lapply(file_list, read.csv)


# Assign names to the dataframes in the list based on the file names (without extensions).
names(all_datasets) <- tools::file_path_sans_ext(basename(file_list))

# calculates centroids of provincial polygons
centroidXY <- st_centroid(region_prov_shp)
coords <- st_coordinates(centroidXY$geometry)

# add coordinates to region_prov_shp
region_prov_shp$x <- coords[,1]
region_prov_shp$y <- coords[,2]

p0  <- 
  ggplot(region_prov_shp) +
  geom_sf() +
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "",
       x = "", y = "") +
  theme_light() +
  geom_point(data = df_pr, aes(x = x, y = y, color = "Pond records"), size = 1) +
  scale_color_manual(name = "", values = c("Pond records" = "red")) + 
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.05),
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
        legend.text = element_text(size = 10, angle = 0, face = 'bold'), 
        legend.key.size = unit(0.7, 'cm')) + 
        geom_magnify(from = c(8.87, 9.05, 39.7, 39.8),  
                 to   = c(9.2, 9.8, 39.14, 39.46),  
                 shadow = TRUE) +
        geom_text(data = region_prov_shp, aes(x = x, y = y, label = Prov), 
            size = 3, color = "black", fontface = "bold", nudge_x = -0.1, nudge_y = 0.05)

p0


p1  <- 
  ggplot(region_shp) +
  geom_sf() +
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "No buffer",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio1_buffer0km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0),
        legend.key.size = unit(0.5, 'cm'))


p2  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_5km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 5 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio1_buffer5km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence","Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
        plot.title = element_text(size = 10, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title = element_text(size = 12, face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        text = element_text(size = 12), 
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p3  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_10km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "", 
       subtitle = "Buffer: 10 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio1_buffer10km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p4  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_20km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 20 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio1_buffer20km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p5  <- 
  ggplot(region_shp) +
  geom_sf() +
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "No buffer",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio10_buffer0km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p6  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_5km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 5 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio10_buffer5km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p7  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_10km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 10 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio10_buffer10km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))


p8  <- 
  ggplot(region_shp) +
  geom_sf() +
  geom_sf(data = buffers_sf[["buffer_20km"]], linetype = "dotted", fill = "yellow", color = "darkblue", alpha = 0.2, size = 0.6) + 
  scale_fill_identity() +
  annotation_scale() +
  labs(title = "",
       subtitle = "Buffer: 20 km",
       x = "", y = "") +
  theme_light() +
  geom_point(data = all_datasets[["df_pb_ratio10_buffer20km"]], aes(x = x, y = y, color = factor(occ)), size = 1) +
  scale_color_manual(name = "", 
                     values = c("1" = "red", "0" = "darkslategrey"), 
                     labels = c("Pseudo-absence", "Presence")) +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background = element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = c(.75, 0.067),
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
        legend.text = element_text(size = 8, angle = 0), 
        legend.key.size = unit(0.5, 'cm'))



ptot1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "Ratio 1:1 (181 presences and 181 pseudo-absences)",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))


ptot2 <- p5 + p6 + p7 + p8  + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "Ratio 1:10 (181 presences and 1810 pseudo-absences)",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))


