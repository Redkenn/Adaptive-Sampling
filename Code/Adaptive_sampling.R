library(raster)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggmap)
library(tidyterra)
library(ggrepel)
library(ggspatial)
library(geosphere)
#library(GeoThinneR)

# Read prediction map (pred_map_spat that works well with tidyterra for plotting)
pred_map <- raster("Adaptive_Sampling/Results/df_pb_ratio1_buffer20km/predictions_raster.tif")
pred_map_spat <- rast("Adaptive_Sampling/Results/df_pb_ratio1_buffer10km/predictions_raster.tif") 


# Convert raster to dataframe with coordinates
df <- as.data.frame(rasterToPoints(pred_map))
df <- df[!is.na(df[, 3]), ]  

# Convert the raster to a dataframe with coordinates
colnames(df) <- c("x", "y", "Probability")

# Definition of probability classes
df <- df %>%
  mutate(prob_class = cut(Probability, 
                          breaks = c(0, 0.4, 0.6, 0.8, 1),
                          labels = c("Low", "Medium", "High", "Very High")))


# Transform dataframe into shp data 
df_shp  <- st_as_sf(df , coords = c('x', 'y'))
df_shp <- st_set_crs(df_shp, 4326)
df_shp <- df_shp %>% st_transform(32632)

# Select df with High habitat suitability
df_VH <- df_shp[df_shp$prob_class == "Very High",]

# Remove unusable data to free up memory
rm(df_shp)


# Load study area shapefile
region_shp <- st_read("Adaptive_Sampling/Study_area/limiteAmministrRegionale.shp") %>% st_transform(32632)


# Create a grid of the map of the Sardinian borders
g <- region_shp %>%
  st_make_grid(cellsize = 10000) %>%
  st_intersection(region_shp) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())


# Read presence points
df_pr <- st_read("Adaptive_Sampling/Sample_points/ponds.shp") %>% st_transform(32632)
df_pr <- df_pr[, 'geometry']
df_pr$occ <- 1 



# Join presence shp data with our grid 
g_pr <- st_join(g, df_pr)

# Replace NA with 0 to indicate no habitat in the cell 
g_pr$occ <- g_pr$occ %>% replace_na(0)

# Select only the maximum value for each cell to avoid repeated IDs
g_pr <- g_pr %>%
  group_by(id) %>% 
  summarise(occ = max(occ))


# Select indexes (id) with Probability > 0
presence_ids <- g_pr %>%
  filter(occ == 1) %>%
  pull(id)


p1  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  #geom_sf()+
  #scale_fill_viridis('occ',option='viridis',direction = 1)+
  annotation_scale() +
  scale_fill_manual(name = "Habitat", 
                     values = c("1" = "red", "0" = "white"), 
                     labels = c("Absence", "Presence")) +
  labs(title = "", subtitle = "Starting data", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))  


p2  <- 
  ggplot() +
  geom_spatraster(data = pred_map_spat, aes(fill = focal_mean)) +
  geom_sf() +
  scale_fill_viridis_c(option = "rocket", direction = -1, limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.value = "powderblue") +
  annotation_scale() +
  labs(title = "",
       subtitle = "Best predictive map",
       x = "", y = "") +
  labs(fill = "Habitat Probability") + 
  theme_light() +
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm')) 


# Join probability shp data with our grid 
g_df_VH <- st_join(g, df_VH)


# Replace NA with 0 to indicate no potential habitat in the cell 
g_df_VH$Probability <- g_df_VH$Probability %>% replace_na(0)

# Select only the maximum value for each cell to avoid repeated id
g_df_VH <- g_df_VH %>%
  group_by(id) %>% 
  summarise(Probability = max(Probability))


# Select indexes (id) with Probability > 0
potential_ids <- g_df_VH %>%
  filter(Probability > 0) %>%
  pull(id)


# Remove the id present in presence_ids from vector potential_ids
id_filtered <- setdiff(potential_ids, presence_ids)



# Assigns the value 2 to the “occ” column for selected indexes
# The value 2 is used to indicate only the presence of potential habitat
g_pr <- g_pr %>%
  mutate(occ = ifelse(id %in% id_filtered, 2, occ))


p3  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  labs(title = "", subtitle = "Potential Habitat (Prob > 0.8)", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))



# Join df_VH data with g_pr 
df_VH_g_pr <- st_join(df_VH,g_pr)


# Select only rows with id present in id_filtered
filtered_df <- df_VH_g_pr %>%
  filter(id %in% id_filtered)


# Extract x and y coordinates
coordinates <- st_coordinates(filtered_df)


# Add id x and y columns to dataframe
final_df <- data.frame(
  id = filtered_df$id,
  Probability = filtered_df$Probability,
  x = coordinates[, 1],  
  y = coordinates[, 2]   
)


# Randomly select 3 points for each distinct id
set.seed(123)
sample_points <- final_df %>%
  group_by(id) %>%
  sample_n(size = min(3, n()), replace = FALSE) %>%
  ungroup()



p4  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  geom_point(data = sample_points,  
    aes(x = x, y = y, color = "Sample points"),
    color = "black",
    size = 1,           
    shape = 16) +  
  labs(title = "", subtitle = "Adaptive sampling", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))




ptot1 <- p1 + p2 + p3 + p4 + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))

ptot1
 

# Approximate coordinates of Sardinia
xmid <- mean(range(sample_points$x))
ymid <- mean(range(sample_points$y))


# Function to assign geographical area
sample_points <- sample_points %>%
  mutate(Zone = case_when(
    x <= xmid & y > ymid ~ "North-West",
    x >  xmid & y > ymid ~ "North-East",
    x <= xmid & y <= ymid ~ "South-West",
    x >  xmid & y <= ymid ~ "South-East"
  )) %>%
  mutate(ID = paste0("P", row_number()))



p5  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  geom_point(data = sample_points[sample_points$Zone == "North-West",], 
             aes(x = x, y = y, color = "Sample points"),
             color = "black",
             size = 1,           
             shape = 16) +  
  labs(title = "", subtitle = "Adaptive sampling (Round 1)", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))


p6  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  geom_point(data = sample_points[sample_points$Zone == "North-East",], 
             aes(x = x, y = y, color = "Sample points"),
             color = "black",
             size = 1,           
             shape = 16) +  
  labs(title = "", subtitle = "Adaptive sampling (Round 2)", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))


p7  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  geom_point(data = sample_points[sample_points$Zone == "South-West",], 
             aes(x = x, y = y, color = "Sample points"),
             color = "black",
             size = 1,           
             shape = 16) +  
  labs(title = "", subtitle = "Adaptive sampling (Round 3)", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))


p8  <- 
  ggplot(g_pr)+
  geom_sf(aes(fill = factor(occ)))+
  scale_fill_manual(name = "Habitat", 
                    values = c("1" = "red", "0" = "white", "2" = "orange"), 
                    labels = c("Absence", "Presence", "Potential")) +
  annotation_scale() +
  geom_point(data = sample_points[sample_points$Zone == "South-East",], 
             aes(x = x, y = y, color = "Sample points"),
             color = "black",
             size = 1,           
             shape = 16) +  
  labs(title = "", subtitle = "Adaptive sampling (Round 4)", x="", y="", fill = "occ") +theme_light()+
  theme(panel.background = element_rect(fill = "powderblue"),
        legend.background=element_blank(),
        legend.key = element_rect(colour = "transparent",fill = "transparent"),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size=15,face = 'bold',hjust = 0.5),
        plot.subtitle = element_text(size = 10, face = 'bold'),
        legend.title=element_text(size=10,face = 'bold'),
        axis.title.x = element_text(size=14,face = 'bold'),
        axis.text.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size=14,face = 'bold'),
        axis.text.y = element_text(size = 10, face = 'bold'),
        axis.ticks.y=element_blank(),
        text = element_text(size=12), 
        strip.text = element_text(size=12),
        legend.text = element_text(size=7,angle = 0), 
        legend.key.size = unit(0.4, 'cm'))


ptot2 <- p5 + p6 + p7 + p8 + plot_layout(nrow = 1, ncol = 4) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 15, hjust = 0.5)))

ptot2


# Converts sample points df to sf objects
# Check that sample points and presence points are in EPSG:4326 (lat/lon in decimal degrees).
sp_sf <- st_as_sf(sample_points, coords = c("x", "y"))
sp_sf <- st_set_crs(sp_sf, 32632)
sp_sf <- sp_sf %>% st_transform(4326)

df_pr <- df_pr %>% st_transform(4326)


# Calculates the minimum distance (in meters) from each candidate point to all points of presence
dist_matrix <- geosphere::distm(st_coordinates(sp_sf), st_coordinates(df_pr))


# Minimum distance for each candidate point (in km).
min_dist_km <- apply(dist_matrix, 1, min) / 1000

# Add column "Distance_to_known" to original dataframe
sample_points$Distance_to_known <- round(min_dist_km, 2)


sample_points <- sample_points %>%
  mutate(Round = case_when(
    Zone == "North-West"  ~ "Round 1",
    Zone == "North-East"  ~ "Round 2",
    Zone == "South-West"  ~ "Round 3",
    Zone == "South-East"  ~ "Round 4"
  )) %>%
  mutate(Access_notes = "") %>% 
  mutate(Sampling_status = "")
 

sample_table <- sample_points %>%
  rename(ID_cell = id,
         ID_point = ID,
         Longitude = x,
         Latitude = y,
         Probability = Probability,
         Quadrants = Zone) %>%
  select(ID_cell, ID_point, Quadrants, Probability, Longitude, Latitude, Distance_to_known, Round, Access_notes, Sampling_status) %>%
  arrange(Round,desc(Probability))


# Seleziona le prime 17 righe
sample_subset <- sample_table[1:17, ]

