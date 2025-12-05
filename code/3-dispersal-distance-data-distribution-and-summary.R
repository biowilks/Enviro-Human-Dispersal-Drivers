rm(list=ls())

# Load all packages ----------
library(sf)
library(tidyverse)
library(rnaturalearth)

# Different projections ----------
WGS84Proj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweideProj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# WGS84Proj: Standard latitude-longitude projection.
# mollweideProj: A Mollweide projection, which is an equal-area projection. 3D projection

# Import data -------------
load("~/share/user/Enviro-Human-Dispersal-Drivers/output/dispersal_distance_converted.Rda")

#################################################################################################
###################################### START LOCATIONS ##########################################
#################################################################################################

## Create sf data of the dispersal_dist data with longlat spatial reference (WGS84)
dispersal_dist_sf <- st_as_sf(db_final_filtered, coords = c("startLongitude", "startLatitude"), crs = WGS84Proj)

## Apply a 1 km buffer (1000 meters)
dispersal_dist_sf_t <- st_transform(dispersal_dist_sf, mollweideProj)  # Transform to Mollweide for global comparison
dispersal_dist_sf_buffered <- st_buffer(dispersal_dist_sf_t, dist = 100)  # 100 m buffer

# Save the buffered sf object
save(dispersal_dist_sf_buffered, file="dispersal_dist_sf_buffered.Rda")

# Map of dispersal locations
# Create a color palette for the classes
class_colours <- c("Mammal" = "goldenrod", "Bird" = "tomato", "Fish" = "dodgerblue3",
                   "Invertebrate" = "darkorchid")

# Calculate the centroids of each polygon in the sf object
dispersal_centroids <- st_centroid(dispersal_dist_sf$geometry)
centroid_coords <- st_coordinates(dispersal_centroids)
centroid_df <- data.frame(
  Longitude = centroid_coords[, 1],
  Latitude = centroid_coords[, 2],
  Class = dispersal_dist_sf_buffered$Taxa.category
)

sample_sizes <- centroid_df %>%
  group_by(Class) %>%
  summarise(n = n())
class_labels <- paste0(sample_sizes$Class, " (n = ", sample_sizes$n, ")")

# World map
world_map <- ne_download(scale = 110, type = 'countries', category = 'cultural', returnclass = "sf")

ggplot() +
  geom_sf(data = world_map, fill = "gray70") +
  geom_point(data = centroid_df, aes(x = Longitude, y = Latitude, color = Class), size = 3) +
  scale_color_manual(values = class_colours, name = "class", labels = class_labels) +
  coord_sf(crs = st_crs(WGS84Proj), datum = sf::st_crs(WGS84Proj)) +  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(), 
    legend.text = element_text(size = 10),     
    axis.text = element_blank(),  
    axis.title = element_blank(),  
    axis.ticks = element_blank(),     
    panel.grid = element_blank()   
  )


ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/Figure_1_Dispersal_distance_distribution.png", height = 6, width = 12, dpi = 300, bg = "white")

# Data summary
summary_per_taxa <- db_final_filtered %>%
  group_by(Taxa.category) %>%
  summarise(
    Statistic = "Total",
    Observations = n(),
    Species = n_distinct(Species.ID),
    Families = n_distinct(Family.gbif),
    Classes = n_distinct(Class.gbif),
    Countries = n_distinct(Country[!is.na(Country)]),
    References = n_distinct(Original.ref[!is.na(Original.ref)]),
    MetaStudies = n_distinct(Meta.ref[!is.na(Meta.ref)]),
    .groups = "drop"
  )

summary_total_all <- db_final_filtered %>%
  group_by(Statistic) %>%
  summarise(
    Taxa.category = "All taxa",
    Observations = n(),
    Species = n_distinct(Species.ID),
    Families = n_distinct(Family.gbif),
    Classes = n_distinct(Class.gbif),
    Countries = n_distinct(Country[!is.na(Country)]),
    References = n_distinct(Original.ref[!is.na(Original.ref)]),
    MetaStudies = n_distinct(Meta.ref[!is.na(Meta.ref)]),
    .groups = "drop"
  ) %>%
  bind_rows(
    db_final_filtered %>%
      summarise(
        Taxa.category = "All taxa",
        Statistic = "Total",
        Observations = n(),
        Species = n_distinct(Species.ID),
        Families = n_distinct(Family.gbif),
        Classes = n_distinct(Class.gbif),
        Countries = n_distinct(Country[!is.na(Country)]),
        References = n_distinct(Original.ref[!is.na(Original.ref)]),
        MetaStudies = n_distinct(Meta.ref[!is.na(Meta.ref)]),
        .groups = "drop"
      )
  )

# Combine
summary_final <- bind_rows(summary_per_taxa, summary_total_all) %>%
  arrange(
    factor(Taxa.category, levels = c(unique(summary_per_taxa$Taxa.category), "All taxa")),
    factor(Statistic, levels = c("Maximum", "Mean", "Median", "Total"))
  ) %>%
  select(Taxa.category, Statistic, Observations, Species, Families, Classes, Countries, References, MetaStudies)

summary_final

