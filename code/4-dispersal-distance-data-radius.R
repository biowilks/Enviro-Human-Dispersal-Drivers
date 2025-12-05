rm(list=ls())

# Load all packages ----------
library(sf)
library(terra)
library(tidyverse)

## Different projections -------------
WGS84Proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweideProj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Import data -------------
setwd("H:/Movement-Database-ENVIR/output")
setwd("~/share/user/Movement-Database-ENVIR/output/")
load("distance_dispersal_converted.Rda")

# Filter data for each group
mammals <- db_final_filtered  |>
  filter(Taxa.category == "Mammal") 

birds <- db_final_filtered  |>
  filter(Taxa.category == "Bird") 

fish <- db_final_filtered  |>
  filter(Taxa.category == "Fish") 

inverts <- db_final_filtered  |>
  filter(Taxa.category == "Invertebrate") 

# Radius calculation using allometries ---------
# mass is in g and home range in m2

# Mammal home range allometry (Tamburello)
# Carnivore - log10(a): 1.66 and b: 1.36 
# Herbivore - log10(a): 1.32 and b: 1.09
mammals <- mammals |>
  mutate(
    # Calculate home range based on Combine trophic level (carnivore or herbivore)
    home_range_calculated = case_when(
      `Trophic.Level(COMBINE)` == "Carnivore" ~ (10^1.66) * Body.mass^1.36,  # Carnivore formula
      `Trophic.Level(COMBINE)` == "Herbivore" ~ (10^1.32) * Body.mass^1.09,  # Herbivore formula
      `Trophic.Level(COMBINE)` == "Omnivore" ~ (10^1.32) * Body.mass^1.09,   # Treat Omnivore as Herbivore for calculation
      TRUE ~ (10^1.32) * Body.mass^1.09  # Default to herbivore if trophic level is NA
    )
  ) |>
  # Calculate the radius of the home range
  mutate(
    radius = sqrt(home_range_calculated / pi)
  )


# Bird home range allometry (Tamburello) - flying only (running also in paper)
# Flying: Carnivore - log10(a): 2.79 and b: 1.5
# Flying: Herbivore - log10(a): 1.63 and b: 1.77 

birds <- birds |>
  mutate(
    # Calculate home range for carnivores, all others default to herbivore values
    home_range_birds = case_when(
      `Trophic.Level(AvoNET)` == "Carnivore" ~ (10^2.79) * Body.mass^1.5,   # Flying carnivore
      TRUE                                  ~ (10^1.63) * Body.mass^1.77    # Default to flying herbivore (includes herbivore, omnivore, NA)
    )
  ) |>
  # Calculate the radius of the home range
  mutate(
    radius = sqrt(home_range_birds / pi)
  )


# Fish home range allometry (Tamburello)
# River Carnivore - log10(a): 1.33 and b: 0.63 *
# Lake Carnivore - log10(a): 1.85 and b: 0.99 
# Marine Carnivore - log10(a): 1.1 and b: 0.82 
# Marine Herbivore - log10(a): -0.03 and b: 1.21 
fish <- fish |>
  mutate(
    home_range_fish = case_when(
      `Habitat(FishBase)` == "Freshwater" ~ (10^1.33) * Body.mass^0.63,  # Freshwater as River Carnivore
      `Habitat(FishBase)` == "Brackish" ~ (10^1.1) * Body.mass^0.82,     # Brackish as Marine Carnivore
      is.na(`Habitat(FishBase)`) ~ (10^1.33) * Body.mass^0.63,            # NA habitat as River Carnivore
      TRUE ~ (10^1.33) * Body.mass^0.63  # Default to River Carnivore
    )
  ) |>
  # Calculate the radius of the home range
  mutate(
    radius = sqrt(home_range_fish / pi)
  )


# Invert home range (Auger 2023) - for all orders (separate orders in paper)
# All: log10(a): 0.46 and b: 0.35 *
inverts <- inverts |>
  mutate(home_range_inverts = (10^0.46) * Body.mass^0.35) |>
  mutate(radius = sqrt(home_range_inverts / pi))


# Transform to sf -------
# Mammals
mammals_sf <- st_as_sf(mammals, coords = c("startLongitude", "startLatitude"), crs = WGS84Proj)
mammals_sf <- st_transform(mammals_sf, mollweideProj)
disp_mammals_sf_HR <- st_buffer(mammals_sf, dist = mammals_sf$radius)

# Birds
birds_sf <- st_as_sf(birds, coords = c("startLongitude", "startLatitude"), crs = WGS84Proj)
birds_sf <- st_transform(birds_sf, mollweideProj)
disp_birds_sf_HR <- st_buffer(birds_sf, dist = birds_sf$radius)

# Fish
fish_sf <- st_as_sf(fish, coords = c("startLongitude", "startLatitude"), crs = WGS84Proj)
fish_sf <- st_transform(fish_sf, mollweideProj)
disp_fish_sf_HR <- st_buffer(fish_sf, dist = fish_sf$radius)

# inverts
inverts_sf <- st_as_sf(inverts, coords = c("startLongitude", "startLatitude"), crs = WGS84Proj)
inverts_sf <- st_transform(inverts_sf, mollweideProj)
disp_inverts_sf_HR <- st_buffer(inverts_sf, dist = inverts_sf$radius)


##### Save datasets #####
# Saving the data with the home ranges in `sf` format
save(disp_fish_sf_HR, file = "dispersalfish_sf_HR.Rda")
save(disp_birds_sf_HR, file = "dispersalbirds_sf_HR.Rda")
save(disp_mammals_sf_HR, file = "dispersalmammals_sf_HR.Rda")
save(disp_inverts_sf_HR, file = "dispersalinverts_sf_HR.Rda")


