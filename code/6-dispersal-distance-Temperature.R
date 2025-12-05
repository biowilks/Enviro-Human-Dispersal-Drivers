rm(list = ls())

# Load all packages ----------
library(sf)
library(terra)
library(tidyverse)

## Projections -------------
WGS84Proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweideProj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

## Import Data -------------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

load("dispersalbirds_sf_HR.Rda")
load("dispersalmammals_sf_HR.Rda")
load("dispersalinverts_sf_HR.Rda")
load("dispersalfish_sf_HR.Rda")

## Functions needed ---------
## CRS Transformation and Buffer - generalize the function to transform and buffer SF objects
prepare_sf_data <- function(sf_object, target_crs, buffer_distance) {
  sf_object %>%
    st_transform(target_crs) %>%  
    st_buffer(dist = buffer_distance)  
}

## Year Matching -----
match_closest_temperature_year <- function(yearStart, temperature_years) {
  if (yearStart <= 1981) return('1981')
  if (yearStart >= 2019) return('2019')
  return(as.character(temperature_years[which.min(abs(temperature_years - yearStart))])) 
}

#################################################################################################
###################################### AIR TEMPERATURE ##########################################
####################################  BIRDS, FISHES, MAMMALS   ####################################
#################################################################################################

## Mapping Temperature ----------
setwd("~/share/user/ENVIR-data/ChelsaClimate/temperature")
setwd("H:/ENVIR-data/ChelsaClimate/temperature")

temperature_rasters <- list.files(pattern = "mean_\\d{4}.tif$", full.names = TRUE) %>%
  set_names(str_extract(., "\\d{4}")) %>%
  map(terra::rast)

# Apply CRS transformation to all species groups
temperature_crs <- crs(temperature_rasters[['2000']])
disp_mammals_sf_HR_temp <- prepare_sf_data(disp_mammals_sf_HR, temperature_crs, disp_mammals_sf_HR$radius)
disp_birds_sf_HR_temp <- prepare_sf_data(disp_birds_sf_HR, temperature_crs, disp_birds_sf_HR$radius)
disp_inverts_sf_HR_temp <- prepare_sf_data(disp_inverts_sf_HR, temperature_crs, disp_inverts_sf_HR$radius)
disp_fish_sf_HR_temp <- prepare_sf_data(disp_fish_sf_HR, temperature_crs, disp_fish_sf_HR$radius)

# Convert buffered SF polygons to terra vector objects
vec_poly_mollweide_mammals_temp <- terra::vect(disp_mammals_sf_HR_temp$geometry)
vec_poly_mollweide_birds_temp <- terra::vect(disp_birds_sf_HR_temp$geometry)
vec_poly_mollweide_inverts_temp <- terra::vect(disp_inverts_sf_HR_temp$geometry)
vec_poly_mollweide_fish_temp <- terra::vect(disp_fish_sf_HR_temp$geometry)

# Plot example to verify
plot(temperature_rasters[['2000']], main = "Temperature Data (2000)")
plot(disp_fish_sf_HR_temp$geometry, add = TRUE, col = "transparent", border = "black", lwd = 2)

## Temperature Year Matching -----
extract_temperature_for_group <- function(sf_object, temperature_rasters) {
  temperature_years <- as.numeric(names(temperature_rasters))  
  
  sf_object <- sf_object %>%
    mutate(
      Temperature_mean = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        temp_index <- match_closest_temperature_year(year_start, temperature_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"])  
        temp_value <- terra::extract(temperature_rasters[[temp_index]], polygon)
        
        mean_temp <- mean(temp_value[, 2], na.rm = TRUE)/ 10 - 273.15 # Kelvin to Celsius
        return(mean_temp)
      }),
      Temperature_median = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        temp_index <- match_closest_temperature_year(year_start, temperature_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"]) 
        temp_value <- terra::extract(temperature_rasters[[temp_index]], polygon)
        
        median_temp <- median(temp_value[, 2], na.rm = TRUE)/ 10 - 273.15 # Kelvin to Celsius
        return(median_temp)
      })
    )
  
  return(sf_object)
}

disp_mammals_sf_temp <- extract_temperature_for_group(disp_mammals_sf_HR_temp, temperature_rasters)
disp_birds_sf_temp <- extract_temperature_for_group(disp_birds_sf_HR_temp, temperature_rasters)
disp_inverts_sf_temp <- extract_temperature_for_group(disp_inverts_sf_HR_temp, temperature_rasters)
disp_fish_sf_temp <- extract_temperature_for_group(disp_fish_sf_HR_temp, temperature_rasters)

## Save the Temperature results -----
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(disp_birds_sf_temp, file = "dispersalbirds_sf_temp.Rda")
save(disp_mammals_sf_temp, file = "dispersalmammals_sf_temp.Rda")
save(disp_inverts_sf_temp, file = "dispersalinverts_sf_temp.Rda")
save(disp_fish_sf_temp, file = "dispersalfish_sf_temp.Rda")

#################################################################################################
###################################### SOIL TEMPERATURE #########################################
####################################    INVERTEBRATES   #########################################
#################################################################################################
# Load data ----------
setwd("~/share/user/ENVIR-data/Soil_temp")

soil_temp_raster <- terra::rast("SBIO1_0_5cm_Annual_Mean_Temperature.tif")

# Apply CRS transformation  ----------
soil_temp_crs <- crs(soil_temp_raster)
disp_inverts_sf_HR_temp <- prepare_sf_data(disp_inverts_sf_HR, soil_temp_crs, disp_inverts_sf_HR$radius)

# Convert buffered SF polygons to terra vector objects
vec_poly_inverts_temp <- terra::vect(disp_inverts_sf_HR_temp$geometry)

# Plot example to verify
plot(soil_temp_raster, main = "Soil Temperature Data")
plot(disp_inverts_sf_HR_temp$geometry, add = TRUE, col = "transparent", border = "black", lwd = 2)

# Extract soil temperature (mean and median)
disp_inverts_sf_temp <- disp_inverts_sf_HR_temp %>%
  mutate(
    Temperature_mean = purrr::map_dbl(1:nrow(.), function(i) {
      polygon <- terra::vect(.[i, "geometry"])
      temp_value <- terra::extract(soil_temp_raster, polygon)
      mean(temp_value[, 2], na.rm = TRUE)  # Already in Celsius
    }),
    Temperature_median = purrr::map_dbl(1:nrow(.), function(i) {
      polygon <- terra::vect(.[i, "geometry"])
      temp_value <- terra::extract(soil_temp_raster, polygon)
      median(temp_value[, 2], na.rm = TRUE)
    })
  )

# Save the output
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(disp_inverts_sf_temp, file = "dispersalinverts_sf_temp.Rda")

