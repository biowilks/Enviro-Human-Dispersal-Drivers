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

## NDVI Year Matching -----
match_closest_ndvi_year <- function(yearStart, ndvi_years) {
  if (yearStart <= 1981) return('1981')
  if (yearStart >= 2021) return('2021')
  return(as.character(ndvi_years[which.min(abs(ndvi_years - yearStart))])) 
}



#################################################################################################
###################################### PRODUCTIVITY #############################################
####################################     ALL TAXA   #############################################
#################################################################################################

## Mapping NDVI mean ----------
setwd("~/share/user/ENVIR-data/NDVI_mean")
setwd("H:/ENVIR-data/NDVI_mean")

# Using base R to extract the years from file names
ndvi_mean_rasters <- list.files(pattern = "NDVI_y\\d{4}_mean\\.tif$", full.names = TRUE) %>%
  set_names(regmatches(., gregexpr("\\d{4}", .))) %>%
  map(function(file) {
    raster <- terra::rast(file)
    raster[raster < 0] <- NA  # Set values < 0 to NA - remove the ocean
    return(raster)
  })


# ndvi_mean_rasters <- list.files(pattern = "NDVI_y\\d{4}_mean\\.tif$", full.names = TRUE) %>%
#   set_names(str_extract(., "\\d{4}")) %>%
#   map(function(file) {
#     raster <- terra::rast(file)
#     raster[raster < 0] <- NA  # Set values < 0 to NA - remove the ocean
#   })

# Apply CRS transformation to all species groups
ndvi_mean_crs <- crs(ndvi_mean_rasters[['2000']])
disp_mammals_sf_HR_ndvim <- prepare_sf_data(disp_mammals_sf_HR, ndvi_mean_crs, disp_mammals_sf_HR$radius)
disp_birds_sf_HR_ndvim <- prepare_sf_data(disp_birds_sf_HR, ndvi_mean_crs, disp_birds_sf_HR$radius)
disp_inverts_sf_HR_ndvim <- prepare_sf_data(disp_inverts_sf_HR, ndvi_mean_crs, disp_inverts_sf_HR$radius)
disp_fish_sf_HR_ndvim <- prepare_sf_data(disp_fish_sf_HR, ndvi_mean_crs, disp_fish_sf_HR$radius)

# Convert buffered SF polygons to terra vector objects
vec_poly_mollweide_mammals_ndvim  <- terra::vect(disp_mammals_sf_HR_ndvim$geometry)
vec_poly_mollweide_birds_ndvim  <- terra::vect(disp_birds_sf_HR_ndvim$geometry)
vec_poly_mollweide_inverts_ndvim  <- terra::vect(disp_inverts_sf_HR_ndvim$geometry)
vec_poly_mollweide_fish_ndvim  <- terra::vect(disp_fish_sf_HR_ndvim$geometry)

# Plot example to verify
plot(ndvi_mean_rasters[['2000']], main = "NDVI Data (2000)")
plot(disp_fish_sf_HR_ndvim$geometry, add = TRUE, col = "transparent", border = "black", lwd = 2)

## NDVImean Year Matching -----
extract_ndvim_for_group <- function(sf_object, ndvi_mean_rasters) {
  ndvi_years <- as.numeric(names(ndvi_mean_rasters))  
  
  sf_object <- sf_object %>%
    dplyr::mutate(
      NDVImean = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        ndvi_index <- match_closest_ndvi_year(year_start, ndvi_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"])  
        ndvi_value <- terra::extract(ndvi_mean_rasters[[ndvi_index]], polygon)
        
        mean_ndvi <- mean(ndvi_value[, 2], na.rm = TRUE)
        return(mean_ndvi)
      }),
      NDVImedian = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        ndvi_index <- match_closest_ndvi_year(year_start, ndvi_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"]) 
        ndvi_value <- terra::extract(ndvi_mean_rasters[[ndvi_index]], polygon)
        
        median_ndvi <- median(ndvi_value[, 2], na.rm = TRUE)
        return(median_ndvi)
      })
    )
  
  return(sf_object)
}

disp_mammals_sf_NDVImean <- extract_ndvim_for_group(disp_mammals_sf_HR_ndvim, ndvi_mean_rasters)
disp_birds_sf_NDVImean <- extract_ndvim_for_group(disp_birds_sf_HR_ndvim, ndvi_mean_rasters)
disp_inverts_sf_NDVImean <- extract_ndvim_for_group(disp_inverts_sf_HR_ndvim, ndvi_mean_rasters)
disp_fish_sf_NDVImean <- extract_ndvim_for_group(disp_fish_sf_HR_ndvim, ndvi_mean_rasters)

## Save the NDVI results -----
setwd("~/share/user/Movement-Database-ENVIR/output/")
setwd("H:/Movement-Database-ENVIR/output/")

save(disp_birds_sf_NDVImean, file = "dispersalbirds_sf_NDVImean.Rda")
save(disp_mammals_sf_NDVImean, file = "dispersalmammals_sf_NDVImean.Rda")
save(disp_inverts_sf_NDVImean, file = "dispersalinverts_sf_NDVImean.Rda")
save(disp_fish_sf_NDVImean, file = "dispersalfish_sf_NDVImean.Rda")




#################################################################################################
###################################### SEASONALITY  #############################################
####################################     ALL TAXA   #############################################
#################################################################################################

## Mapping NDVI mean ----------
setwd("~/share/user/ENVIR-data/NDVI_sd")
setwd("H:/ENVIR-data/NDVI_sd")

# ndvi_sd_rasters <- list.files(pattern = "NDVI_y\\d{4}_sd\\.tif", full.names = TRUE) %>%
#   set_names(str_extract(., "\\d{4}")) %>%
#   map(function(file) {
#     raster <- terra::rast(file)
#     raster[raster < 0] <- NA  # Set values < 0 to NA - as this is the sea
#     return(raster)
#   })

#Using base R to extract the years from file names for ndvi_sd_rasters
ndvi_sd_rasters <- list.files(pattern = "NDVI_y\\d{4}_sd\\.tif", full.names = TRUE) %>%
  set_names(regmatches(., gregexpr("\\d{4}", .))) %>%
  map(function(file) {
    raster <- terra::rast(file)
    raster[raster < 0] <- NA  # Set values < 0 to NA - as this is the sea
    return(raster)
  })

# Apply CRS transformation to all species groups
ndvi_sd_crs <- crs(ndvi_sd_rasters[['2000']])
disp_mammals_sf_HR_ndvisd <- prepare_sf_data(disp_mammals_sf_HR, ndvi_sd_crs, disp_mammals_sf_HR$radius)
disp_birds_sf_HR_ndvisd <- prepare_sf_data(disp_birds_sf_HR, ndvi_sd_crs, disp_birds_sf_HR$radius)
disp_inverts_sf_HR_ndvisd <- prepare_sf_data(disp_inverts_sf_HR, ndvi_sd_crs, disp_inverts_sf_HR$radius)
disp_fish_sf_HR_ndvisd <- prepare_sf_data(disp_fish_sf_HR, ndvi_sd_crs, disp_fish_sf_HR$radius)

# Convert buffered SF polygons to terra vector objects
vec_poly_mollweide_mammals_ndvisd  <- terra::vect(disp_mammals_sf_HR_ndvisd$geometry)
vec_poly_mollweide_birds_ndvisd  <- terra::vect(disp_birds_sf_HR_ndvisd$geometry)
vec_poly_mollweide_inverts_ndvisd  <- terra::vect(disp_inverts_sf_HR_ndvisd$geometry)
vec_poly_mollweide_fish_ndvisd  <- terra::vect(disp_fish_sf_HR_ndvisd$geometry)

# Plot example to verify
plot(ndvi_sd_rasters[['2000']], main = "NDVI SD Data (2000)")
plot(disp_fish_sf_HR_ndvisd$geometry, add = TRUE, col = "transparent", border = "black", lwd = 2)

## NDVIsd Year Matching -----
extract_ndvi_sd_for_group <- function(sf_object, ndvi_sd_rasters) {
  ndvi_years <- as.numeric(names(ndvi_sd_rasters))
  
  sf_object <- sf_object %>%
    dplyr::mutate(
      NDVIsd_mean = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        ndvi_index <- match_closest_ndvi_year(year_start, ndvi_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"]) 
        ndvi_value <- terra::extract(ndvi_sd_rasters[[ndvi_index]], polygon)
        
        mean_ndvi_sd <- mean(ndvi_value[, 2], na.rm = TRUE)
        return(mean_ndvi_sd)
      }),
      
      NDVIsd_median = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        ndvi_index <- match_closest_ndvi_year(year_start, ndvi_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"])  
        ndvi_value <- terra::extract(ndvi_sd_rasters[[ndvi_index]], polygon)
        
        median_ndvi_sd <- median(ndvi_value[, 2], na.rm = TRUE)
        return(median_ndvi_sd)
      })
    )
  
  return(sf_object)
}


disp_mammals_sf_NDVIsd <- extract_ndvi_sd_for_group(disp_mammals_sf_HR_ndvisd, ndvi_sd_rasters)
disp_birds_sf_NDVIsd <- extract_ndvi_sd_for_group(disp_birds_sf_HR_ndvisd, ndvi_sd_rasters)
disp_inverts_sf_NDVIsd <- extract_ndvi_sd_for_group(disp_inverts_sf_HR_ndvisd, ndvi_sd_rasters)
disp_fish_sf_NDVIsd <- extract_ndvi_sd_for_group(disp_fish_sf_HR_ndvisd, ndvi_sd_rasters)

## Save the NDVI SD results -----
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(disp_birds_sf_NDVIsd, file = "dispersalbirds_sf_NDVIsd.Rda")
save(disp_mammals_sf_NDVIsd, file = "dispersalmammals_sf_NDVIsd.Rda")
save(disp_inverts_sf_NDVIsd, file = "dispersalinverts_sf_NDVIsd.Rda")
save(disp_fish_sf_NDVIsd, file = "dispersalfish_sf_NDVIsd.Rda")

