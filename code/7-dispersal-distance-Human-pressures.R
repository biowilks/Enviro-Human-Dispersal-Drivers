rm(list=ls())

# Load all packages ----------
library(sf)
library(terra)
library(tidyverse)


# Different projections -------------
WGS84Proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweideProj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Import data -------------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

load("dispersalbirds_sf_HR.Rda")
load("dispersalmammals_sf_HR.Rda")
load("dispersalinverts_sf_HR.Rda")
load("dispersalfish_sf_HR.Rda")

#################################################################################################
################################# HUMAN FOOTPRINT INDEX #########################################
############################  BIRDS, INVERTEBRATES, MAMMALS   ###################################
#################################################################################################
## Mapping HFI ----------
setwd("~/share/user/ENVIR-data/HFI")
setwd("H:/ENVIR-data/HFI")

hfi_rasters <- list.files(pattern = "hfp\\d{4}.tif$", full.names = TRUE) %>%
  set_names(regmatches(., gregexpr("\\d{4}", .))) %>%
  #set_names(str_extract(., "\\d{4}")) %>%
  map(terra::rast)

## CRS transformation and buffer -----
prepare_sf_data <- function(sf_object, crs_target, buffer_distance) {
  st_transform(sf_object, crs_target) %>%
    st_buffer(dist = buffer_distance)  
}

# Apply to all groups
disp_mammals_sf_HR_HFI <- prepare_sf_data(disp_mammals_sf_HR, crs(hfi_rasters[['2019']]), disp_mammals_sf_HR$radius)
disp_birds_sf_HR_HFI <- prepare_sf_data(disp_birds_sf_HR, crs(hfi_rasters[['2019']]), disp_birds_sf_HR$radius)
disp_inverts_sf_HR_HFI <- prepare_sf_data(disp_inverts_sf_HR, crs(hfi_rasters[['2019']]), disp_inverts_sf_HR$radius)
disp_fish_sf_HR_HFI <- prepare_sf_data(disp_fish_sf_HR, crs(hfi_rasters[['2019']]), disp_fish_sf_HR$radius)

# Convert buffered polygons to terra vector objects
vec_poly_mollweide_mammals <- terra::vect(disp_mammals_sf_HR_HFI$geometry)
vec_poly_mollweide_birds <- terra::vect(disp_birds_sf_HR_HFI$geometry)
vec_poly_mollweide_inverts <- terra::vect(disp_inverts_sf_HR_HFI$geometry)
vec_poly_mollweide_fish <- terra::vect(disp_fish_sf_HR_HFI$geometry)

# Plot example to verify
plot(hfi_rasters[['2019']], main = "HFI Data (2019)", col = terrain.colors(10))
plot(disp_mammals_sf_HR_HFI$geometry, add = TRUE, col = "transparent", border = "black", lwd = 2)

# HFI Year Matching -----
hfi_years <- as.numeric(names(hfi_rasters))

match_closest_hfi_year <- function(yearStart, hfi_years) {
  if (yearStart < 1995) return('1993')
  if (yearStart < 2000) return('2000')
  return(as.character(hfi_years[which.min(abs(hfi_years - yearStart))]))
}

extract_hfi_for_group <- function(sf_object, hfi_rasters) {
  hfi_years <- as.numeric(names(hfi_rasters))
  
  sf_object <- sf_object %>%
    dplyr::mutate(
      HFImean = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        hfi_index <- match_closest_hfi_year(year_start, hfi_years)

        polygon <- terra::vect(sf_object[i, "geometry"])
        hfi_value <- terra::extract(hfi_rasters[[hfi_index]], polygon)
        
        mean_hfi <- mean(hfi_value[, 2], na.rm = TRUE)
        
        return(mean_hfi)
      }),
      HFImedian = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        hfi_index <- match_closest_hfi_year(year_start, hfi_years)
        
        polygon <- terra::vect(sf_object[i, "geometry"]) 
        hfi_value <- terra::extract(hfi_rasters[[hfi_index]], polygon)
        
        median_hfi <- median(hfi_value[, 2], na.rm = TRUE)
        
        return(median_hfi)
      }),
      HFIsd = purrr::map_dbl(1:nrow(sf_object), function(i) {
        year_start <- sf_object$yearStart[i]
        
        hfi_index <- match_closest_hfi_year(year_start, hfi_years)
      
        polygon <- terra::vect(sf_object[i, "geometry"])  
        hfi_value <- terra::extract(hfi_rasters[[hfi_index]], polygon)
        
        sd_hfi <- sd(hfi_value[, 2], na.rm = TRUE)
        
        return(sd_hfi)
      })
    )
  
  return(sf_object)
}


# Apply extraction and year matching for each group
disp_mammals_sf_HFI <- extract_hfi_for_group(disp_mammals_sf_HR_HFI, hfi_rasters)
disp_birds_sf_HFI <- extract_hfi_for_group(disp_birds_sf_HR_HFI, hfi_rasters)
disp_inverts_sf_HFI <- extract_hfi_for_group(disp_inverts_sf_HR_HFI, hfi_rasters)
disp_fish_sf_HFI <- extract_hfi_for_group(disp_fish_sf_HR_HFI, hfi_rasters)

## Saving the results -----
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(disp_birds_sf_HFI, file = "dispersalbirds_sf_HFI.Rda")
save(disp_mammals_sf_HFI, file = "dispersalmammals_sf_HFI.Rda")
save(disp_inverts_sf_HFI, file = "dispersalinverts_sf_HFI.Rda")
save(disp_fish_sf_HFI, file = "dispersalfish_sf_HFI.Rda")


#################################################################################################
################################# FRAGMENTATION STATUS INDEX ####################################
############################                FISHES            ###################################
#################################################################################################
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")
load("dispersalfish_sf_HR.Rda")

load("~/share/user/ENVIR-data/River_connectivity_index/disp_fish_CSI.Rda")

## Connectivity status index (CSI) ----------
gdb_river <- "~/share/user/ENVIR-data/River_connectivity_index/FFR_river_network.gdb"
st_layers(gdb_river)
river_reach_layer <- st_read(gdb_river, layer = "FFR_river_network_v1") # From Grill et al. 2019
river_CSI <- river_reach_layer %>%
  dplyr::select(GOID, CONTINENT, COUNTRY, CSI, LENGTH_KM,VOLUME_TCM,Shape)
setwd("~/share/user/ENVIR-data/River_connectivity_index/")
save(river_CSI, file = "disp_fish_CSI.Rda")


# Calculate Fragmentation Status Index  (FSI) following Schipper & Barbarossa (2021) ----------
river_CSI <- river_CSI %>%
  dplyr::mutate(FSI = 100 - CSI)  # The CSI varies between 0 and 100 (larger values means higher connectivity)
# FSI = 100 – CSI, higher value represents more human influence

# Adding buffer
disp_fish_sf_buffered <- st_buffer(disp_fish_sf_HR, dist = 1000) # 1 km buffer to get more data 
disp_fish_sf_buffered  <- st_transform(disp_fish_sf_buffered, crs = st_crs(river_CSI))
disp_fish_FSI <- st_join(disp_fish_sf_buffered, river_CSI, join = st_intersects)


sum(is.na(disp_fish_FSI$FSI))

disp_fish_FSI <- disp_fish_FSI %>%
  filter(!is.na(FSI)) %>%
  distinct(Event.ID, .keep_all = TRUE)  # 532 obs


setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(disp_fish_FSI, file = "disp_fish_FSI.Rda")
