rm(list=ls())

# Load all packages ----------
library(tidyverse)
library(sf)

# Import data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

# Load 
load("dispersalbirds_sf_NDVImean.Rda")
load("dispersalmammals_sf_NDVImean.Rda")
load("dispersalinverts_sf_NDVImean.Rda")
load("dispersalfish_sf_NDVImean.Rda")

load("dispersalbirds_sf_NDVIsd.Rda")
load("dispersalmammals_sf_NDVIsd.Rda")
load("dispersalinverts_sf_NDVIsd.Rda")
load("dispersalfish_sf_NDVIsd.Rda")

load("dispersalbirds_sf_temp.Rda")
load("dispersalmammals_sf_temp.Rda")
load("dispersalinverts_sf_temp.Rda")
load("dispersalfish_sf_temp.Rda")

load("dispersalbirds_sf_HFI.Rda")
load("dispersalmammals_sf_HFI.Rda")
load("dispersalinverts_sf_HFI.Rda")
load("disp_fish_FSI.Rda")

# Function to select the necessary columns and the environmental variable
select_columns <- function(data, env_var, value_column) {
  data %>%
    dplyr::select(
      Event.ID, Species.ID, Species.ID.gbif, Taxa.category, Class.gbif, Family.gbif,
      Movement.type, Movement.metric, Value, Units, Statistic, 
      Sampling.method.simple, yearStart, Locomotion.mode, Body.mass, 
      Country, Coordinates.added, Original.ref, Meta.ref, 
      `Trophic.guild (Auger)`, `Activity (Auger)`,  
      `foraging_stratum(COMBINE)`, `Trophic.Level(COMBINE)`, `Hand-Wing.Index(AvoNET)`,
      `Tarsus.Length(AvoNET)`, `Migration(AvoNET)`, `Habitat(AvoNET)`, 
      `Trophic.Level(AvoNET)`, `Habitat(FishBase)`, `Trophic.Level(FishBase)`, 
      `Diet.Troph.Level(FishBase)`, `Migration(FishBase)`, `Habitat.Preference(FishBase)`, 
      radius, 
      !!sym(env_var) := !!sym(value_column) # Rename the environmental variable column
    )
}



# Combine NDVImean for all taxa
combined_NDVImean <- bind_rows(
  select_columns(disp_birds_sf_NDVImean, "NDVImean", "NDVImean"),
  select_columns(disp_mammals_sf_NDVImean, "NDVImean", "NDVImean"),
  select_columns(disp_inverts_sf_NDVImean, "NDVImean", "NDVImean"),
  select_columns(disp_fish_sf_NDVImean, "NDVImean", "NDVImean")
)


# Combine NDVIsd for all taxa
combined_NDVIsd <- bind_rows(
  select_columns(disp_birds_sf_NDVIsd, "NDVIsd_mean", "NDVIsd_mean"),
  select_columns(disp_mammals_sf_NDVIsd, "NDVIsd_mean", "NDVIsd_mean"),
  select_columns(disp_inverts_sf_NDVIsd, "NDVIsd_mean", "NDVIsd_mean"),
  select_columns(disp_fish_sf_NDVIsd, "NDVIsd_mean", "NDVIsd_mean")
)

# Combine Temperature for all taxa
combined_temp <- bind_rows(
  select_columns(disp_birds_sf_temp, "Temperature_mean", "Temperature_mean"),
  select_columns(disp_mammals_sf_temp, "Temperature_mean", "Temperature_mean"),
  select_columns(disp_inverts_sf_temp, "Temperature_mean", "Temperature_mean"),
  select_columns(disp_fish_sf_temp, "Temperature_mean", "Temperature_mean")
)

combined_HFI <- bind_rows(
  select_columns(disp_birds_sf_HFI, "HFImean", "HFImean"),
  select_columns(disp_mammals_sf_HFI, "HFImean", "HFImean"),
  select_columns(disp_inverts_sf_HFI, "HFImean", "HFImean")
)

# Function to clean the data
clean_combined_data <- function(data, value_column) {
  data %>%
    filter(
      !is.na(!!sym(value_column)) & 
        !is.infinite(!!sym(value_column)) &  
        !is.nan(!!sym(value_column)) &
        !!sym(value_column) > 0  
    )
}

# Clean the data for each environmental variable
clean_NDVImean <- clean_combined_data(combined_NDVImean, "NDVImean")
clean_NDVIsd <- clean_combined_data(combined_NDVIsd, "NDVIsd_mean")
clean_temp <- clean_combined_data(combined_temp, "Temperature_mean")
clean_HFI <- clean_combined_data(combined_HFI, "HFImean")
clean_FSI <- clean_combined_data(disp_fish_FSI, "FSI")

########################################## Combining all datasets  ###################################
#####################################################################################################
clean_NDVImean <- st_drop_geometry(clean_NDVImean) 
clean_NDVIsd <- st_drop_geometry(clean_NDVIsd)
clean_temp <- st_drop_geometry(clean_temp) 
clean_HFI <- st_drop_geometry(clean_HFI) 
clean_FSI <- st_drop_geometry(clean_FSI)

# Combine clean data sets and select relevant variables for analysis
combined_data <- clean_NDVIsd  %>%
  left_join(clean_temp %>% dplyr::select(Event.ID, Temperature_mean), by = "Event.ID") %>%
  left_join(clean_HFI %>% dplyr::select(Event.ID, HFImean), by = "Event.ID") %>%
  left_join(clean_NDVImean %>% dplyr::select(Event.ID, NDVImean), by = "Event.ID") %>%
  left_join(clean_FSI %>% dplyr::select(Event.ID, FSI), by = "Event.ID") %>%
  distinct(Event.ID, .keep_all = TRUE) %>%
  dplyr::select(
    Event.ID, 
    Species.ID,
    Taxa.category,
    Family.gbif,
    Class.gbif,
    Sampling.method.simple,
    yearStart,
    Country,
    Original.ref,
    Meta.ref,
    Locomotion.mode, 
    `Trophic.guild (Auger)`, 
    `Trophic.Level(COMBINE)`, 
    `Migration(AvoNET)`, 
    `Trophic.Level(AvoNET)`, 
    `Trophic.Level(FishBase)`,
    `Diet.Troph.Level(FishBase)`,
    `Migration(FishBase)`,
    `Habitat.Preference(FishBase)`,
    Value,
    Statistic,
    Body.mass, 
    NDVImean,
    NDVIsd_mean,
    HFImean,
    Temperature_mean,
    FSI
  )


#############################################################################################
########################################## Transform data ###################################
#############################################################################################
combined_data$dist <- log10(combined_data$Value)  
combined_data$BM <- log10(combined_data$Body.mass)
combined_data <- combined_data[!is.na(combined_data$dist) & !is.nan(combined_data$dist) & is.finite(combined_data$dist), ]  
combined_data <- combined_data[!is.na(combined_data$BM) & !is.nan(combined_data$BM) & is.finite(combined_data$BM), ]  


combined_data <- combined_data %>%
  mutate(
    TG = coalesce(`Trophic.guild (Auger)`, `Trophic.Level(COMBINE)`, 
                  `Trophic.Level(AvoNET)`, `Trophic.Level(FishBase)`),
    TG = str_to_title(tolower(TG)),  
    MS = coalesce(`Migration(AvoNET)`, `Migration(FishBase)`),  
    MS = str_to_title(tolower(MS)),
    HP = `Habitat.Preference(FishBase)`,
    LM = `Locomotion.mode`
  ) %>%
  filter(!is.na(Event.ID),!is.na(dist), !is.na(BM)) %>%
  dplyr::select(-`Locomotion.mode`, -`Habitat.Preference(FishBase)`, -`Trophic.guild (Auger)`, 
                -`Trophic.Level(COMBINE)`, -`Trophic.Level(AvoNET)`, -`Migration(AvoNET)`, 
                -`Migration(FishBase)`, -`Trophic.Level(FishBase)`) 


table(combined_data$TG)
table(combined_data$LM)
table(combined_data$MS)

combined_data <- combined_data %>%
  mutate(
    # Recode TG
    TG = ifelse(TG == "Herbivore/Detritivore", "Herbivore", TG),
    
    # Correct LM classification by Family
    LM = case_when(
      Family.gbif %in% c("Carabidae", "Elateridae", "Scarabaeidae",
                         "Tenebrionidae", "Neanuridae", "Onychiuridae",
                         "Ixodidae") ~ "Running",
      TRUE ~ LM
    )
  ) %>%
  filter(LM != "Crawling")


save(combined_data, file = "combined_ENVIR.Rda")
