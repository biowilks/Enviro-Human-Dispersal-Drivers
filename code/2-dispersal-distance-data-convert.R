rm(list = ls())  

# Load all packages ----------
library(tidyverse)

# Set working directory ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

# Load data ----------
load("distance_alltraits.Rda")

#################################################################################################
########################### HARMONISING UNITS AND START YEAR ####################################
#################################################################################################

unique(db_final_extended$Units)

db_final <- db_final_extended |>
  filter(!is.na(Value), !is.na(Body.mass), !is.na(Locomotion.mode))

# Convert distances to meters
db_final_units <- db_final |>
  mutate(
    Value = case_when(
      Units == "km" ~ Value * 1000,
      Units == "miles" ~ Value * 1609.34,
      Units == "m" ~ Value,
      TRUE ~ NA
    ),
    Units = "m"
  )

unique(db_final_extended$Body.mass.units)

# Clean body mass units
db_final_units_BM <- db_final_units |>
  mutate(
    Body.mass.units = case_when(
      Body.mass.units == "y" ~ NA_character_,
      TRUE ~ Body.mass.units
    )
  )

# Convert body mass to grams
db_final_units_all <- db_final_units_BM |>
  mutate(
    Body.mass = case_when(
      Body.mass.units == "kg" ~ Body.mass * 1000,
      TRUE ~ Body.mass
    ),
    Body.mass.units = "g"
  )

# Fill missing yearStart
db_final_year <- db_final_units_all |>
  mutate(
    yearStart = if_else(
      is.na(yearStart),
      as.numeric(str_extract(Original.ref, "(19|20)\\d{2}")),
      yearStart
    )
  ) |>
  filter(!is.na(yearStart))

#################################################################################################
################################# STATISTIC CALCULATION  ########################################
#################################################################################################
# Swift et al. 2021: calculate mean and median
swift_stats <- db_final_year |>
  filter(Original.ref == "Swift et al. 2021") |>
  filter(!is.na(Value)) |>
  summarise(
    mean_statistic = mean(Value),
    median_statistic = median(Value)
  )

swift_replacement <- db_final_year |>
  filter(Original.ref == "Swift et al. 2021") |>
  slice(1:2) |>
  mutate(
    Value = c(swift_stats$mean_statistic, swift_stats$median_statistic),
    Statistic = c("Mean", "Median")
  )

# Remove Swift and add replacement
db_final_extended <- db_final_year |>
  filter(Original.ref != "Swift et al. 2021")

dispersal_dist_stat <- bind_rows(db_final_extended, swift_replacement)

#################################################################################################
########################### FILTER FOR LOCATION AND CLASS ######################################
#################################################################################################
dispersal_dist_longlat <- dispersal_dist_stat |>
  filter(!is.na(startLongitude) & !is.na(startLatitude)) |>
  filter(!is.na(Statistic))

table(dispersal_dist_longlat$Taxa.category)

dispersal_dist_class <- dispersal_dist_longlat |>
  filter(!Taxa.category %in% c("Amphibian", "Reptile")) |>
  filter(!is.na(Taxa.category))

dispersal_dist_class |>
  group_by(Taxa.category, Locomotion.mode) |>
  summarise(count = n(), .groups = 'drop')

dispersal_dist_all <- dispersal_dist_class |>
  filter(!(Taxa.category == "Bird" & Locomotion.mode %in% c("Running", "Swimming"))) |>
  filter(!(Taxa.category == "Mammal" & Locomotion.mode %in% c("Climbing", "Swimming")))

#################################################################################################
################################# STATISTIC SELECTION  ##########################################
#################################################################################################
# Statistic selection
dispersal_dist_grouped <- dispersal_dist_all |>
  group_by(Species.ID, Original.ref) |>
  summarise(
    mean_value = ifelse(any(Statistic == "Mean"), mean(Value[Statistic == "Mean"], na.rm = TRUE), NA),
    median_value = ifelse(any(Statistic == "Median"), mean(Value[Statistic == "Median"], na.rm = TRUE), NA),
    max_value = ifelse(!any(Statistic %in% c("Mean", "Median")) & any(Statistic == "Maximum"),
                       max(Value[Statistic == "Maximum"], na.rm = TRUE), NA),
    .groups = 'drop'
  )

# Keep all mean/median, include max only if no mean/median
dispersal_dist_combined <- dispersal_dist_grouped |>
  pivot_longer(cols = c(mean_value, median_value, max_value),
               names_to = "Statistic",
               values_to = "final_value") |>
  filter(!is.na(final_value))

# Join to main data
db_final_filtered <- dispersal_dist_all |>
  left_join(dispersal_dist_combined, by = c("Species.ID", "Original.ref", "Statistic")) |>
  mutate(Value = ifelse(!is.na(final_value), final_value, Value)) |>
  select(-final_value) |>
  distinct(Event.ID, .keep_all = TRUE)

# counting metrics
metric_counts <- db_final_filtered |>
  group_by(Statistic) |>
  summarise(N = n(), .groups = "drop")


################################################################################################
########################### DATA COVERAGE ######################################################
#################################################################################################

summarise_dispersal <- function(data, group_name = "Total") {
  tibble(
    Group = group_name,
    Observations = nrow(data),
    Countries = n_distinct(data$Country, na.rm = TRUE),
    Max = sum(data$Statistic == "Maximum", na.rm = TRUE),
    Mean = sum(data$Statistic == "Mean", na.rm = TRUE),
    Median = sum(data$Statistic == "Median", na.rm = TRUE),
    Metastudies = n_distinct(data$Meta.ref, na.rm = TRUE),
    Empirical = n_distinct(data$Original.ref, na.rm = TRUE),
    Species = n_distinct(data$Species.ID.gbif, na.rm = TRUE),
    Families = n_distinct(data$Family.gbif, na.rm = TRUE),
    Min_mass = min(data$Body.mass, na.rm = TRUE),
    Max_mass = max(data$Body.mass, na.rm = TRUE),
    Min_value = min(data$Value, na.rm = TRUE),
    Max_value = max(data$Value, na.rm = TRUE),
    Year_start = min(data$yearStart, na.rm = TRUE),
    Year_end = max(data$yearStart, na.rm = TRUE)
  )
}

summary_total <- summarise_dispersal(db_final_filtered, "Total")
summary_mammals <- summarise_dispersal(filter(db_final_filtered, Taxa.category == "Mammal"), "Mammal")
summary_birds   <- summarise_dispersal(filter(db_final_filtered, Taxa.category == "Bird"), "Bird")
summary_fish    <- summarise_dispersal(filter(db_final_filtered, Taxa.category == "Fish"), "Fish")
summary_inverts <- summarise_dispersal(filter(db_final_filtered, Taxa.category == "Invertebrate"), "Invertebrate")

summary_table <- bind_rows(summary_total, summary_mammals, summary_birds, summary_fish, summary_inverts)

# Save results
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(db_final_filtered, file = "dispersal_distance_converted.Rda")
write_csv(summary_table, "dispersal_distance_summary.csv")


