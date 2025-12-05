rm(list=ls())

# Load all packages ----------
library(tidyverse)
library(rfishbase)

# Import trait data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/data")
load("MacroMove_dispersal.Rda")

# Soria2021 - Combine
combine <-read_tsv('COMBINE_trait_data_imputed.txt')
# Tobias2022
avonet<-read_tsv('AVONET1_BirdLife.txt')
# Auger2023
invert <-read_tsv('AugerTrait.txt') |> dplyr::select(-Locomotion.mode)


# Load FishBase
# Load FishBase data and ecology information
fishbase_all <- load_taxa(server = "fishbase")
fishbase <- species(server = "fishbase")
fishbase_ecology <- ecology(fishbase$Species, server = "fishbase")


# Adding invert traits
db <- MacroMove_dispersal |>
  left_join(invert, by = c("Species.ID.gbif" = "Species"), relationship = "many-to-many")


# Adding additional trait data ---------
colnames(db)

db_final <- db |>
  dplyr::select(-Sampling.method, -Sampling.level, -N.measurement,-dayStart,
         -monthStart,-dayEnd,-monthEnd,-Temporal.frequency, -Inference.method,-Body.mass.sampling.level,
         -Sex, -Lifestage, -Age, -Location.description, -Reference, -Verified.by) |>
  rename('Trophic.guild (Auger)' = 'Trophic.guild') |>
  rename('Activity (Auger)' = 'Activity') 

combine_subset <- combine |>
  dplyr::select(iucn2020_binomial, trophic_level,foraging_stratum)

combine_subset <- combine_subset |>
  mutate(
    Trophic.Level = case_when(
      trophic_level == 1 ~ "Herbivore",
      trophic_level == 2 ~ "Omnivore",
      trophic_level == 3 ~ "Carnivore",
      TRUE ~ as.character(trophic_level)
    ),
    foraging_stratum = case_when(
      foraging_stratum == "M" ~ "marine",
      foraging_stratum == "G" ~ "ground level",
      foraging_stratum == "S" ~ "scansorial",
      foraging_stratum == "Ar" ~ "arboreal",
      foraging_stratum == "A" ~ "aerial",
      TRUE ~ foraging_stratum
    )
  ) |>
  dplyr::select(-trophic_level) |>
  rename_with(~ paste0(.x, "(COMBINE)"))

avonet_subset <- avonet |>
  dplyr::select(Species1, `Hand-Wing.Index`, `Tarsus.Length`, Migration, `Habitat`,
         `Trophic.Level`
  ) |>
  mutate(
    Migration = case_when(
      Migration == 1 ~ "Sedentary",
      Migration == 2 ~ "Partially migratory",
      Migration == 3 ~ "Migratory",
      TRUE ~ as.character(Migration)
    )
  )|>
  rename_with(~ paste0(.x, "(AvoNET)"))


fb_subset <- fishbase_all %>%
  left_join(
    fishbase %>% dplyr::select(SpecCode, AnaCat, DemersPelag, Fresh, Brack, Saltwater), 
    by = "SpecCode", 
    relationship = "many-to-many"
  ) %>%
  left_join(
    fishbase_ecology %>% dplyr::select(SpecCode, DietTroph, FoodTroph), 
    by = "SpecCode", 
    relationship = "many-to-many"
  ) %>%
  mutate(
    Habitat = case_when(
      Brack == 1 ~ "Brackish",  
      Fresh == 1 ~ "Freshwater", 
      Saltwater == 1 ~ "Saltwater", 
      TRUE ~ "Unknown"
    ),
    Migration = case_when(
      AnaCat == "anadromous" ~ "Migratory",  
      AnaCat == "catadromous" ~ "Migratory", 
      TRUE ~ "Sedentary" 
    ),
    Trophic.Level = case_when( # based on feeding habits
      FoodTroph >= 2.0 & FoodTroph <= 2.19 ~ "Herbivore/Detritivore", 
      FoodTroph >= 2.2 & FoodTroph <= 2.79 ~ "Omnivore",  
      FoodTroph >= 2.8 ~ "Carnivore",  
      TRUE ~ NA_character_
    ),
    Diet.Troph.Level = case_when( # based on stomach content
      DietTroph >= 2.0 & DietTroph <= 2.19 ~ "Herbivore",
      DietTroph >= 2.2 & DietTroph <= 2.79 ~ "Omnivore",
      DietTroph >= 2.8 ~ "Carnivore",
      TRUE ~ NA
    ),
    Habitat.Preference = case_when(
      DemersPelag == "bathydemersal" ~ "Bathydemersal", # living and feeding on the sea bed at depths below 200m
      DemersPelag == "bathypelagic" ~ "Bathypelagic", # living and feeding in open water depths below 200m
      DemersPelag == "benthopelagic" ~ "Benthopelagic", # foraging across benthic and pelagic habitats
      DemersPelag == "demersal" ~ "Demersal", # living and feeding near the sea bed shallower than 200m and not reef-associated
      DemersPelag == "pelagic-neritic" ~ "Pelagic-neritic", # living and feeding in the pelagic zone above a continental shelf
      DemersPelag == "pelagic-oceanic" ~ "Pelagic-oceanic", # living and feeding in the pelagic zone of the open ocean
      DemersPelag == "reef-associated" ~ "Reef associated", # living and feeding on a wave resistant feature the upper surface of which is within 0–20m of the ocean surface
      TRUE ~ NA_character_
    )) %>%
  dplyr::select(Species, Habitat, Trophic.Level, Diet.Troph.Level, Migration, Habitat.Preference) %>%
  rename_with(~ paste0(.x, "(FishBase)"))


db_final_extended <- db_final |>
  left_join(combine_subset, by = c("Species.ID.gbif" = "iucn2020_binomial(COMBINE)"), relationship = "many-to-many") |>
  left_join(avonet_subset, by = c("Species.ID.gbif" = "Species1(AvoNET)"), relationship = "many-to-many") |>
  left_join(fb_subset, by = c("Species.ID.gbif" = "Species(FishBase)"), relationship = "many-to-many") |>
  distinct(Event.ID, .keep_all = TRUE)


setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output")
save(db_final_extended, file = "distance_alltraits.Rda")

