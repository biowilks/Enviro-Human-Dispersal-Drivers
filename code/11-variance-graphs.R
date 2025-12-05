rm(list = ls())

# Load all packages ----------
library(tidyverse)   
library(partR2)
library(RColorBrewer)

# Import data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")
load("taxa_simple_models.RData")

### Variance calculations -------------------
# Fixed effect variance 
# Birds
partR2_birds <- partR2(birds_simple_model, partvars = c("NDVImean", "NDVIsd_mean", "Temperature_mean", "HFImean",
                                                        "BM", "MS", "TG"), data = birds_data, max_level = 1)

bird_variance <- as.data.frame(partR2_birds$R2)[-1,]

bird_variance <- mutate(bird_variance,
                        totalR2 = sum(estimate),
                        rescaledR2 = (estimate / totalR2) * 100,
                        group = 'Birds') |>
  dplyr::select(term, rescaledR2, group)


# Fish
partR2_fish <- partR2(fish_simple_model, partvars = c("NDVImean", "NDVIsd_mean", "Temperature_mean", "FSI",
                                                      "BM", "MS"), data = fish_data, max_level = 1)

fish_variance <- as.data.frame(partR2_fish$R2)[-1,]

fish_variance <- mutate(fish_variance,
                        totalR2 = sum(estimate),
                        rescaledR2 = (estimate / totalR2) * 100,
                        group = 'Fishes') |>
  dplyr::select(term, rescaledR2, group)


# inverts
partR2_invert <- partR2(invert_simple_model, partvars = c("NDVImean", "NDVIsd_mean", "Temperature_mean", "HFImean",
                                                          "BM", "LM", "TG"), data = invert_data, max_level = 1)

invert_variance <- as.data.frame(partR2_invert$R2)[-1,]

invert_variance <- mutate(invert_variance,
                          totalR2 = sum(estimate),
                          rescaledR2 = (estimate / totalR2) * 100,
                          group = 'Invertebrates') |>
  dplyr::select(term, rescaledR2, group)


# Mammals
partR2_mammal<- partR2(mammal_simple_model, partvars = c("NDVImean", "NDVIsd_mean", "Temperature_mean", "HFImean",
                                                         "BM", "TG"), data = mammal_data, max_level = 1)

mammal_variance <- as.data.frame(partR2_mammal$R2)[-1,]

mammal_variance <- mutate(mammal_variance,
                          totalR2 = sum(estimate),
                          rescaledR2 = (estimate / totalR2) * 100,
                          group = 'Mammals') |>
  dplyr::select(term, rescaledR2, group)


# Combine all and rename
variance_all <- bird_variance |> 
  full_join(fish_variance) |>  
  full_join(invert_variance) |> 
  full_join(mammal_variance)


# Birds
# Extract the random effects variance components
random_birds <- VarCorr(birds_simple_model)

random_bird_variance  <- as.data.frame(random_birds)

random_bird_variance <- mutate(random_bird_variance,
                               total = sum(vcov),
                               rescaled = (vcov / total) * 100,
                               group = 'Birds') 

# Fish
random_fish <- VarCorr(fish_simple_model)

random_fish_variance  <- as.data.frame(random_fish)

random_fish_variance <- mutate(random_fish_variance,
                               total = sum(vcov),
                               rescaled = (vcov / total) * 100,
                               group = 'Fishes') 


# inverts
random_invert <- VarCorr(invert_simple_model)

random_invert_variance  <- as.data.frame(random_invert)

random_invert_variance <- mutate(random_invert_variance,
                                 total = sum(vcov),
                                 rescaled = (vcov / total) * 100,
                                 group = 'Invertebrates') 


# Mammals
random_mammal <- VarCorr(mammal_simple_model)

random_mammal_variance  <- as.data.frame(random_mammal)

random_mammal_variance <- mutate(random_mammal_variance,
                                 total = sum(vcov),
                                 rescaled = (vcov / total) * 100,
                                 group = 'Mammals') 


random_variance_all <- random_bird_variance |> 
  full_join(random_fish_variance) |>  
  full_join(random_invert_variance) |> 
  full_join(random_mammal_variance) |>
  select(grp,rescaled,group)


### VARIANCE GRAPHS -------------------
colour_palette_red <- brewer.pal(9, "OrRd")
colour_palette_blue <-  brewer.pal(9, "GnBu")

variance_all$term <- factor(variance_all$term, levels = c(
  "BM", "MS", "TG", "LM", "NDVImean", "NDVIsd_mean", "Temperature_mean", "HFImean", "FSI"
))

# Fixed effects plot
ggplot(variance_all, aes(x = group, y = rescaledR2, fill = term)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Taxon", y = "Relative Contribution (%)", fill = "Fixed Effect") +
  scale_fill_manual(
    values = c(
      "NDVImean" = colour_palette_red[9],  
      "NDVIsd_mean" = colour_palette_red[7], 
      "Temperature_mean" = colour_palette_red[5], 
      "HFImean" =  colour_palette_red[3], 
      "FSI" = colour_palette_red[2],  
      "BM" = colour_palette_blue[9],  
      "MS" = colour_palette_blue[8],  
      "TG" = colour_palette_blue[6], 
      "LM" = colour_palette_blue[5]
    ),
    labels = c(
      "NDVImean" = "Productivity (NDVI mean)",
      "NDVIsd_mean" = "Seasonality (NDVI SD)",
      "Temperature_mean" = "Temperature",
      "HFImean" = "Human Footprint Index",
      "FSI" = "Fragmentation Status Index",
      "BM" = "Body mass",
      "MS" = "Migratory status",
      "TG" = "Trophic guild",
      "LM" = "Locomotion mode"
    )
  ) +
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )


# Random effects plot
ggplot(random_variance_all, aes(x = group, y = rescaled, fill = grp)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Taxon", y = "Relative Contribution (%)", fill = "Random Effect") +
  scale_fill_viridis_d(labels = c(
    "Family.gbif" = "Taxonomic family",
    "Sampling.method_checked" = "Sampling method",
    "yearStart" = "Start year"
  )) + 
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )


