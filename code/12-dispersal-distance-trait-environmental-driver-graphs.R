rm(list = ls())

# Load all packages ----------
library(tidyverse)   
library(patchwork)     
library(ggeffects) 
library(emmeans)

# Import data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

load("trait_models.RData")
load("combined_ENVIR.Rda")
load("taxa_models.RData")

### Colour palettes  ------------------
migration_colour <- scale_color_manual(values = c("Migratory" = "maroon3",
                                                  "Partially Migratory" = "darkred",
                                                  "Sedentary" = "slateblue")) 


locomotion_mode_colour <- scale_color_manual(values = c("Flying" = "orangered1", 
                                                        "Running" = "#99CC66", 
                                                        "Swimming" = "cornflowerblue"))

trophic_guild_colour <- scale_color_manual(values = c("Carnivore" = 'firebrick3', 
                                                      "Herbivore" = 'seagreen3', 
                                                      "Omnivore" = 'dodgerblue3',
                                                      "Decomposer" = '#663300'))


### Plot layout ------------------
plot_layout_1 <- plot_layout(ncol = 4, nrow = 1)

### Trait plots ------------------
# BM overall
BM_slopes <- emtrends(BM_model, ~ 1, var = "BM", infer = TRUE)
print(BM_slopes)

## TG model predictions ---
BM_TG_all <- ggpredict(TG_model, terms = c("BM[all]", "TG[all]"))

# Calculate slopes and significant relationships ---
BM_TG_slopes <- emtrends(TG_model, var = "BM", specs = "TG", infer = TRUE)
print(BM_TG_slopes)


# Find BM ranges ---
trait_data %>%
  group_by(TG) %>%
  summarise(
    min_BM = min(BM, na.rm = TRUE),
    max_BM = max(BM, na.rm = TRUE)
  )

# Filter model predictions based on BM ranges ---
BM_TG_all <- BM_TG_all %>%
  filter(
    case_when(
      group == "Carnivore" ~ (x >= -6.94 & x <= 5.91),
      group == "Decomposer" ~ (x >= -5.39 & x <= 2.85),
      group == "Herbivore" ~ (x >= -4.4 & x <= 6.6),
      group == "Omnivore" ~ (x >= 0.914 & x <= 5.61),
      TRUE ~ TRUE
    )
  )

# Plot ---
BM_TG <- ggplot(filter(trait_data, !is.na(TG)), aes(x = BM, y = dist)) +
  geom_point(aes(color = TG), alpha = 0.3, size = 2) +
  geom_line(
    data = BM_TG_all,
    aes(x = x, y = predicted, color = group),
    linetype = "solid",
    size = 1.5
  ) +
  trophic_guild_colour +
  labs(
    x = "", 
    y = "",
    color = "Trophic level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


## LM model predictions ---
BM_LM_all <- ggpredict(LM_model, terms = c("BM[all]", "LM[all]"))

# Calculate slopes and significant relationships ---
BM_LM_slopes <- emtrends(LM_model, var = "BM", specs = "LM", infer = TRUE)
print(BM_LM_slopes)

# Find BM ranges ---
trait_data %>%
  group_by(LM) %>%
  summarise(
    min_BM = min(BM, na.rm = TRUE),
    max_BM = max(BM, na.rm = TRUE)
  )

# Filter model predictions based on BM ranges ---
BM_LM_all <- BM_LM_all %>%
  filter(
    case_when(
      group == "Flying" ~ (x >= -4.28 & x <= 3.84),
      group == "Swimming" ~ (x >= 2.34 & x <= 5.91),
      group == "Running" ~ (x >= -5.53 & x <= 6.66),
      group == "Crawling" ~ (x >= -6.94 & x <= 1.07),
      TRUE ~ TRUE
    )
  )

# Plot ---
BM_LM <- ggplot(trait_data, aes(x = BM, y = dist)) +
  geom_point(aes(color = LM), alpha = 0.3, size = 2) +
  geom_line(
    data = BM_LM_all,
    aes(x = x, y = predicted, color = group),
    linetype = "solid",
    size = 1.5
  ) +
  locomotion_mode_colour +
  labs(
    x = "",
    y = "",
    color = "Locomotion mode"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )


# MS model predictions ---
BM_MS_all <- ggpredict(MS_model, terms = c("BM[all]","MS [all]","Taxa.category"))

# Calculate slopes and significant relationships ---
BM_MS_slopes <- emtrends(MS_model, var = "BM", specs = c("MS", "Taxa.category"), infer = TRUE)
print(BM_MS_slopes)

# Find BM ranges ---
combined_data %>%
  filter(Taxa.category == "Bird") %>%
  group_by(MS) %>%
  summarise(
    min_BM = min(BM, na.rm = TRUE),
    max_BM = max(BM, na.rm = TRUE)
  )

combined_data %>%
  filter(Taxa.category == "Fish") %>%
  group_by(MS) %>%
  summarise(
    min_BM = min(BM, na.rm = TRUE),
    max_BM = max(BM, na.rm = TRUE)
  )

BM_MS_all <- BM_MS_all %>%
  filter(!(facet == "Fish" & group == "Partially Migratory")) %>%
  filter(
    case_when(
      facet == "Bird" & group == "Migratory" ~ (x >= 0.929 & x <= 3.84),
      facet == "Bird" & group == "Partially Migratory" ~ (x >= 0.996 & x <= 3.67),
      facet == "Bird" & group == "Sedentary" ~ (x >= 0.38 & x <= 3.39),
      facet == "Fish" & group == "Migratory" ~ (x >= 3.28 & x <= 5.91),
      facet == "Fish" & group == "Sedentary" ~ (x >= 2.34 & x <= 5.11),
      TRUE ~ FALSE  
    )
  )

# Plot ---
BM_MS <- ggplot(filter(combined_data, !is.na(MS)), aes(x = BM, y = dist)) +
  geom_point(aes(color = MS), alpha = 0.3, size = 2) +
  geom_line(
    data = BM_MS_all,
    aes(x = x, y = predicted, color = group, linetype = facet),  
    size = 1.5
  ) +
  scale_linetype_manual(
    values = c("Bird" = "solid", "Fish" = "twodash")) +
  migration_colour +
  labs(
    x = "",
    y = "",
    color = "Migratory status",
    linetype = "Class" 
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

(BM_TG + BM_LM + BM_MS)

ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/TRAIT_DRIVERS.png", height = 5, width = 20, dpi = 300,bg = "transparent")

### Productivity - NDVImean ------------------
## BIRD
# NDVI model predictions
NDVImean_bird_MS_pred <- ggpredict(birds_full_model, 
                                   terms = c("NDVImean[all]", "MS"))

# Calculate slopes and significant relationships ---
NDVImean_bird_MS_slopes <- emtrends(birds_full_model, var = "NDVImean", specs = "MS", infer = TRUE)
print(NDVImean_bird_MS_slopes)

# Find ranges ---
birds_data %>%
  group_by(MS) %>%
  summarise(
    min = min(NDVImean, na.rm = TRUE),
    max = max(NDVImean, na.rm = TRUE)
  )

# Filter model predictions based on NDVI ranges ---
NDVImean_bird_MS_pred <- NDVImean_bird_MS_pred %>%
  filter(
    case_when(
      group == "Migratory" ~ (x >= 0.127 & x <= 0.757),
      group == "Partially Migratory" ~ (x >= 0.144 & x <= 0.742),
      group == "Sedentary" ~ (x >= 0.0283 & x <= 0.820),
      TRUE ~ TRUE
    )
  )

# Plot ---
NDVImean_bird_MS <- ggplot(birds_data, aes(x = NDVImean, y = dist)) +
  geom_point(aes(color = MS), alpha = 0.3, size = 2) +  
  geom_line(data = NDVImean_bird_MS_pred, aes(x = x, y = predicted, color = group), size = 1.5) +
  migration_colour + 
  labs(
    x = "Annual NDVI mean", 
    y = "",
    color = "Migratory status"
  ) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()  
  )

## FISH
# NDVI model predictions
NDVImean_fish_MS_pred <- ggpredict(fish_quad_model, 
                                   terms = c("NDVImean[all]", "MS"))

# Calculate slopes and significant relationships ---
NDVImean_fish_MS_slopes <- emtrends(fish_quad_model, var = "NDVImean", specs = "MS", infer = TRUE)
print(NDVImean_fish_MS_slopes)

# Find ranges ---
fish_data %>%
  group_by(MS) %>%
  summarise(
    min = min(NDVImean, na.rm = TRUE),
    max = max(NDVImean, na.rm = TRUE)
  )

# Filter model predictions based on NDVI ranges ---
NDVImean_fish_MS_pred <- NDVImean_fish_MS_pred %>%
  filter(
    case_when(
      group == "Migratory" ~ (x >= 0.244 & x <= 0.877),
      group == "Sedentary" ~ (x >= 0.269 & x <= 0.774),
      TRUE ~ TRUE
    )
  )

# Plot ---
NDVImean_fish_MS <- ggplot(fish_data, aes(x = NDVImean, y = dist)) +
  geom_point(aes(color = MS), alpha = 0.3, size = 2) +  
  geom_line(data = NDVImean_fish_MS_pred, aes(x = x, y = predicted, color = group), size = 1.5) +
  migration_colour + 
  labs(
    x = "Annual NDVI mean", 
    y = "",
    color = "Migratory status"
  ) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()  
  )

## INVERTEBRATE - N.S

## MAMMAL - main effect
NDVImean_mammal_pred <- ggpredict(mammal_full_model, terms = c("NDVImean[all]"))

# Calculate slopes and significant relationships ---
NDVImean_mammal_slopes <- emtrends(mammal_full_model, ~ 1, var = "NDVImean", infer = TRUE)
print(NDVImean_mammal_slopes)

NDVImean_mammal <- ggplot(mammal_data, aes(x = NDVImean, y = dist)) +
  geom_point(alpha = 0.2, size = 2, color = "black") + 
  geom_line(data = NDVImean_mammal_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(
    x = "Annual NDVI mean", 
    y = ""
  ) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )



(NDVImean_bird_MS + NDVImean_fish_MS + plot_spacer() + NDVImean_mammal) + plot_layout_1

ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/PRODUCTIVITY_all.png", height = 5, width = 20, dpi = 300, bg = "transparent")


### Seasonality - NDVIsd ------------------

## BIRD - N.S.

## FISH - main effect
NDVIsd_fish_pred <- ggpredict(fish_quad_model, terms = c("NDVIsd_mean[all]"))

# Calculate slopes and significant relationships ---
NDVIsd_fish_slopes <- emtrends(fish_quad_model, ~ 1, var = "NDVIsd_mean", infer = TRUE)
print(NDVIsd_fish_slopes)

NDVIsd_fish <- ggplot(fish_data, aes(x = NDVIsd_mean, y = dist)) +
  geom_point(alpha = 0.2, size = 2, color = "black") + 
  geom_line(data = NDVIsd_fish_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(
    x = "Annual NDVI (SD) mean", 
    y = "") + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

## INVERTEBRATE - TG
NDVIsd_invert_TG_pred <- ggpredict(invert_full_model, terms = c("NDVIsd_mean[all]", "TG"))

# Calculate slopes and significant relationships ---
NDVIsd_invert_TG_slopes <- emtrends(invert_full_model, var = "NDVIsd_mean", specs = "TG", infer = TRUE)
print(NDVIsd_invert_TG_slopes)

# Find ranges ---
invert_data %>%
  group_by(TG) %>%
  summarise(
    min = min(NDVIsd_mean, na.rm = TRUE),
    max = max(NDVIsd_mean, na.rm = TRUE)
  )

# Filter model predictions based on NDVI ranges ---
NDVIsd_invert_TG_pred <- NDVIsd_invert_TG_pred %>%
  filter(
    case_when(
      group == "Carnivore" ~ (x >= 0.00668 & x <= 0.313),
      group == "Decomposer" ~ (x >= 0.00708 & x <= 0.298),
      group == "Omnivore" ~ (x >= 0.00954 & x <= 0.328),
      TRUE ~ TRUE
    )
  )

# Plot ---
NDVIsd_invert_TG <- ggplot(invert_data, aes(x = NDVIsd_mean, y = dist)) +
  geom_point(aes(color = TG), alpha = 0.3, size = 2) + 
  geom_line(data = NDVIsd_invert_TG_pred, aes(x = x, y = predicted, color = group), size = 1.5) +  
  trophic_guild_colour +
  labs(
    x = "Annual NDVI (SD) mean", 
    y = "",
    color = "Trophic Level"
  ) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )


## MAMMALS - N.S

(plot_spacer() + NDVIsd_fish + NDVIsd_invert_TG + plot_spacer()) + plot_layout_1

ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/SEASONALITY_all.png", height = 5, width = 20, dpi = 300,bg = "transparent")


### Temperature ------------------
## BIRD - Main effect
TEMP_bird_pred <- ggpredict(birds_full_model, 
                               terms = c("Temperature_mean[all]"))


# Calculate slopes and significant relationships ---
TEMP_bird_slopes <- emtrends(birds_full_model, ~ 1, var = "Temperature_mean", infer = TRUE)
print(TEMP_bird_slopes)


# Plot ---
TEMP_bird <- ggplot(birds_data, aes(x = Temperature_mean, y = dist)) +
  geom_point(alpha = 0.3, size = 2, color = "black") + 
  geom_line(data = TEMP_bird_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(x = "Annual Air Temperature (°C)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  ) 

## FISH - Main effect
TEMP_fish_pred <- ggpredict(fish_quad_model, terms = c("Temperature_mean[all]"))

# Calculate slopes and significant relationships ---
temp_range <- seq(
  min(fish_data$Temperature_mean, na.rm = TRUE),
  max(fish_data$Temperature_mean, na.rm = TRUE),
  length.out = 50
)

TEMP_fish_slopes_range <- emtrends(
  fish_quad_model,
  specs = ~1,
  var = "Temperature_mean",
  max.degree = 2,
  at = list(Temperature_mean = temp_range),
  infer = TRUE
)

print(TEMP_fish_slopes_range)

# Plot ---
TEMP_fish <- ggplot(fish_data, aes(x = Temperature_mean, y = dist)) +
  geom_point(alpha = 0.3, size = 2, color = "black") + 
  geom_line(data = TEMP_fish_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(
    x = "Annual Air Temperature (°C)", 
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

## INVERTEBRATE - N.S

## MAMMALS - main effect
TEMP_mammal_pred <- ggpredict(mammal_full_model, terms = c("Temperature_mean[all]"))

# Calculate slopes and significant relationships ---
TEMP_mammal_slopes <- emtrends(mammal_full_model, ~ 1, var = "Temperature_mean", infer = TRUE)
print(TEMP_mammal_slopes)

# Plot ---
TEMP_mammal <- ggplot(mammal_data, aes(x = Temperature_mean, y = dist)) +
  geom_point(alpha = 0.3, size = 2, color = "black") + 
  geom_line(data = TEMP_mammal_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(x = "Annual Air Temperature (°C)", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  ) 


# Combine plots
(TEMP_bird + TEMP_fish + plot_spacer() + TEMP_mammal) + plot_layout_1

ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/TEMPERATURE_all.png", height = 5, width = 20, dpi = 300, bg = "transparent")

### Human pressures - HFI or FSI ------------------
## BIRDS - main effect
HFI_bird_pred <- ggpredict(birds_full_model, terms = c("HFImean[all]"))

# Calculate slopes and significant relationships ---
HFI_bird_slopes <- emtrends(birds_full_model, ~ 1, var = "HFImean", infer = TRUE)
print(HFI_bird_slopes)

HFI_bird <- ggplot(birds_data, aes(x = HFImean, y = dist)) +
  geom_point(alpha = 0.3, size = 2, color = "black") + 
  geom_line(data = HFI_bird_pred, aes(x = x, y = predicted), size = 1.5, color = "black") +
  labs(
    x = "HFI mean", 
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.line = element_line(color = "grey30", size = 1),
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )



## FISH - MS
# MS model predictions
FSI_fish_MS_pred <- ggpredict(fish_quad_model, terms = c("FSI[all]", "MS"))

# Calculate slopes and significant relationships ---
FSI_fish_MS_slopes <- emtrends(fish_quad_model, var = "FSI", specs = "MS", infer = TRUE)
print(FSI_fish_MS_slopes)

# Find ranges ---
fish_data %>%
  group_by(MS) %>%
  summarise(
    min = min(FSI, na.rm = TRUE),
    max = max(FSI, na.rm = TRUE)
  )

# Filter model predictions based on NDVI ranges ---
FSI_fish_MS_pred <- FSI_fish_MS_pred %>%
  filter(
    case_when(
      group == "Migratory" ~ (x >= -1.88 & x <= 1.87),
      group == "Sedentary" ~ (x >= -1.82 & x <= 1.89),
      TRUE ~ TRUE
    )
  )

# Plot ---
FSI_fish_MS <- ggplot(fish_data, aes(x = FSI, y = dist)) +
  geom_point(aes(color = MS), alpha = 0.3, size = 2) + 
  geom_line(data = FSI_fish_MS_pred, aes(x = x, y = predicted, color = group), size = 1.5) +  
  migration_colour +
  labs(
    x = "FSI", 
    y = "",
    color = "Migratory Status"
  ) + 
  theme_minimal() +  
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),  
    axis.line = element_line(color = "grey30", size = 1),  
    panel.grid.major = element_line(color = "lightgrey", linetype = "dashed"),  
    panel.grid.minor = element_blank(),
    plot.title = element_blank()
  )

## INVERTEBRATES - N.S.

(HFI_bird + FSI_fish_MS + plot_spacer() + plot_spacer()) + plot_layout_1

ggsave("~/share/user/Enviro-Human-Dispersal-Drivers/docs/HUMANPRESSURES_all.png", height = 5, width = 20, dpi = 300,bg = "transparent")

