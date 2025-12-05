# Enviro-Human-Dispersal-Drivers

The file structure is organised as follows:

Enviro-Human-Dispersal-Drivers/ \
├── code/ \
│   ├── 1-dispersal-distance-data-addTRAIT.R \
│   ├── 2-dispersal-distance-data-convert.R \
│   ├── 3-dispersal-distance-data-distribution-and-summary.R \
│   ├── 4-dispersal-distance-data-radius.R \
│   ├── 5-dispersal-distance-Productivity-Seasonality.R \
│   ├── 6-dispersal-distance-Temperature.R \
│   ├── 7-dispersal-distance-Human-pressures.R \
│   ├── 8-dispersal-distance-combine-all-drivers.R \
│   ├── 9-dispersal-distance-trait-models.R \
│   ├── 10-dispersal-distance-LMM-models-all.R \
│   ├── 11-variance-graphs.R \
│   ├── 12-dispersal-distance-trait-environmental-driver-graphs.R \
│   ├── README.md \
├── data/ \
│   ├── MacroMove_dispersal.RDA \
│   ├── AugerTrait.txt \
│   ├── AVONET1_BirdLife.txt \
│   ├── COMBINE_trait_data_imputed.txt \
│   └── README.md \
├── docs/ \
│   ├── Figure_1_Dispersal_distance_distribution.PNG \
│   ├── Figure_2_Variance_trait_graphs_all.PNG \
│   ├── Figure_3_Trait_driver_results_all.PNG \
│   ├── Figure_S1_Variance_random_effect_graphs_all.PNG \
│   ├── Table1_Dispersal_driver_predictions.PNG \
│   ├── HUMANPRESSURES_all.PNG \
│   ├── PRODUCTIVITY_all.PNG \
│   ├── SEASONALITY_all.PNG \
│   ├── TEMPERATURE_all.PNG \
│   ├── TRAIT_DRIVERS_all.PNG \
│   └── README.md \
├── output/ \
│   ├── anova_XXX.csv \
│   ├── combined_ENVIR.RDA \
│   ├── disp_fish_FSI.RDA \
│   ├── dispersal_dist_sf_buffered.RDA \
│   ├── dispersal_distance_converted.RDA \
│   ├── dispersal_distance_summary.csv \
│   ├── dispersalXXX_sf_HFI.RDA \
│   ├── dispersalXXX_sf_HFI.RDA \
│   ├── dispersalXXX_sf_NDVImean.RDA \
│   ├── dispersalXXX_sf_NDVIsd.RDA \
│   ├── dispersalXXX_sf_temp.RDA \
│   ├── distance_alltraits.RDA \
│   ├── taxa_models.RDA \
│   ├── taxa_simple_models.RDA \
│   ├── trait_models.RDA \
│   └── README.md \
└── README.md \

### File Naming Convention

Many output files use the placeholder `XXX` to indicate the taxonomic group.
For example:

- `birds`
- `fish`
- `inverts`
- `mammals`

Thus, any file with `XXX` appears once for each taxon.  

Examples:
- `anova_birds.csv`, `anova_fish.csv`, `anova_inverts.csv`, `anova_mammals.csv` etc.