# Output Overview

This folder contains processed data and model outputs generated during the
Enviro–Human Dispersal Drivers workflow. Outputs are used for analysis,
modelling, and figure generation.

---

## Files

### CSV Outputs

- `anova_XXX.csv`  
  Analysis of variance results for trait and environmental driver models.

- `dispersal_distance_summary.csv`  
  Summary statistics of dispersal distances across species and taxa.

### RDA Outputs

- `combined_ENVIR.RDA`  
  Combined environmental and human-pressure data for all buffered dispersal polygons.

- `disp_fish_FSI.RDA`  
  Fish-specific dispersal and functional trait dataset (FSI = fragmentation status index).

- `dispersal_dist_sf_buffered.RDA`  
  Buffered dispersal polygons in spatial format for environmental extraction.

- `dispersal_distance_converted.RDA`  
  Standardised dispersal distance data with consistent units.

- `dispersalXXX_sf_HFI.RDA`  
  Dispersal polygons with Human Footprint Index values by taxon.

- `dispersalXXX_sf_NDVImean.RDA` / `dispersalXXX_sf_NDVIsd.RDA`  
  NDVI (mean and standard deviation) extracted for each species’ dispersal polygon by taxon.

- `dispersalXXX_sf_temp.RDA`  
  Temperature metrics extracted for dispersal polygons by taxon.

- `distance_alltraits.RDA`  
  Combined dataset linking dispersal distances with species traits.

- `taxa_models.RDA` / `taxa_simple_models.RDA`  
  Full and simplified mixed-effects models by taxon.

- `trait_models.RDA`  
  Trait-based models predicting dispersal distances across species.

---

## Notes

- Files with `XXX` represent versions for birds, fish, inverts, and mammals.  
- RDA files store processed R objects used in modelling and figure generation.  
- CSV files are summary or statistical outputs suitable for inspection or reuse.  
- All outputs were generated via scripts in `/code` following the numbered workflow.
