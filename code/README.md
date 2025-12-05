# Code Overview

This folder contains the full workflow used to prepare dispersal distance data,
extract environmental drivers, run statistical models, and generate figures for
the Enviro–Human Dispersal Drivers project.

The scripts are numbered in the order they should be run.

---

## 1–4. Data preparation and standardisation

### 1-dispersal-distance-data-addTRAIT.R
Adds trait data (from AVONET, Auger and COMBINE datasets) to the raw
dispersal distance dataset.

### 2-dispersal-distance-data-convert.R
Converts the raw dispersal distance data to consistent units, formats, and
standardised column names.

### 3-dispersal-distance-data-distribution-and-summary.R
Generates summary statistics and distribution plots for dispersal distances.

### 4-dispersal-distance-data-radius.R
Calculates buffered dispersal radii in spatial format and prepares data for
environmental extraction.

---

## 5–7. Environmental driver extraction

### 5-dispersal-distance-Productivity-Seasonality.R
Extracts NDVI (mean, seasonality) and related productivity metrics for each
dispersal starting point.

### 6-dispersal-distance-Temperature.R
Extracts temperature variables.

### 7-dispersal-distance-Human-pressures.R
Extracts human pressure metrics (e.g., Human Footprint Index and fragmentation status index).

---

## 8. Combine all drivers

### 8-dispersal-distance-combine-all-drivers.R
Merges all environmental, human-pressure, and trait datasets into a single
analysis-ready object.

---

## 9–10. Statistical modelling

### 9-dispersal-distance-trait-models.R
Fits trait-based models predicting dispersal distances across species.

### 10-dispersal-distance-LMM-models-all.R
Fits mixed-effects models incorporating traits, environmental drivers, and
random effects (e.g., taxonomy).

---

## 11–12. Figures and outputs

### 11-variance-graphs.R
Generates variance partitioning graphs across all taxa and models.

### 12-dispersal-distance-trait-environmental-driver-graphs.R
Generates combined trait–environment visualisations and summary figures for the
manuscript.

---

## Notes
- Scripts follow a numbered pipeline and should be run sequentially unless
otherwise noted.
- Output files are written to the `/output` directory.
- Files with `XXX` represent versions run separately for birds, fish, inverts,
and mammals.