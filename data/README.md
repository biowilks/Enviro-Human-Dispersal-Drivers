# Data Overview

This folder contains the dispersal and trait datasets used in the
Enviro–Human Dispersal Drivers project.

---

## Files

### MacroMove_dispersal.RDA
Subset of the MacroMove global animal movement database.  
Includes dispersal distances and core traits (body mass and locomotion mode).  
Used as the primary dataset for all analyses.

### AugerTrait.txt
Trait data for invertebrates.  
Used mainly to assign trophic guild and locomotion mode.

### AVONET1_BirdLife.txt
Bird trait data including body mass, trophic guild, and migratory status.

### COMBINE_trait_data_imputed.txt
Mammal trait data including body mass and trophic guild.

---

## Trait Extraction Summary
All species included from MacroMove had body mass and locomotion mode.  
Additional traits were merged from external databases:

- **Trophic guild:**  
  - Birds → AVONET  
  - Invertebrates → Auger  
  - Mammals → COMBINE  
  - Fish → not used (little variation)

- **Migratory status:**  
  - Birds → AVONET  
  - Fish → FishBase  
  - Invertebrates & mammals → not available

These datasets were combined with the MacroMove subset early in the workflow.

---

## References

Wilkinson, C., Berti, E., Dyer, A., Menz, M.H.M., Meyer, C., Tucker, M.A.,  
Wootton, K.L., Broekman, M.J.E., Hoeks, S., Roth, J., Ryser, R., & Hirt, M.R.  
*MacroMove: a global database of animal movement across taxa and movement types* (preprint).

Tobias, J.A., Sheard, C., Pigot, A.L., Devenish, A.J.M., Yang, J., Sayol, F.,  
et al. (2022). AVONET: morphological, ecological and geographical data for all birds.  
*Ecology Letters*, 25, 581–597.

Auger, G., Pottier, J., Mathieu, J. & Jabot, F. (2024). Space use of invertebrates  
in terrestrial habitats: phylogenetic, functional and environmental drivers of  
interspecific variations. *Global Ecology and Biogeography*, 33, e13811.

Soria, C.D., Pacifici, M., Di Marco, M., Stephen, S.M. & Rondinini, C. (2021).  
COMBINE: a coalesced mammal database of intrinsic and extrinsic traits.  
*Ecology*, 102, e03344.

Boettiger, C., Chamberlain, S., Lang, D.T., Wainwright, P. & Cazelles, K. (2023).  
*rfishbase*: R interface to FishBase.
