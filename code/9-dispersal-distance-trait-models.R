rm(list = ls())

# Load all packages ----------
library(tidyverse)   
library(lmerTest)   

# Import data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

load("combined_ENVIR.Rda")


#############################################################################################
#################################    Trait  models  #########################################
#############################################################################################

trait_data <- combined_data %>%
  mutate(TG = as.factor(TG),
         LM = as.factor(LM),
         MS = as.factor(MS),
         Taxa.category = as.factor(Taxa.category),
         Sampling.method.simple = as.factor(Sampling.method.simple),
         yearStart = as.factor(yearStart),
         Statistic = as.factor(Statistic)) 

BM_model <- lmerTest::lmer(
  dist ~ BM + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
  data = trait_data,
  REML = TRUE)

summary(BM_model)

TG_model <- lmerTest::lmer(
  dist ~ BM * TG + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
  data = trait_data,
  REML = TRUE)

summary(TG_model)

LM_model <- lmerTest::lmer(
  dist ~ BM * LM + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
  data = trait_data,
  REML = TRUE)

summary(LM_model)

trait_data_MS <- subset(trait_data, Taxa.category %in% c("Bird", "Fish"))

MS_model <- lmerTest::lmer(
  dist ~ BM * MS * Taxa.category + 
    (1 | yearStart) + 
    (1 | Sampling.method.simple),
  data = trait_data_MS,
  REML = TRUE
)

summary(MS_model)


setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")
save(trait_data, trait_data_MS, BM_model, TG_model, LM_model, MS_model, file = "trait_models.RData")

