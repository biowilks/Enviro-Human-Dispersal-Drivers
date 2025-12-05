rm(list = ls())

# Load all packages ----------
library(tidyverse)   
library(lmerTest)   
library(car)

# Import data ----------
setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

load("combined_ENVIR.Rda")

#############################################################################################
########################################## Mixed- effect models #############################
#############################################################################################

########################################## Birds ############################################
birds_data <- filter(combined_data, Taxa.category == "Bird") %>%
  filter(!is.na(NDVImean), !is.na(NDVIsd_mean), !is.na(HFImean),
         !is.na(Temperature_mean), !is.na(MS), !is.na(TG)) %>%
  mutate(  TG = as.factor(TG),
           MS = as.factor(MS),
           Family.gbif = as.factor(Family.gbif),
           Sampling.method.simple = as.factor(Sampling.method.simple),
           yearStart = as.factor(yearStart),
           Statistic = as.factor(Statistic)
         )

# Check for multicollinearity 
birds_simple_model <- lmerTest::lmer(
                      dist ~ BM + MS + TG +  
                             NDVImean + NDVIsd_mean + HFImean + Temperature_mean +
                             (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
                             data = birds_data, 
                             REML = TRUE)

vif(birds_simple_model) # All < 2

summary(birds_simple_model)

# full model
birds_full_model <- lmerTest::lmer(
                    dist ~ BM + MS + TG +
                      NDVImean + NDVImean:MS + NDVImean:TG +
                      NDVIsd_mean + NDVIsd_mean:MS + NDVIsd_mean:TG +
                      HFImean + HFImean:MS + HFImean:TG + 
                      Temperature_mean +
                      (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
                    data = birds_data, 
                    REML = F)

summary(birds_full_model, correlation = T)

anova_birds <- anova(birds_full_model, type = 2)
anova_birds_df <- as.data.frame(anova_birds)
write.csv(anova_birds_df, "~/share/user/Enviro-Human-Dispersal-Drivers/output/anova_bird.csv", row.names = TRUE)

########################################## Fishes ###########################################
fish_data <- filter(combined_data, Taxa.category == "Fish") %>%
  filter(!is.na(NDVImean), !is.na(NDVIsd_mean),
         !is.na(Temperature_mean), !is.na(FSI), !is.na(MS), !is.na(HP)) %>%
  mutate(
    MS = as.factor(MS),
    HP = as.factor(HP),
    Family.gbif = as.factor(Family.gbif),
    Sampling.method.simple = as.factor(Sampling.method.simple),
    yearStart = as.factor(yearStart),
    Statistic = as.factor(Statistic),
    FSI = log10(FSI)
  )

# Check for multicollinearity 
fish_simple_model <- lmerTest::lmer(dist ~ BM + MS + 
                                           NDVImean + NDVIsd_mean + FSI + Temperature_mean +
                                           (1|Family.gbif) + (1|yearStart) + (1|Statistic),
                                           data = fish_data,
                                           REML = TRUE)

vif(fish_simple_model) # All < 2

#full model
fish_full_model <-  lmerTest::lmer(dist ~ BM + MS +
                                     NDVImean + NDVImean:MS +  
                                     NDVIsd_mean + NDVIsd_mean:MS +
                                     FSI + FSI:MS + 
                                     Temperature_mean +
                                     (1|Family.gbif) + (1|yearStart) + (1|Statistic),
                                     data = fish_data,
                                     REML = FALSE)

summary(fish_full_model, correlation = T)


# quadratic
fish_quad_model <- lmerTest::lmer(
  dist ~  BM + MS + 
    NDVImean + NDVImean:MS +  
    NDVIsd_mean + NDVIsd_mean:MS +
    FSI + FSI:MS + 
    poly(Temperature_mean, 2, raw = TRUE) + 
    (1|Family.gbif) + (1|yearStart) + (1|Statistic),
  data = fish_data, 
  REML = F
)

summary(fish_quad_model, correlation = T)
AIC(fish_full_model, fish_quad_model)

anova_fish <- anova(fish_quad_model, type = 2)
anova_fish_df <- as.data.frame(anova_fish)
write.csv(anova_fish_df, "~/share/user/Enviro-Human-Dispersal-Drivers/output/anova_fish.csv", row.names = TRUE)



########################################## Insects ##########################################
invert_data <- filter(combined_data, Taxa.category == "Invertebrate")  %>%
  filter(!is.na(NDVImean), !is.na(NDVIsd_mean), !is.na(HFImean),
         !is.na(Temperature_mean), !is.na(LM), !is.na(TG)) %>%
  mutate(
    TG = as.factor(TG),
    LM = as.factor(LM),
    Family.gbif = as.factor(Family.gbif),
    Sampling.method.simple = as.factor(Sampling.method.simple),
    yearStart = as.factor(yearStart),
    Statistic = as.factor(Statistic)
  )



# Check for multicollinearity 
invert_simple_model <- lmerTest::lmer(dist ~ BM + LM + TG + 
                                            NDVImean + NDVIsd_mean + HFImean + Temperature_mean + 
                                            (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple), 
                                    data = invert_data,
                                    REML = TRUE)

vif(invert_simple_model) # All < 2
summary(invert_simple_model)

# full model
invert_full_model <- lmerTest::lmer(dist ~ BM + LM + TG +
                                      NDVImean + NDVImean:TG +
                                      NDVIsd_mean +  NDVIsd_mean:TG +
                                      HFImean + HFImean:TG + HFImean:LM + 
                                      Temperature_mean + 
                                      (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple), 
                                    data = invert_data,
                                    REML = F)

summary(invert_full_model, correlation = T)

# quadratic
invert_quad_model <- lmerTest::lmer(
  dist ~ BM + LM + TG +
    NDVImean + NDVImean:TG +
    NDVIsd_mean +  NDVIsd_mean:TG +
    HFImean + HFImean:TG + HFImean:LM +  
    poly(Temperature_mean, 2, raw = TRUE) + 
    (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple),
  data = invert_data, 
  REML = F
)

summary(invert_quad_model, correlation = T)
AIC(invert_full_model, invert_quad_model)

anova_invert <- anova(invert_full_model, type = 2)
anova_invert_df <- as.data.frame(anova_invert)
write.csv(anova_invert_df, "~/share/user/Enviro-Human-Dispersal-Drivers/output/anova_invert.csv", row.names = TRUE)

########################################## Mammals ##########################################
mammal_data <- filter(combined_data, Taxa.category == "Mammal") %>%
  filter(!is.na(NDVImean), !is.na(NDVIsd_mean), !is.na(HFImean),
         !is.na(Temperature_mean), !is.na(TG))  %>%
  mutate(
    TG = as.factor(TG),
    Family.gbif = as.factor(Family.gbif),
    Sampling.method.simple = as.factor(Sampling.method.simple),
    yearStart = as.factor(yearStart),
    Statistic = as.factor(Statistic)
  )

# Check for multicollinearity 
mammal_simple_model <- lmerTest::lmer(dist ~ BM + TG +
                                             NDVImean + NDVIsd_mean + HFImean + Temperature_mean + 
                                            (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
                                            data = mammal_data, 
                                            REML = TRUE)

vif(mammal_simple_model) # All < 2

anova(mammal_simple_model, type = '2')
summary(mammal_simple_model)

# full model
mammal_full_model <- lmerTest::lmer(dist ~ BM + TG +
                                           NDVImean + NDVImean:TG +
                                           NDVIsd_mean + NDVIsd_mean:TG +
                                           HFImean + HFImean:TG + 
                                           Temperature_mean + 
                                           (1|Family.gbif) + (1|yearStart) + (1|Sampling.method.simple) + (1|Statistic),
                                    data = mammal_data, 
                                    REML = F)

summary(mammal_full_model, correlation = T)


anova_mammal <- anova(mammal_full_model, type = 2)
anova_mammal_df <- as.data.frame(anova_mammal)
write.csv(anova_mammal_df, "~/share/user/Enviro-Human-Dispersal-Drivers/output/anova_mammal.csv", row.names = TRUE)


setwd("~/share/user/Enviro-Human-Dispersal-Drivers/output/")

save(birds_data, fish_data, invert_data, mammal_data, 
     birds_simple_model, fish_simple_model,invert_simple_model, mammal_simple_model, file = "taxa_simple_models.RData")

save(birds_data, fish_data, invert_data, mammal_data, 
     birds_full_model, fish_quad_model,invert_full_model, mammal_full_model, file = "taxa_models.RData")

