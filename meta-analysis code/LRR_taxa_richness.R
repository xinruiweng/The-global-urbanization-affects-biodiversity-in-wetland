### LRR taxonomic richness 
library(metafor)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(tidyr)
library(multcomp)
df <- read.csv("data/richness_data_extraction.csv")

##LRR taxonomic richness----
escalc_richness <- escalc(
  measure = "ROM",
  m1i = taxa_richness_mean_urbanization,
  sd1i = taxa_richness_sd_urbanization,
  n1i = taxa_richness_n_urbanization,
  m2i = taxa_richness_mean_control,
  sd2i = taxa_richness_sd_control,
  n2i = taxa_richness_n_control,
  data = df
)

write.csv(escalc_richness,"LRR/LRR_richness_weighted.csv")
