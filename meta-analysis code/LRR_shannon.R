### LRR Shannon 
library(metafor)
library(dplyr)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(multcomp)
library(aod)
df <- read.csv("data/shannon_data_extraction.csv")

##计算LRR taxa----
escalc_shannon <- escalc(
  measure = "ROM",
  m1i = Diversity_mean_urbanization,
  sd1i = Diversity_sd_urbanization,
  n1i = Diversity_n_urbanization,
  m2i = Diversity_mean_control,
  sd2i = Diversity_sd_control,
  n2i = Diversity_n_control,
  data = df
)

write.csv(escalc_shannon,"LRR/LRR_shannon_weighted.csv")
