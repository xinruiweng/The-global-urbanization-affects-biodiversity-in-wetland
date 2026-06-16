### LRR homogeneity

# 安装并加载metafor包
library(metafor)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(car)
library(ggplot2)
library(emmeans)
library(tidyr)
library(multcomp)
library(readr)
df <- read.csv("data/homo_data_extraction.csv")

##计算homogeneity_LRR
escalc_homo <- escalc(
  measure = "ROM",
  m1i = mean_d_urbanization,
  sd1i = SD_urbanization,
  n1i = n_urbanization,
  m2i = mean_d_control,
  sd2i = SD_control,
  n2i = n_control,
  data = df
)

write.csv(escalc_homo,"LRR/LRR_homogeneity_weighted.csv")