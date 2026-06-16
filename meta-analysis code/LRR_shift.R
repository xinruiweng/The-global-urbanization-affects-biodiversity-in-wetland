###calculate the LRR shift
library(metafor)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
df <- read.csv("data/shift_data_extraction.csv")

##data_mutate
df <- df %>%
  mutate(mean_d_both = (mean_d_control + mean_d_urbanization)/2)

df <- df %>%
  mutate(mean_n_both = (n_urbanization + n_control)/2)

df <- df %>%
  mutate(mean_sd_both = (SD_urbanization + SD_control)/2)

escalc_shift <- escalc(
  measure = "ROM",
  m1i = mean_d_between,
  sd1i = SD_between ,
  n1i = n_between,
  m2i = mean_d_both,
  sd2i = mean_sd_both,
  n2i = mean_n_both,
  data = df
)

write.csv(escalc_shift,"LRR/LRR_shift_weighted.csv")
