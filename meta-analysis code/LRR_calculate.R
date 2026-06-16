library(metafor)
##data---
richness <- read.csv("data/richness_data_extraction.csv")
shannon <- read.csv("data/shannon_data_extraction.csv")
homo <- read.csv("data/homogeneity_data_extraction.csv")
shift <- read.csv("data/shift_data_extraction.csv")

##LRR taxonomic richness----
escalc_richness <- escalc(
  measure = "ROM",
  m1i = taxa_richness_mean_urbanization,
  sd1i = taxa_richness_sd_urbanization,
  n1i = taxa_richness_n_urbanization,
  m2i = taxa_richness_mean_control,
  sd2i = taxa_richness_sd_control,
  n2i = taxa_richness_n_control,
  data = richness
)
write.csv(escalc_richness,"LRR/LRR_richness_weighted.csv")

##LRR shannon----
escalc_shannon <- escalc(
  measure = "ROM",
  m1i = Diversity_mean_urbanization,
  sd1i = Diversity_sd_urbanization,
  n1i = Diversity_n_urbanization,
  m2i = Diversity_mean_control,
  sd2i = Diversity_sd_control,
  n2i = Diversity_n_control,
  data = shannon
)
write.csv(escalc_shannon,"LRR/LRR_shannon_weighted.csv")

##LRR homogeneity----
escalc_homo <- escalc(
  measure = "ROM",
  m1i = mean_d_urbanization,
  sd1i = SD_urbanization,
  n1i = n_urbanization,
  m2i = mean_d_control,
  sd2i = SD_control,
  n2i = n_control,
  data = homo
)
write.csv(escalc_homo,"LRR/LRR_homogeneity_weighted.csv")

##shift----
shift <- shift %>%
  mutate(mean_d_both = (mean_d_control + mean_d_urbanization)/2)

shift <- shift %>%
  mutate(mean_n_both = (n_urbanization + n_control)/2)

shift <- shift %>%
  mutate(mean_sd_both = (SD_urbanization + SD_control)/2)

escalc_shift <- escalc(
  measure = "ROM",
  m1i = mean_d_between,
  sd1i = SD_between ,
  n1i = n_between,
  m2i = mean_d_both,
  sd2i = mean_sd_both,
  n2i = mean_n_both,
  data = shift
)
write.csv(escalc_shift,"LRR/LRR_shift_weighted.csv")
