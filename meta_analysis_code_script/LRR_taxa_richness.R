library(metafor)
df <- read.csv("data/taxa_data_extraction.csv")

##LRR taxa----
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

write.csv(escalc_richness,"LRR/LRR_taxa_weighted.csv")
