##correlation
#taxa_homo----
df <- read.csv("LRR/LRR_all_taxa_homo_shift.csv")
cor.test(df$yi_taxa, df$yi_homo, method = "spearman")
cor.test(df$yi_taxa, df$yi_shift, method = "spearman")
cor.test(df$yi_shift, df$yi_homo, method = "spearman")

dff <- read.csv("LRR/LRR_all_shannon_homo_shift.csv")
cor.test(dff$yi_shannon, dff$yi_homo, method = "spearman")
cor.test(dff$yi_shannon, dff$yi_shift, method = "spearman")
ggplot(dfrs, aes(LRR_richness, LRR_shannon)) +  #richness-homo
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "ln RR for Taxonomic richness",
    y = "ln RR for Shannon"
  )