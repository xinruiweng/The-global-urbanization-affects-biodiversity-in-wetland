library(metafor)
library(ggplot2)
library(patchwork)
##read data ----
LRR_richness_weighted <-read.csv("LRR/LRR_richness_weighted.csv")
LRR_shannon_weighted <-read.csv("LRR/LRR_shannon_weighted.csv")
LRR_homo_weighted <- read.csv("LRR/LRR_homogeneity_weighted.csv")
LRR_shift_weighted <- read.csv("LRR/LRR_shift_weighted.csv")
##multilevel random-effects model for taxa and shannon----
##richness
res_overall_taxa <- rma.mv(yi, vi,
                           random = ~1 | Study_ID/Plot_ID,
                           data = LRR_richness_weighted,
                           method = "REML")
##shannon
res_overall_shannon <- rma.mv(yi, vi,
                             random = ~1 | Study_ID/Plot_ID,
                             data = LRR_shannon_weighted,
                             method = "REML")
##homo
res_overall_homo <- rma.mv(yi, vi,
                              random = ~1 | Study_ID/Plot_ID,
                              data = LRR_homo_weighted,
                              method = "REML")
##shift
res_overall_shift <- rma.mv(yi, vi,
                            random = ~1 | Study_ID/Plot_ID,
                            data = LRR_shift_weighted,
                            method = "REML")

# 提取数据----
extract_overall <- function(fit, label){
  est <- coef(fit)[1]
  se  <- sqrt(vcov(fit)[1,1])
  ci95 <- est + c(-1,1) * 1.96  * se
  data.frame(
    group = label,
    mean = est,
    se = se,
    ci95_lower = ci95[1], ci95_upper = ci95[2]
  )
}

# 四个结果合并
sumtab <- rbind(
  extract_overall(res_overall_taxa,    "Taxonomic richness"),
  extract_overall(res_overall_shannon, "Shannon diversity"),
  extract_overall(res_overall_homo,    "Homogeneity"),
  extract_overall(res_overall_shift,   "Composition shift")
)

sumtab
#write.csv(sumtab,"data/overall.csv")

##four histogram----
richness_h <- ggplot(LRR_richness_weighted, aes(x = yi)) +
  geom_histogram(binwidth = 0.3, fill = "#1B5F9E", color = "#1B5F9E" ) +
  geom_vline(xintercept = 0, linetype = "dashed",color = "grey0", linewidth = 1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "grey40"),
    axis.title.y = element_text(size = 8, face = "plain"),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_rect(color = "#7AC5CD", fill = NA, linewidth = 2)
  )+ 
  labs(
    x = "LRR taxonomic richness",
    y = NULL
  )

shannon_h <- ggplot(LRR_shannon_weighted, aes(x = yi)) +
  geom_histogram(binwidth = 0.3, fill = "#008b8b", color = "#008b8b" ) +
  geom_vline(xintercept = 0, linetype = "dashed",color = "grey0", linewidth = 1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "grey40"),
    axis.title.y = element_text(size = 8, face = "plain"),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_rect(color = "#7AC5CD", fill = NA, linewidth = 2)
  )+ 
  labs(
    x = "LRR Shannon",
    y = NULL
  )

homo_h <- ggplot(LRR_homo_weighted, aes(x = yi)) +
  geom_histogram(binwidth = 0.3, fill = "grey40", color = "grey40" ) +
  geom_vline(xintercept = 0, linetype = "dashed",color = "grey0", linewidth = 1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "grey40"),
    axis.title.y = element_text(size = 8, face = "plain"),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_rect(color = "#7AC5CD", fill = NA, linewidth = 2)
  )+ 
  labs(
    x = "LRR homogeneity",
    y = NULL
  )

shift_h <- ggplot(LRR_shift_weighted, aes(x = yi)) +
  geom_histogram(binwidth = 0.3, fill = "#eead0e", color = "#eead0e" ) +
  geom_vline(xintercept = 0, linetype = "dashed",color = "grey0", linewidth = 1) +
  theme_minimal(base_size = 14) +
  theme(
    axis.line = element_line(color = "grey40"),
    axis.title.y = element_text(size = 8, face = "plain"),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_rect(color = "#7AC5CD", fill = NA, linewidth = 2)
  )+ 
  labs(
    x = "LRR shift",
    y = NULL
  )

##plots combine----
#histograms
right_plots_2 <- (richness_h | shannon_h) / (homo_h | shift_h )
colum_plot <- right_plots_2 + 
  plot_annotation(tag_levels = list(c("a","b", "c", "d"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))
