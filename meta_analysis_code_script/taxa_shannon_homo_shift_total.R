##taxa_shannon_homo_shift_total forest plot

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
library(aod)
library(patchwork)
library(magick)
library(cowplot)
##read data taxa shannon----
LRR_taxa_weighted <-read.csv("LRR/LRR_taxa_weighted.csv")
LRR_shannon_weighted <-read.csv("LRR/LRR_shannon_weighted.csv")
LRR_homo_weighted <- read.csv("LRR/LRR_homogeneity_weighted.csv")
LRR_shift_weighted <- read.csv("LRR/LRR_shift_weighted.csv")
##multilevel random-effects model for taxa and shannon----
##taxa
res_overall_taxa <- rma.mv(yi, vi,
                           random = ~1 | Study_ID/Plot_ID,
                           data = LRR_taxa_weighted,
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

# data_extraction----
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

# comb
sumtab <- rbind(
  extract_overall(res_overall_taxa,    "Taxonomic richness"),
  extract_overall(res_overall_shannon, "Shannon diversity"),
  extract_overall(res_overall_homo,    "Homogeneity"),
  extract_overall(res_overall_shift,   "Composition shift")
)

sumtab
#write.csv(sumtab,"data/specific_use_total_forest_plot.csv")

##combine taxa/shannon/homo/shift forest plot----
df <- read.csv("data/specific_use_total_forest_plot.csv")
df <- df %>%
  mutate(label_n = paste0(group, " (", n, ")"))

# fig
total_foest <- ggplot(df, aes(y = label_n, x = mean, color = label_n)) +
  geom_errorbarh(aes(xmin = ci95_lower, xmax = ci95_upper),
                 width = 0, size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey0", size = 1) +
  geom_point(size = 5) +
  scale_color_manual(values = c(
    "Taxonomic richness (171)" = "#1B5F9E",
    "Shannon diversity (112)" = "#008b8b",
    "Composition shift (57)" = "#eead0e",
    "Homogeneity (57)" = "grey40"
  )) +
  labs(
    x = "LRR (95% credible interval)",
    y = NULL
  ) +
  scale_x_continuous(breaks = seq(floor(min(df$ci95_lower)), ceiling(max(df$ci95_upper)), by = 0.5)) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_blank(),
    strip.text = element_text(face = "bold"),
    panel.border = element_rect(color = "#7AC5CD", fill = NA, linewidth = 2) # 给panel加外框
  )+
  theme(legend.position = "none")

total_foest

##four histogram----
taxa_h <- ggplot(LRR_taxa_weighted, aes(x = yi)) +
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

##plots combine_2----
##colum
right_plots_2 <- (taxa_h | shannon_h) / (homo_h | shift_h )
colum_plot <- right_plots_2 + 
  plot_annotation(tag_levels = list(c("a","b", "c", "d"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))
##forest
total_foest <- total_foest+
  plot_annotation(tag_levels = list(c("a"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))
##comb
# read images
img1 <- magick::image_read("plot/overall_forest.png")
img2 <- magick::image_read("plot/four_histogram_lay.png")

target_width <- 800

img1_resized <- image_scale(img1, paste0(target_width))
img2_resized <- image_scale(img2, paste0(target_width))

combined_img <- image_append(c(img1_resized, img2_resized), stack = TRUE)

print(combined_img)

#PNG
image_write(combined_img, "plot/total_forest.png", 
            format = "png", 
            quality = 100,  # 最高质量
            density = 300)   # 高DPI
