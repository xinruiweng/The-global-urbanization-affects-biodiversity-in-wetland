library(metafor)
library(dplyr)
library(ggplot2)
library(patchwork)
##LRR shannon data import----
LRR_shannon_weighted <- read.csv("LRR/LRR_shannon_weighted.csv")
##four mixed model for shannon----
model_shannon_biome <- rma.mv(yi, vi,
                           mods = ~ taxa_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_shannon_weighted, 
                           method = "REML") 

model_shannon_wetland_type <- rma.mv(yi, vi,
                                  mods = ~ wetland_type_grouped, 
                                  random = ~ 1 | Study_ID/Plot_ID,
                                  data = LRR_shannon_weighted, 
                                  method = "REML") 

model_shannon_scale <- rma.mv(yi, vi,
                           mods = ~ scale_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_shannon_weighted, 
                           method = "REML") 

model_shannon_koppen <- rma.mv(yi, vi,
                              mods = ~ koppen_climate, 
                              random = ~ 1 | Study_ID/Plot_ID,
                              data = LRR_shannon_weighted, 
                              method = "REML")
model_shannon_income <- rma.mv(yi, vi,
                               mods = ~ income_region, 
                               random = ~ 1 | Study_ID/Plot_ID,
                               data = LRR_shannon_weighted, 
                               method = "REML")
model_shannon_reference <- rma.mv(yi, vi,
                               mods = ~ reference_type, 
                               random = ~ 1 | Study_ID/Plot_ID,
                               data = LRR_shannon_weighted, 
                               method = "REML")

##extra_mean_95%CI----
##taxa_grouped----
LRR_shannon_weighted$taxa_grouped <- factor(LRR_shannon_weighted$taxa_grouped)

newdat <- data.frame(
  taxa_grouped = levels(LRR_shannon_weighted$taxa_grouped)
)

X <- model.matrix(~ taxa_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_biome,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  taxa_grouped = newdat$taxa_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/shannonresul_taxa.csv",
  row.names = FALSE
)

##wetland_type_grouped----
LRR_shannon_weighted$wetland_type_grouped <- factor(LRR_shannon_weighted$wetland_type_grouped)

newdat <- data.frame(
  wetland_type_grouped = levels(LRR_shannon_weighted$wetland_type_grouped)
)

X <- model.matrix(~ wetland_type_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_wetland_type,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  wetland_type_grouped = newdat$wetland_type_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/shannonresul_wetland.csv",
  row.names = FALSE
)

##scale----
LRR_shannon_weighted$scale_grouped <- factor(LRR_shannon_weighted$scale_grouped)

newdat <- data.frame(
  scale_grouped = levels(LRR_shannon_weighted$scale_grouped)
)

X <- model.matrix(~ scale_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_scale,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  scale_grouped = newdat$scale_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/shannonresul_scale.csv",
  row.names = FALSE
)

##koppen----
LRR_shannon_weighted$koppen_climate <- factor(LRR_shannon_weighted$koppen_climate)

newdat <- data.frame(
  koppen_climate = levels(LRR_shannon_weighted$koppen_climate)
)

X <- model.matrix(~ koppen_climate, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_koppen,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  koppen_climate = newdat$koppen_climate,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/supple_use/shannonresul_koppen.csv",
  row.names = FALSE
)
##income----
LRR_shannon_weighted$income_region <- factor(LRR_shannon_weighted$income_region)

newdat <- data.frame(
  income_region = levels(LRR_shannon_weighted$income_region)
)

X <- model.matrix(~ income_region, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_income,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  income_region = newdat$income_region,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/shannonresul_income.csv",
  row.names = FALSE
)

##reference----
LRR_shannon_weighted$reference_type <- factor(LRR_shannon_weighted$reference_type)

newdat <- data.frame(
  reference_type = levels(LRR_shannon_weighted$reference_type)
)

X <- model.matrix(~ reference_type, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_shannon_reference,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
shannon_marginal_means <- data.frame(
  reference_type = newdat$reference_type,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  shannon_marginal_means,
  "model_mean_95ci/shannon/shannonresul_reference.csv",
  row.names = FALSE
)

##forest plot----

df <- read.csv("data/shannon_mean_95ci.csv")

df <- df %>%
  mutate(label_n = paste0(lable, " (", n, ")"))


bio_order <- c(
  "Bacteria",
  "Algae",
  "Plant",
  "Zooplankton",
  "Macroinvertebrate",
  "Fish",
  "Amphibian",
  "Bird",
  "Others"
)

wetland_order <- c(
  "River",
  "Reservoir/pond",
  "Lake",
  "Inland vegetated wetland",
  "Coastal wetland",
  "Others"
)
scale_order <- c("less than 10 km",
                 "(10, 50] km",
                 "(50, 100] km",
                 "(100, 200] km",
                 "more than 200 km"
)
income_order <- c("High-income",
                  "Upper-middle-income",
                  "Lower-middle-income",
                  "Low-income"
)
reference_order <- c("Natural vegetation",
                     "Semi-natural vegetation",
                     "Intensively managed",
                     "Peri-urban"
)
df <- df |>
  dplyr::mutate(
    lable = dplyr::case_when(
      group == "Biological group" ~ factor(lable, levels = rev(bio_order)),
      group == "Wetland type"     ~ factor(lable, levels = rev(wetland_order)),
      group == "Scale"     ~ factor(lable, levels = rev(scale_order)),
      group == "Income"     ~ factor(lable, levels = rev(income_order)),
      group == "Reference"     ~ factor(lable, levels = rev(reference_order)),
      TRUE                        ~ factor(lable)
    )
  )


##combine----
x_lim <- range(df$CI_lower, df$CI_upper, na.rm = TRUE)
base_plot <- function(data) {
  ggplot(data, aes(
    x = Estimate,
    y = lable,
    xmin = CI_lower,
    xmax = CI_upper,
    color = group
  )) +
    geom_point(aes(size = n), shape = 16) +
    scale_size(range = c(3, 6)) +
    geom_errorbarh(height = 0, linewidth = 0.8) +
    geom_text(
      aes(x = 0.2, label = paste0("(", n,")")), 
      hjust = -0.2,
      vjust = 0.5,
      size = 4,
      color = "black",
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey40",
      linewidth = 1
    ) +
    geom_errorbarh(
      aes(
        xmin = CI_lower,
        xmax = CI_upper,
        color = ifelse(CI_lower <= 0 & CI_upper >= 0, "cross_zero", "not_cross_zero")
      ),
      height = 0,
      linewidth = 0.8
    ) +
    #point
    geom_point(
      aes(
        size = n,
        color = ifelse(CI_lower <= 0 & CI_upper >= 0, "cross_zero", "not_cross_zero")
      ),
      shape = 16
    ) +
    facet_wrap(~ group, scales = "free_y", ncol = 1,strip.position = "right") +
    scale_x_continuous(limits = x_lim) +
    labs(x = "LRR Shannon", y = "") +
    scale_color_manual(
      values = c(
        "cross_zero" = "grey70",
        "not_cross_zero" = "#008b8b"
      ),
      guide = "none"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(
        color = "#7AC5CD",
        fill = NA,
        linewidth = 1.5
      ),
      strip.text = element_text(
        size = 15,
        face = "bold",
        margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
    )
}
p1 <- base_plot(subset(df, group == "Biological group"))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5)
  )

p2 <- base_plot(subset(df, group == "Wetland type"))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5)
  )

p3 <- base_plot(subset(df, group == "Scale"))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5)
  )

p4 <- base_plot(subset(df, group == "All"))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5)
  )
p6 <- base_plot(subset(df, group == "Income"))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(1.5, 1.5, 1.5, 1.5)
  )
p5 <- base_plot(subset(df, group == "Reference"))+
  theme(plot.margin = margin(1.5, 1.5, 1.5, 1.5))

shannon_plot <- (p4/ p1 / p2 / p3/p6/p5) +
  plot_layout(heights = c(1, 9, 6, 5,4,4))

richness_shannon_comb <- (richness_plot|shannon_plot)+
  plot_layout(widths = c(1, 0.68))+ 
  plot_annotation(tag_levels = list(c("a"," "," ", " ", "","","b"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

richness_shannon_comb

##richness_shannon_climate_zone----

cb <- read.csv("model_mean_95ci/supple_use_comb/richness_shannon_climate_mean_95ci.csv")

cb <- cb %>%
  mutate(
    color_group = case_when(
      
      CI_lower <= 0 & CI_upper >= 0 ~ "cross_zero",
      
      index == "Taxonomic richness" ~ "taxonomic_not_cross",
      
      index == "Shannon" ~ "shannon_not_cross"
    )
  )

#point
shape_mapping <- c(
  "Taxonomic richness" = 16,
  "Shannon" = 15
)


color_mapping <- c(
  "cross_zero" = "grey70",           
  "taxonomic_not_cross" = "#1B5F9E",
  "shannon_not_cross" = "#008b8b"
)


base_plot <- ggplot(cb, aes(
  x = lable,
  y = Estimate,
  shape = index
)) +
  
  geom_errorbar(
    aes(
      ymin = CI_lower,
      ymax = CI_upper,
      color = color_group
    ),
    width = 0.3,
    linewidth = 1,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_point(
    aes(color = color_group),
    size = 5,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
 
  geom_text(
    aes(
      y = CI_lower,
      label = paste0("(", n, ")")
    ),
    position = position_dodge(width = 1),
    vjust = 1, 
    hjust = 0.5,
    size = 4.5,
    color = "black"
  ) +
  
  facet_grid(
    . ~ group,
    scales = "free_x",
    space = "free_x"
  ) +
  
  scale_shape_manual(
    name = "Index",
    values = shape_mapping,
    guide = guide_legend(
      override.aes = list(color = "black", size = 5)
    )
  ) +
 
  scale_color_manual(
    values = color_mapping,
    guide = "none"
  ) +
  
  labs(
    x = "",
    y = "Estimate with 95% CI"
  ) +
  
  theme_minimal(base_size = 17) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      vjust = 1,
      size = 16,
      color = "black"
      #face = "bold"
    ),
    axis.text.y = element_text(
      size = 16,
      color = "black",face = "bold"
    ),
    axis.title.y = element_text(
      size = 17,
      color = "black",face = "bold"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.background = element_blank(),
    strip.text = element_text(
      size = 14,    
      color = "black"
    ) 
  )

print(base_plot)
