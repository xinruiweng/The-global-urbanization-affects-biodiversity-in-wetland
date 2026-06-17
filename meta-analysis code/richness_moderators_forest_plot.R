library(metafor)
library(dplyr)
library(ggplot2)
library(patchwork)

##LRR richness data import----
LRR_richness_weighted <- read.csv("LRR/LRR_richness_weighted.csv")
##mixed model for richness----
model_richness_biome <- rma.mv(yi, vi,
                      mods = ~ taxa_grouped, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_richness_weighted, 
                      method = "REML") 

model_richness_wetland_type <- rma.mv(yi, vi,
                      mods = ~ wetland_type_grouped, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_taxa_weighted, 
                      method = "REML") 

model_richness_scale <- rma.mv(yi, vi,
                      mods = ~ scale_grouped, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_taxa_weighted, 
                      method = "REML") 

model_richness_koppen <- rma.mv(yi, vi,
                           mods = ~ koppen_climate, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_richness_weighted, 
                           method = "REML") 

model_richness_income <- rma.mv(yi, vi,
                           mods = ~ income_region, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_richness_weighted, 
                           method = "REML") 
model_richness_reference <- rma.mv(yi, vi,
                            mods = ~ reference_type, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_richness_weighted, 
                            method = "REML")
##extract_mean_95%CI----
##richness_grouped----
LRR_richness_weighted$taxa_grouped <- factor(LRR_richness_weighted$taxa_grouped)
newdat <- data.frame(
  taxa_grouped = levels(LRR_richness_weighted$taxa_grouped)
)

X <- model.matrix(~ taxa_grouped, data = newdat)
#marginal mean and 95%CI
pred <- predict(
  model_richness_biome,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  taxa_grouped = newdat$taxa_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/richnessresul_taxa.csv",
  row.names = FALSE
)

##wetland_type_grouped----
LRR_richness_weighted$wetland_type_grouped <- factor(LRR_richness_weighted$wetland_type_grouped)

newdat <- data.frame(
  wetland_type_grouped = levels(LRR_richness_weighted$wetland_type_grouped)
)
X <- model.matrix(~ wetland_type_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_richness_wetland_type,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  wetland_type_grouped = newdat$wetland_type_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/richnessresul_wetland.csv",
  row.names = FALSE
)

##scale----
LRR_richness_weighted$scale_grouped <- factor(LRR_richness_weighted$scale_grouped)

newdat <- data.frame(
  scale_grouped = levels(LRR_richness_weighted$scale_grouped)
)

X <- model.matrix(~ scale_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_richness_scale,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  scale_grouped = newdat$scale_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/richnessresul_scale.csv",
  row.names = FALSE
)


##koppen----
LRR_richness_weighted$koppen_climate <- factor(LRR_richness_weighted$koppen_climate)

newdat <- data.frame(
  koppen_climate = levels(LRR_richness_weighted$koppen_climate)
)

X <- model.matrix(~ koppen_climate, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_koppen,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  koppen_climate = newdat$koppen_climate,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/supple_use/richnessresul_koppen.csv",
  row.names = FALSE
)
##income----
LRR_richness_weighted$income_region <- factor(LRR_richness_weighted$income_region)

newdat <- data.frame(
  income_region = levels(LRR_richness_weighted$income_region)
)

X <- model.matrix(~ income_region, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_richness_income,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  income_region = newdat$income_region,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/richnessresul_income.csv",
  row.names = FALSE
)

##reference----
LRR_richness_weighted$reference_type <- factor(LRR_richness_weighted$reference_type)

newdat <- data.frame(
  reference_type = levels(LRR_richness_weighted$reference_type)
)

X <- model.matrix(~ reference_type, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_richness_reference,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
richness_marginal_means <- data.frame(
  reference_type = newdat$reference_type,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  richness_marginal_means,
  "model_mean_95ci/richness/richnessresul_reference.csv",
  row.names = FALSE
)


##forest plot data----
df <- read.csv("data/richness_mean_95ci.csv")

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
      aes(x = 0.02, label = paste0("(", n,")")), 
      hjust = -0.1,
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
    # errorbar
    geom_errorbarh(
      aes(
        xmin = CI_lower,
        xmax = CI_upper,
        color = ifelse(CI_lower <= 0 & CI_upper >= 0, "cross_zero", "not_cross_zero")
      ),
      height = 0,
      linewidth = 0.8
    ) +
    # point
    geom_point(
      aes(
        size = n,
        color = ifelse(CI_lower <= 0 & CI_upper >= 0, "cross_zero", "not_cross_zero")
      ),
      shape = 16
    ) +
    scale_x_continuous(limits = x_lim) +
    labs(x = "LRR taxonomic richness", y = "") +
    
    scale_color_manual(
      values = c(
        "cross_zero" = "grey70",
        "not_cross_zero" = "#1B5F9E"
      ),
      guide = "none"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 15),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_rect(
        color = "#7AC5CD",
        fill = NA,
        linewidth = 1.5
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

richness_plot <- (p4/ p1 / p2 / p3/p6/p5) +
  plot_layout(heights = c(1, 9, 6, 5,4,4))

richness_plot
