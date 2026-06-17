library(metafor)
library(dplyr)
library(ggplot2)
library(patchwork)
##LRR homo import----
LRR_homo_weighted <- read.csv("LRR/LRR_homogeneity_weighted.csv")
##four mixed model for homo----
model_homo_biome <- rma.mv(yi, vi,
                           mods = ~ taxa_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_homo_weighted, 
                           method = "REML")

model_homo_scale <- rma.mv(yi, vi,
                           mods = ~ scale_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_homo_weighted, 
                           method = "REML")

model_homo_wetland_type <- rma.mv(yi, vi,
                                  mods = ~ wetland_type_grouped, 
                                  random = ~ 1 | Study_ID/Plot_ID,
                                  data = LRR_homo_weighted, 
                                  method = "REML")

model_homo_koppen <- rma.mv(yi, vi,
                            mods = ~ koppen_climate, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_homo_weighted, 
                            method = "REML")

model_homo_income <- rma.mv(yi, vi,
                            mods = ~ income_region, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_homo_weighted, 
                            method = "REML") 
model_homo_reference <- rma.mv(yi, vi,
                            mods = ~ reference_type, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_homo_weighted, 
                            method = "REML")
#mean 95%CI----
##taxa_grouped----
LRR_homo_weighted$taxa_grouped <- factor(LRR_homo_weighted$taxa_grouped)

newdat <- data.frame(
  taxa_grouped = levels(LRR_homo_weighted$taxa_grouped)
)

X <- model.matrix(~ taxa_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_biome,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  taxa_grouped = newdat$taxa_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/homoresul_biome.csv",
  row.names = FALSE
)

##scale----
LRR_homo_weighted$scale_grouped <- factor(LRR_homo_weighted$scale_grouped)

newdat <- data.frame(
  scale_grouped = levels(LRR_homo_weighted$scale_grouped)
)

X <- model.matrix(~ scale_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_scale,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  scale_grouped = newdat$scale_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/homoresul_scale.csv",
  row.names = FALSE
)


##wetland_type_grouped----
LRR_homo_weighted$wetland_type_grouped <- factor(LRR_homo_weighted$wetland_type_grouped)

newdat <- data.frame(
  wetland_type_grouped = levels(LRR_homo_weighted$wetland_type_grouped)
)

X <- model.matrix(~ wetland_type_grouped, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_wetland_type,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  wetland_type_grouped = newdat$wetland_type_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/homoresul_wetland.csv",
  row.names = FALSE
)

##koppen----
LRR_homo_weighted$koppen_climate <- factor(LRR_homo_weighted$koppen_climate)

newdat <- data.frame(
  koppen_climate = levels(LRR_homo_weighted$koppen_climate)
)

X <- model.matrix(~ koppen_climate, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_koppen,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  koppen_climate = newdat$koppen_climate,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/supple_use/homoresul_koppen.csv",
  row.names = FALSE
)



##income region----
LRR_homo_weighted$income_region <- factor(LRR_homo_weighted$income_region)

newdat <- data.frame(
  income_region = levels(LRR_homo_weighted$income_region)
)

X <- model.matrix(~ income_region, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_income,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  income_region = newdat$income_region,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/homoresul_income.csv",
  row.names = FALSE
)
#reference----
LRR_homo_weighted$reference_type <- factor(LRR_homo_weighted$reference_type)

newdat <- data.frame(
  reference_type = levels(LRR_homo_weighted$reference_type)
)

X <- model.matrix(~ reference_type, data = newdat)
#marginal mean 95%CI
pred <- predict(
  model_homo_reference,
  newmods = X[, -1],
  transf = NULL
)
#dataframe
homo_marginal_means <- data.frame(
  reference_type = newdat$reference_type,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#output
write.csv(
  homo_marginal_means,
  "model_mean_95ci/homo/homoresul_reference.csv",
  row.names = FALSE
)

##forest plot----
df <- read.csv("data/homogeneity_mean_95ci.csv")

df <- df %>%
  mutate(label_n = paste0(lable, " (", n, ")"))



bio_order <- c(
  "Bacteria",
  "Algae",
  "Plant",
  "Zooplankton",
  "Macroinvertebrate",
  "Fish",
  "Bird",
  "Others"
)

wetland_order <- c(
  "River",
  "Reservoir/pond",
  "Lake",
  "Inland vegetated wetland",
  "Coastal wetland"
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

##plot----
x_lim <- range(df$CI_lower, df$CI_upper, na.rm = TRUE)

base_plot <- function(data) {
  data <- data %>%
    mutate(
      color_group = ifelse(
        CI_lower <= 0 & CI_upper >= 0,
        "cross_zero",
        "not_cross_zero"
      )
    )
  
  ggplot(data, aes(
    x = Estimate,
    y = lable
  )) +
    #color
    geom_col(
      aes(fill = color_group),
      width = 0.6,
      alpha = 0.8
    ) +
    
    geom_errorbarh(
      aes(
        xmin = CI_lower, 
        xmax = CI_upper,
        color = color_group
      ),
      height = 0,
      linewidth = 0.8,
      alpha = 0.7
    ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey40",
      linewidth = 1
    ) +
    
    scale_x_continuous(limits = x_lim) +
    
    labs(x = "LRR homogeneity", y = "") +
    
    scale_fill_manual(
      values = c(
        "cross_zero" = "grey50",
        "not_cross_zero" = "#eead0e"
      ),
      guide = "none"
    ) +
    scale_color_manual(
      values = c(
        "cross_zero" = "grey50",
        "not_cross_zero" = "#eead0e"
      ),
      guide = "none"
    ) +
    scale_y_discrete(
      labels = function(x) {
        lab_map <- data |>
          dplyr::distinct(lable, label_n) |>
          tibble::deframe()
        lab_map[x]
      }
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 14),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
}


p1 <- base_plot(subset(df, group == "Biological group")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

p2 <- base_plot(subset(df, group == "Wetland type")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

p3 <- base_plot(subset(df, group == "Scale")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p4 <- base_plot(subset(df, group == "All")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
p6 <- base_plot(subset(df, group == "Income")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
p5 <- base_plot(subset(df, group == "Reference")) +
  theme(axis.line.x = element_line(color = "black", linewidth = 0.7))

homo_plot <- (p4/ p1 / p2 / p3/p6/p5) +
  plot_layout(heights = c(1, 8, 5, 5,3,4))

print(homo_plot)
