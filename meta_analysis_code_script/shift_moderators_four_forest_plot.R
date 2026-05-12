library(metafor)
library(dplyr)
library(ggplot2)
library(multcomp)
library(patchwork)
##LRR shift import----
LRR_shift_weighted <- read.csv("LRR/LRR_shift_weighted.csv")
##four mixed model for homo----
model_shift_biome <- rma.mv(yi, vi,
                           mods = ~ taxa_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_shift_weighted, 
                           method = "REML")

model_shift_scale <- rma.mv(yi, vi,
                           mods = ~ scale_grouped, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_shift_weighted, 
                           method = "REML")

model_shift_wetland_type <- rma.mv(yi, vi,
                                  mods = ~ wetland_type_grouped, 
                                  random = ~ 1 | Study_ID/Plot_ID,
                                  data = LRR_shift_weighted, 
                                  method = "REML")
##supple----
model_shift_koppen <- rma.mv(yi, vi,
                            mods = ~ koppen_climate, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_shift_weighted, 
                            method = "REML")

model_shift_income <- rma.mv(yi, vi,
                            mods = ~ income_region, 
                            random = ~ 1 | Study_ID/Plot_ID,
                            data = LRR_shift_weighted, 
                            method = "REML") 

#mean/ 95%CI----
##taxa_grouped----
LRR_shift_weighted$taxa_grouped <- factor(LRR_shift_weighted$taxa_grouped)

newdat <- data.frame(
  taxa_grouped = levels(LRR_shift_weighted$taxa_grouped)
)

X <- model.matrix(~ taxa_grouped, data = newdat)

pred <- predict(
  model_shift_biome,
  newmods = X[, -1],
  transf = NULL
)

shift_marginal_means <- data.frame(
  taxa_grouped = newdat$taxa_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)

write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_biome.csv",
  row.names = FALSE
)


##wetland_type_grouped----
LRR_shift_weighted$wetland_type_grouped <- factor(LRR_shift_weighted$wetland_type_grouped)

newdat <- data.frame(
  wetland_type_grouped = levels(LRR_shift_weighted$wetland_type_grouped)
)

X <- model.matrix(~ wetland_type_grouped, data = newdat)

pred <- predict(
  model_shift_wetland_type,
  newmods = X[, -1],
  transf = NULL
)

shift_marginal_means <- data.frame(
  wetland_type_grouped = newdat$wetland_type_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)

write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_wetland.csv",
  row.names = FALSE
)

##scale----
LRR_shift_weighted$scale_grouped <- factor(LRR_shift_weighted$scale_grouped)

newdat <- data.frame(
  scale_grouped = levels(LRR_shift_weighted$scale_grouped)
)

X <- model.matrix(~ scale_grouped, data = newdat)

pred <- predict(
  model_shift_scale,
  newmods = X[, -1],
  transf = NULL
)

shift_marginal_means <- data.frame(
  scale_grouped = newdat$scale_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)

write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_scale.csv",
  row.names = FALSE
)


##income region----
LRR_shift_weighted$income_region <- factor(LRR_shift_weighted$income_region)

newdat <- data.frame(
  income_region = levels(LRR_shift_weighted$income_region)
)

X <- model.matrix(~ income_region, data = newdat)

pred <- predict(
  model_shift_income,
  newmods = X[, -1],  
  transf = NULL
)

shift_marginal_means <- data.frame(
  income_region = newdat$income_region,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)

write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/supple_use/shiftresul_income.csv",
  row.names = FALSE
)

#koppen----
LRR_shift_weighted$koppen_climate <- factor(LRR_shift_weighted$koppen_climate)

newdat <- data.frame(
  koppen_climate = levels(LRR_shift_weighted$koppen_climate)
)

X <- model.matrix(~ koppen_climate, data = newdat)

pred <- predict(
  model_shift_koppen,
  newmods = X[, -1],  
  transf = NULL
)

shift_marginal_means <- data.frame(
  koppen_climate = newdat$koppen_climate,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)

write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/supple_use/shiftresul_koppen.csv",
  row.names = FALSE
)

##forest plot----
df <- read.csv("data/shift_mixed_model_mean_95ci.csv")

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
  "Resevoir/pond",
  "Lake",
  "Inland vegetated wetland",
  "Coastal wetland"
)
scale_order <- c("s≤10 km",
                 "10<s≤50 km",
                 "50<s≤100 km",
                 "100<s≤200 km",
                 "s>200 km"
)
df <- df |>
  dplyr::mutate(
    lable = dplyr::case_when(
      group == "Biological group" ~ factor(lable, levels = rev(bio_order)),
      group == "Wetland type"     ~ factor(lable, levels = rev(wetland_order)),
      group == "Scale"     ~ factor(lable, levels = rev(scale_order)),
      TRUE                        ~ factor(lable)
    )
  )


##graph----
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
    facet_wrap(~ group, scales = "free_y", ncol = 1,strip.position = "right") +

    labs(x = "LRR shift", y = "") +

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
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(
        size = 14, 
        face = "bold",
        margin = margin(t = 5, r = 5, b = 5, l = 5)
      )
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
  theme(axis.line.x = element_line(color = "black", linewidth = 0.7))

p4 <- base_plot(subset(df, group == "All")) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())

shift_plot <- (p4/ p1 / p2 / p3) +
  plot_layout(heights = c(1, 8, 5, 5))

homo_shift_comb <- (homo_plot|shift_plot)+
  plot_layout(widths = c( 1, 1))+ 
  plot_annotation(tag_levels = list(c("a"," "," ", " ", "b"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

