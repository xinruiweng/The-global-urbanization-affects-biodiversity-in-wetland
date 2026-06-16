#shift_moderators_four_dorest_plot
library(metafor)
library(dplyr)
library(ggplot2)
library(multcomp)
library(patchwork)
install.packages("patchwork")
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
model_shift_reference <- rma.mv(yi, vi,
                             mods = ~ reference_type, 
                             random = ~ 1 | Study_ID/Plot_ID,
                             data = LRR_shift_weighted, 
                             method = "REML")
#提取mean 95%CI----
##taxa_grouped----
LRR_shift_weighted$taxa_grouped <- factor(LRR_shift_weighted$taxa_grouped)
#构建每个分类的新数据
newdat <- data.frame(
  taxa_grouped = levels(LRR_shift_weighted$taxa_grouped)
)
#构建设计矩阵
X <- model.matrix(~ taxa_grouped, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_biome,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  taxa_grouped = newdat$taxa_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_biome.csv",
  row.names = FALSE
)


##wetland_type_grouped----
LRR_shift_weighted$wetland_type_grouped <- factor(LRR_shift_weighted$wetland_type_grouped)
#构建每个分类的新数据
newdat <- data.frame(
  wetland_type_grouped = levels(LRR_shift_weighted$wetland_type_grouped)
)
#构建设计矩阵
X <- model.matrix(~ wetland_type_grouped, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_wetland_type,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  wetland_type_grouped = newdat$wetland_type_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_wetland.csv",
  row.names = FALSE
)

##scale----
LRR_shift_weighted$scale_grouped <- factor(LRR_shift_weighted$scale_grouped)
#构建每个分类的新数据
newdat <- data.frame(
  scale_grouped = levels(LRR_shift_weighted$scale_grouped)
)
#构建设计矩阵
X <- model.matrix(~ scale_grouped, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_scale,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  scale_grouped = newdat$scale_grouped,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/shiftresul_scale.csv",
  row.names = FALSE
)


##income region----
LRR_shift_weighted$income_region <- factor(LRR_shift_weighted$income_region)
#构建每个分类的新数据
newdat <- data.frame(
  income_region = levels(LRR_shift_weighted$income_region)
)
#构建设计矩阵
X <- model.matrix(~ income_region, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_income,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  income_region = newdat$income_region,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/supple_use/shiftresul_income.csv",
  row.names = FALSE
)

#koppen----
LRR_shift_weighted$koppen_climate <- factor(LRR_shift_weighted$koppen_climate)
#构建每个分类的新数据
newdat <- data.frame(
  koppen_climate = levels(LRR_shift_weighted$koppen_climate)
)
#构建设计矩阵
X <- model.matrix(~ koppen_climate, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_koppen,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  koppen_climate = newdat$koppen_climate,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/supple_use/shiftresul_koppen.csv",
  row.names = FALSE
)
##reference----
LRR_shift_weighted$reference_type <- factor(LRR_shift_weighted$reference_type)
#构建每个分类的新数据
newdat <- data.frame(
  reference_type = levels(LRR_shift_weighted$reference_type)
)
#构建设计矩阵
X <- model.matrix(~ reference_type, data = newdat)
#计算marginal mean 和95%CI
pred <- predict(
  model_shift_reference,
  newmods = X[, -1],   # 去掉截距列
  transf = NULL
)
#生成dataframe
shift_marginal_means <- data.frame(
  reference_type = newdat$reference_type,
  estimate = pred$pred,
  ci_lb = pred$ci.lb,
  ci_ub = pred$ci.ub
)
#导出
write.csv(
  shift_marginal_means,
  "model_mean_95ci/shift/supple_use/shiftresul_reference.csv",
  row.names = FALSE
)

##forest plot----
df <- read.csv("data/shift_mixed_model_mean_95ci.csv")

df <- df %>%
  mutate(label_n = paste0(lable, " (", n, ")"))

# 控制分组顺序
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

#df <- df |>
#  dplyr::mutate(
#    lable = dplyr::case_when(
#      group == "Biological group" ~ factor(lable, levels = bio_order),
#      group == "Wetland type"     ~ factor(lable, levels = wetland_order),
#      group == "Scale"     ~ factor(lable, levels = scale_order),
#      TRUE                        ~ factor(lable)
#    )
#  )

###柱状图拼图----
x_lim <- range(df$CI_lower, df$CI_upper, na.rm = TRUE)

base_plot <- function(data) {
  data <- data %>%
    mutate(
      color_group = ifelse(
        CI_lower <= 0 & CI_upper >= 0,  # 误差线跨越0
        "cross_zero",  # 跨越0的颜色组
        "not_cross_zero"  # 不跨越0的颜色组
      )
    )
  
  ggplot(data, aes(
    x = Estimate,
    y = lable
  )) +
    # 柱状图 - 根据是否跨越0设置颜色
    geom_col(
      aes(fill = color_group),  # 使用颜色分组
      width = 0.6,
      alpha = 0.8
    ) +
    # 误差线 - 根据是否跨越0设置颜色
    geom_errorbarh(
      aes(
        xmin = CI_lower, 
        xmax = CI_upper,
        color = color_group  # 使用颜色分组
      ),
      height = 0,  # 误差线两端的高度
      linewidth = 0.8,
      alpha = 0.7
    ) +
    # 参考线
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      color = "grey40",
      linewidth = 1
    ) +
    # 坐标轴范围
    scale_x_continuous(limits = x_lim) +
    facet_wrap(~ group, scales = "free_y", ncol = 1,strip.position = "right") +
    # 标签
    labs(x = "LRR shift", y = "") +
    # 颜色方案
    scale_fill_manual(
      values = c(
        "cross_zero" = "grey50",     # 跨越0：灰色
        "not_cross_zero" = "#eead0e"  # 不跨越0：#eead0e
      ),
      guide = "none"  # 不显示图例
    ) +
    scale_color_manual(
      values = c(
        "cross_zero" = "grey50",     # 跨越0：灰色
        "not_cross_zero" = "#eead0e"  # 不跨越0：#eead0e
      ),
      guide = "none"  # 不显示图例
    ) +
    # 主题
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
        size = 14,  # 字体大小
        face = "bold",  # 字体粗细
        margin = margin(t = 5, r = 5, b = 5, l = 5)  # 边距
      )
    )
}

# 创建三个子图
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

shift_plot <- (p4/ p1 / p2 / p3/p6/p5) +
  plot_layout(heights = c(1, 8, 5, 5,3,4))

homo_shift_comb <- (homo_plot|shift_plot)+
  plot_layout(widths = c( 1, 1))+ 
  plot_annotation(tag_levels = list(c("a"," "," ", " ", "","","b"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))
homo_shift_comb
##supple横相且richciness与shannon合在一起----
# 读取数据
hs <- read.csv("model_mean_95ci/supple_use_comb/homo_shift_mixed_model_mean_95ci.csv")

# 创建颜色分组变量
hs <- hs %>%
  mutate(
    color_group = case_when(
      # 误差线经过0的情况
      CI_lower <= 0 & CI_upper >= 0 ~ "cross_zero",
      # 误差线不经过0，且是Taxonomic richness
      index == "Homogeneity" ~ "Homogeneity_not_cross",
      # 误差线不经过0，且是Shannon
      index == "Shift" ~ "Shift_not_cross"
    )
  )

# 定义点的形状映射
shape_mapping <- c(
  "Homogeneity" = 16,  # 圆形
  "Shift" = 15             # 方形
)

# 定义颜色映射
color_mapping <- c(
  "cross_zero" = "grey70",           # 经过0的误差线
  "Homogeneity_not_cross" = "#eead0e",  # Taxonomic richness不经过0
  "Shift_not_cross" = "#eead0e"     # Shannon不经过0
)

# 创建基础图形
base_plot <- ggplot(hs, aes(
  x = lable,
  y = Estimate,
  shape = index
)) +
  # 添加误差线
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
  # 添加点
  geom_point(
    aes(color = color_group),
    size = 5,
    position = position_dodge(width = 0.5)
  ) +
  # 添加水平参考线
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40",
    linewidth = 0.8
  ) +
  # 添加n值标签
  geom_text(
    aes(
      y = CI_lower,
      label = paste0("(", n, ")")
    ),
    position = position_dodge(width = 1),
    vjust = 1,  # 稍微向下偏移
    hjust = 0.5,  # 居中对齐
    size = 4.5,
    color = "black"
  ) +
  # 分面
  facet_grid(
    . ~ group,
    scales = "free_x",
    space = "free_x"
  ) +
  # 形状映射
  scale_shape_manual(
    name = "Index",
    values = shape_mapping,
    guide = guide_legend(
      override.aes = list(color = "black", size = 5)
    )
  ) +
  # 颜色映射
  scale_color_manual(
    values = color_mapping,
    guide = "none"  # 不显示图例
  ) +
  # 坐标轴标签
  labs(
    x = "",
    y = "Estimate with 95% CI"
  ) +
  # 主题设置
  theme_minimal(base_size = 17) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,  # 居中对齐
      vjust = 1,
      size = 16,    # 字体大小16
      color = "black",# 字体颜色黑色
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 16,    # y轴字体大小
      color = "black",face = "bold"
    ),
    axis.title.y = element_text(
      size = 17,    # y轴标题字体大小
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
    )  # group标签
  )

# 显示图形
print(base_plot)