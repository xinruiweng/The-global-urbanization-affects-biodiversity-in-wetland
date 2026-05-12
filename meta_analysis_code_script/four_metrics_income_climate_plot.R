hs <- read.csv("model_mean_95ci/supple_use_comb/homo_shift_mixed_model_mean_95ci.csv")


hs <- hs %>%
  mutate(
    color_group = case_when(
      CI_lower <= 0 & CI_upper >= 0 ~ "cross_zero",
      index == "Homogeneity" ~ "Homogeneity_not_cross",
      index == "Shift" ~ "Shift_not_cross"
    )
  )

shape_mapping <- c(
  "Homogeneity" = 16, 
  "Shift" = 15
)

color_mapping <- c(
  "cross_zero" = "grey70",
  "Homogeneity_not_cross" = "#eead0e", 
  "Shift_not_cross" = "#eead0e" 
)

base_plot <- ggplot(hs, aes(
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
      color = "black",
      face = "bold"
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