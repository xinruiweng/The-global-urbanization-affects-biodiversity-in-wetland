#regression

library(metafor)
LRR_homo_weighted <- read.csv("LRR/LRR_taxa_weighted.csv")
LRR_homo_weighted$log_scale <-
  log10(LRR_homo_weighted$scale_largest_distance_km)

mod <- rma.mv(yi,vi,
  mods = ~ log_scale,
  random = ~ 1 | Study_ID/Plot_ID,
  data = LRR_homo_weighted,
  method = "REML"
)

summary(mod)

# 点大小（inverse variance weight）
weights <- 1 / LRR_homo_weighted$vi

plot(
  LRR_homo_weighted$log_scale,
  LRR_homo_weighted$yi,
  cex = sqrt(weights) / 2,
  pch = 21,
  bg = "lightblue",
  xlab = "log10(Spatial scale km)",
  ylab = "Effect size"
)

# regression line
newx <- seq(
  min(LRR_homo_weighted$log_scale),
  max(LRR_homo_weighted$log_scale),
  length = 100
)

pred <- predict(
  mod,
  newmods = newx
)

lines(
  newx,
  pred$pred,
  lwd = 2,
  col = "red"
)