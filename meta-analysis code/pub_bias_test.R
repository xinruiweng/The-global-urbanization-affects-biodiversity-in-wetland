##taxa_shannon_homo_shift_total forest plot
library(meta)
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
library(clubSandwich)
##read data----
LRR_taxa_weighted <-read.csv("LRR/LRR_taxa_weighted.csv")
LRR_shannon_weighted <-read.csv("LRR/LRR_shannon_weighted.csv")
LRR_homo_weighted <- read.csv("LRR/LRR_homogeneity_weighted.csv")
LRR_shift_weighted <- read.csv("LRR/LRR_shift_weighted.csv")
##Robustness test----
par(cex.lab = 1.5)   # 控制 xlab & ylab 的字体
##richness----
# 多水平Egger's test
res_egger_tax <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),  # 关键：添加标准误作为预测变量
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_taxa_weighted,
                        method = "REML")
summary(res_egger_tax)

#funnel for egger
funnel(res_egger_tax, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Taxonomic Richness")

#egger_plot
# 1. 提取数据
data_plot_t <- LRR_taxa_weighted
se <- sqrt(data_plot_t$vi)  # 标准误
precision <- 1/se  # 精度（1/SE）
x_var <- sqrt(data_plot_t$vi)  # X轴：sqrt(采样方差) = SE

# 2. 提取Egger's test的回归系数
alpha <- res_egger_tax$beta[1, 1]  # 截距
beta <- res_egger_tax$beta[2, 1]  # sqrt(vi)的系数
pval <- res_egger_tax$pval[2]  # p值

# 3. 拟合回归线（用于绘制置信区间）
lm_fit <- lm(data_plot_t$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# 4. 绘制Egger's plot
par(mar = c(5, 5, 4, 2) + 0.1)  # 增加边距

# 创建散点图（点的大小与精度成正比）
plot(x_var, data_plot_t$yi,
     xlab = expression(sqrt("Sampling Variance")),  # X轴标签
     ylab = "lnRR (Effect Size)",  # Y轴标签
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,  # 点的大小与精度成正比
     xlim = range(x_var),
     ylim = range(c(data_plot_t$yi, pred)),  # 确保y轴包含所有点
     main = "Egger's Test Plot for Taxonomic Richness",
     cex.lab = 1.2,
     cex.axis = 1.1)

# 添加回归线（Egger's test的回归线）
abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)

# 添加95%置信区间带（可选，但原图有）
polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),  # 半透明红色
        border = NA)

# 改为相对位置
x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05

# 添加Egger's test结果的文本注释
text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")

# 添加图例（精度点大小）
legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),  # 点的大小对比
       bty = "n",
       title = "Precision (1/SE)")

##shannon----
# 多水平Egger's test
res_egger_shanno <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),  # 关键：添加标准误作为预测变量
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_shannon_weighted,
                        method = "REML")
summary(res_egger_shanno)

#funnel for egger
funnel(res_egger_shanno, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Shannon Diversity")
#egger_plot
# 1. 提取数据
data_plot_s <- LRR_shannon_weighted
se <- sqrt(data_plot_s$vi)  # 标准误
precision <- 1/se  # 精度（1/SE）
x_var <- sqrt(data_plot_s$vi)  # X轴：sqrt(采样方差) = SE

# 2. 提取Egger's test的回归系数
alpha <- res_egger_shanno$beta[1, 1]  # 截距
beta <- res_egger_shanno$beta[2, 1]  # sqrt(vi)的系数
pval <- res_egger_shanno$pval[2]  # p值

# 3. 拟合回归线（用于绘制置信区间）
lm_fit <- lm(data_plot_s$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# 4. 绘制Egger's plot
par(mar = c(5, 5, 4, 2) + 0.1)  # 增加边距

# 创建散点图（点的大小与精度成正比）
plot(x_var, data_plot_s$yi,
     xlab = expression(sqrt("Sampling Variance")),  # X轴标签
     ylab = "lnRR (Effect Size)",  # Y轴标签
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,  # 点的大小与精度成正比
     xlim = range(x_var),
     ylim = range(c(data_plot_s$yi, pred)),  # 确保y轴包含所有点
     main = "Egger's Test Plot for Shannon Diversity",
     cex.lab = 1.2,
     cex.axis = 1.1)

# 添加回归线（Egger's test的回归线）
abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)

# 添加95%置信区间带（可选，但原图有）
polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),  # 半透明红色
        border = NA)

# 改为相对位置
x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05

# 添加Egger's test结果的文本注释
text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")

# 添加图例（精度点大小）
legend("top", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),  # 点的大小对比
       bty = "n",
       title = "Precision (1/SE)")

##homo----
# 多水平Egger's test
res_egger_hom <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),  # 关键：添加标准误作为预测变量
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_homo_weighted,
                        method = "REML")
summary(res_egger_hom)

#funnel for egger
funnel(res_egger_hom, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Homogeneity")
#egger_plot
# 1. 提取数据
data_plot_h <- LRR_homo_weighted
se <- sqrt(data_plot_h$vi)  # 标准误
precision <- 1/se  # 精度（1/SE）
x_var <- sqrt(data_plot_h$vi)  # X轴：sqrt(采样方差) = SE

# 2. 提取Egger's test的回归系数
alpha <- res_egger_hom$beta[1, 1]  # 截距
beta <- res_egger_hom$beta[2, 1]  # sqrt(vi)的系数
pval <- res_egger_hom$pval[2]  # p值

# 3. 拟合回归线（用于绘制置信区间）
lm_fit <- lm(data_plot_h$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# 4. 绘制Egger's plot
par(mar = c(5, 5, 4, 2) + 0.1)  # 增加边距

# 创建散点图（点的大小与精度成正比）
plot(x_var, data_plot_h$yi,
     xlab = expression(sqrt("Sampling Variance")),  # X轴标签
     ylab = "lnRR (Effect Size)",  # Y轴标签
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,  # 点的大小与精度成正比
     xlim = range(x_var),
     ylim = range(c(data_plot_h$yi, pred)),  # 确保y轴包含所有点
     main = "Egger's Test Plot for Homogeneity",
     cex.lab = 1.2,
     cex.axis = 1.1)

# 添加回归线（Egger's test的回归线）
abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)

# 添加95%置信区间带（可选，但原图有）
polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),  # 半透明红色
        border = NA)

# 改为相对位置
x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05

# 添加Egger's test结果的文本注释
text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")

# 添加图例（精度点大小）
legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),  # 点的大小对比
       bty = "n",
       title = "Precision (1/SE)")

#shift----
# 多水平Egger's test
res_egger_shif <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),  # 关键：添加标准误作为预测变量
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_shift_weighted,
                        method = "REML")
summary(res_egger_shif)

#funnel for egger
funnel(res_egger_shif, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Compsition Shift")
#egger_plot
# 1. 提取数据
data_plot_f <- LRR_shift_weighted
se <- sqrt(data_plot_f$vi)  # 标准误
precision <- 1/se  # 精度（1/SE）
x_var <- sqrt(data_plot_f$vi)  # X轴：sqrt(采样方差) = SE

# 2. 提取Egger's test的回归系数
alpha <- res_egger_shif$beta[1, 1]  # 截距
beta <- res_egger_shif$beta[2, 1]  # sqrt(vi)的系数
pval <- res_egger_shif$pval[2]  # p值

# 3. 拟合回归线（用于绘制置信区间）
lm_fit <- lm(data_plot_f$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# 4. 绘制Egger's plot
par(mar = c(5, 5, 4, 2) + 0.1)  # 增加边距

# 创建散点图（点的大小与精度成正比）
plot(x_var, data_plot_f$yi,
     xlab = expression(sqrt("Sampling Variance")),  # X轴标签
     ylab = "lnRR (Effect Size)",  # Y轴标签
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,  # 点的大小与精度成正比
     xlim = range(x_var),
     ylim = range(c(data_plot_f$yi, pred)),  # 确保y轴包含所有点
     main = "Egger's Test Plot for Composition Shift",
     cex.lab = 1.2,
     cex.axis = 1.1)

# 添加回归线（Egger's test的回归线）
abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)

# 添加95%置信区间带（可选，但原图有）
polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),  # 半透明红色
        border = NA)

# 改为相对位置
x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05

# 添加Egger's test结果的文本注释
text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")

# 添加图例（精度点大小）
legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),  # 点的大小对比
       bty = "n",
       title = "Precision (1/SE)")

