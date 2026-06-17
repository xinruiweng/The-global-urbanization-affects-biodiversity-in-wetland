library(metafor)
library(magick)
library(cowplot)
library(patchwork)
library(ggplot2)
##read data----
LRR_richness_weighted <-read.csv("LRR/LRR_richness_weighted.csv")
LRR_shannon_weighted <-read.csv("LRR/LRR_shannon_weighted.csv")
LRR_homo_weighted <- read.csv("LRR/LRR_homogeneity_weighted.csv")
LRR_shift_weighted <- read.csv("LRR/LRR_shift_weighted.csv")
##Robustness test----
par(cex.lab = 1.5)
##richness----
# Egger's test
res_egger_tax <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_richness_weighted,
                        method = "REML")
summary(res_egger_tax)

#funnel for egger
funnel(res_egger_tax, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Taxonomic Richness")

#egger_plot
# data
data_plot_t <- LRR_taxa_weighted
se <- sqrt(data_plot_t$vi)
precision <- 1/se
x_var <- sqrt(data_plot_t$vi)

# extract
alpha <- res_egger_tax$beta[1, 1]
beta <- res_egger_tax$beta[2, 1]
pval <- res_egger_tax$pval[2]

# lm
lm_fit <- lm(data_plot_t$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# plot
par(mar = c(5, 5, 4, 2) + 0.1)

plot(x_var, data_plot_t$yi,
     xlab = expression(sqrt("Sampling Variance")),
     ylab = "lnRR (Effect Size)",
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,
     xlim = range(x_var),
     ylim = range(c(data_plot_t$yi, pred)),
     main = "Egger's Test Plot for Taxonomic Richness",
     cex.lab = 1.2,
     cex.axis = 1.1)

# line
abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)

polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),
        border = NA)

x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05

text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")

legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),
       bty = "n",
       title = "Precision (1/SE)")

##shannon----
# Egger's test
res_egger_shanno <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_shannon_weighted,
                        method = "REML")
summary(res_egger_shanno)

#funnel for egger
funnel(res_egger_shanno, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Shannon Diversity")
#egger_plot

data_plot_s <- LRR_shannon_weighted
se <- sqrt(data_plot_s$vi)
precision <- 1/se
x_var <- sqrt(data_plot_s$vi)


alpha <- res_egger_shanno$beta[1, 1]
beta <- res_egger_shanno$beta[2, 1]
pval <- res_egger_shanno$pval[2]


lm_fit <- lm(data_plot_s$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# plot
par(mar = c(5, 5, 4, 2) + 0.1)


plot(x_var, data_plot_s$yi,
     xlab = expression(sqrt("Sampling Variance")),
     ylab = "lnRR (Effect Size)",
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,
     xlim = range(x_var),
     ylim = range(c(data_plot_s$yi, pred)),
     main = "Egger's Test Plot for Shannon Diversity",
     cex.lab = 1.2,
     cex.axis = 1.1)


abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)


polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),
        border = NA)


x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05


text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")


legend("top", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),
       bty = "n",
       title = "Precision (1/SE)")

##homo----
# Egger's test
res_egger_hom <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_homo_weighted,
                        method = "REML")
summary(res_egger_hom)

#funnel for egger
funnel(res_egger_hom, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Homogeneity")
#egger_plot

data_plot_h <- LRR_homo_weighted
se <- sqrt(data_plot_h$vi)
precision <- 1/se
x_var <- sqrt(data_plot_h$vi)


alpha <- res_egger_hom$beta[1, 1]
beta <- res_egger_hom$beta[2, 1]
pval <- res_egger_hom$pval[2]


lm_fit <- lm(data_plot_h$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# plot
par(mar = c(5, 5, 4, 2) + 0.1)


plot(x_var, data_plot_h$yi,
     xlab = expression(sqrt("Sampling Variance")),
     ylab = "lnRR (Effect Size)",
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,
     xlim = range(x_var),
     ylim = range(c(data_plot_h$yi, pred)),
     main = "Egger's Test Plot for Homogeneity",
     cex.lab = 1.2,
     cex.axis = 1.1)


abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)


polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),
        border = NA)


x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05


text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")


legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),
       bty = "n",
       title = "Precision (1/SE)")

#shift----
# Egger's test
res_egger_shif <- rma.mv(yi, vi,
                        mods = ~ sqrt(vi),
                        random = ~1 | Study_ID/Plot_ID,
                        data = LRR_shift_weighted,
                        method = "REML")
summary(res_egger_shif)

#funnel for egger
funnel(res_egger_shif, yaxis = "seinv", level = c(90, 95, 99), shade = c("white", "gray55", "gray75"), refline = 0, legend = TRUE,
       main = "Funnel Plot for Compsition Shift")
#egger_plot

data_plot_f <- LRR_shift_weighted
se <- sqrt(data_plot_f$vi)
precision <- 1/se
x_var <- sqrt(data_plot_f$vi)


alpha <- res_egger_shif$beta[1, 1]
beta <- res_egger_shif$beta[2, 1]
pval <- res_egger_shif$pval[2]


lm_fit <- lm(data_plot_f$yi ~ x_var)
pred <- predict(lm_fit, newdata = data.frame(x_var = sort(x_var)), interval = "confidence")

# plot
par(mar = c(5, 5, 4, 2) + 0.1)


plot(x_var, data_plot_f$yi,
     xlab = expression(sqrt("Sampling Variance")),
     ylab = "lnRR (Effect Size)",
     pch = 19, 
     col = "darkblue",
     cex = precision / max(precision) * 3,
     xlim = range(x_var),
     ylim = range(c(data_plot_f$yi, pred)),
     main = "Egger's Test Plot for Composition Shift",
     cex.lab = 1.2,
     cex.axis = 1.1)


abline(a = alpha, b = beta, col = "red", lwd = 2, lty = 2)


polygon(c(sort(x_var), rev(sort(x_var))), 
        c(pred[,2], rev(pred[,3])), 
        col = rgb(1, 0, 0, 0.1),
        border = NA)


x_pos <- par("usr")[1] + diff(par("usr")[1:2]) * 0.05
y_pos <- par("usr")[3] + diff(par("usr")[3:4]) * 0.05


text(x_pos, y_pos, 
     paste0("β = ", round(beta, 3), ", p = ", format.pval(pval, digits = 2)),
     pos = 4, cex = 1.0, col = "red")


legend("topleft", 
       legend = c("High Precision (n large)", "Low Precision (n small)"),
       pch = 19, 
       col = "darkblue",
       pt.cex = c(2, 0.5),
       bty = "n",
       title = "Precision (1/SE)")

##funnel plot comb----
##funnel_comb
img10 <- magick::image_read("plot/funnel_richness.png")
img11 <- magick::image_read("plot/funnel_shannon.png")

img13 <- magick::image_read("plot/funnel_homo.png")
img14 <- magick::image_read("plot/funnel_shift.png")
# combine with cowplot (convert to ggdraw)

p10 <- ggdraw() + draw_image(img10)
p11 <- ggdraw() + draw_image(img11)

p13 <- ggdraw() + draw_image(img13)
p14 <- ggdraw() + draw_image(img14)
combined1 <- wrap_plots( p10, p11, p13, p14, ncol = 1)

combined1 + 
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 17, face = "bold"))

##egger_comb
img101 <- magick::image_read("plot/egger_richness.png")
img111 <- magick::image_read("plot/egger_shannon.png")

img131 <- magick::image_read("plot/egger_homo.png")
img141 <- magick::image_read("plot/egger_shift.png")
# combine with cowplot (convert to ggdraw)

p101 <- ggdraw() + draw_image(img101)
p111 <- ggdraw() + draw_image(img111)

p131 <- ggdraw() + draw_image(img131)
p141 <- ggdraw() + draw_image(img141)
combined2 <- wrap_plots( p101, p111, p131, p141, ncol = 1)

combined2 + 
  plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 17, face = "bold"))
