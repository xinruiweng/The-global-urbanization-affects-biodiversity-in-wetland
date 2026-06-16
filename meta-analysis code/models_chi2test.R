##mixed model and Type two wald chi2 text and robust test
##Robustness test
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
library(puniform)
library(clubSandwich)
##LRR taxa file----
LRR_taxa_weighted <-read.csv("LRR/LRR_taxa_weighted.csv")
##mixed model and test for total----
res_overall_taxa <- rma.mv(yi, vi,
                      mods = ~ taxa_grouped + income_region + wetland_type_grouped + scale_grouped + koppen_climate + reference_type, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_taxa_weighted, 
                      method = "REML")

# 这将输出一个更整洁的系数表格
coef_summary <- coef(summary(res_overall_taxa))
print(coef_summary, digits = 3)

beta <- res_overall_taxa$beta
vb <- res_overall_taxa$vb

# 定义每个变量的位置（查看 res_overall_taxa$beta 的命名）
var_names <- c("taxa_grouped", "income_region", "wetland_type_grouped", "scale_grouped", "koppen_climate","reference_type")

# 循环计算 Wald 检验
for (var in var_names) {
  # 找到变量在系数向量中的位置
  term_pos <- grep(paste0("^", var), rownames(beta))
  
  # 计算 Wald 统计量（Type II：控制其他变量）
  wald_stat <- t(beta[term_pos, , drop = FALSE]) %*% solve(vb[term_pos, term_pos]) %*% beta[term_pos, ]
  p_value <- pchisq(wald_stat, df = length(term_pos), lower.tail = FALSE)
  
  # 输出结果
  cat("\nWald Test for:", var, "\n")
  cat("Chi2 =", wald_stat, "df =", length(term_pos), "p =", p_value, "\n")
}

##LRR shannon file----
LRR_shannon_weighted <-read.csv("LRR/LRR_shannon_weighted.csv")
##mixed model and test for total----
# 构建混合效应模型
res_overall_shannon <- rma.mv(yi, vi, 
                      mods = ~ taxa_grouped + income_region + wetland_type_grouped + scale_grouped + koppen_climate + reference_type, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_shannon_weighted, 
                      method = "REML")  

beta <- res_overall_shannon$beta
vb <- res_overall_shannon$vb

# 定义每个变量的位置（查看 res_overall_shannon$beta 的命名）
var_names <- c("taxa_grouped", "income_region", "wetland_type_grouped", "scale_grouped", "koppen_climate","reference_type")

# 循环计算 Wald 检验
for (var in var_names) {
  # 找到变量在系数向量中的位置
  term_pos <- grep(paste0("^", var), rownames(beta))
  
  # 计算 Wald 统计量（Type II：控制其他变量）
  wald_stat <- t(beta[term_pos, , drop = FALSE]) %*% solve(vb[term_pos, term_pos]) %*% beta[term_pos, ]
  p_value <- pchisq(wald_stat, df = length(term_pos), lower.tail = FALSE)
  
  # 输出结果
  cat("\nWald Test for:", var, "\n")
  cat("Chi2 =", wald_stat, "df =", length(term_pos), "p =", p_value, "\n")
}

##LRR homo file----
LRR_homo_weighted <-read.csv("LRR/LRR_homogeneity_weighted.csv")
##mixed model and test for total----
res_overall_homo <- rma.mv(yi, vi,
                      mods = ~ taxa_grouped + income_region + wetland_type_grouped + scale_grouped + koppen_climate + reference_type, 
                      random = ~ 1 | Study_ID/Plot_ID,
                      data = LRR_homo_weighted, 
                      method = "REML")  

# 这将输出一个更整洁的系数表格
coef_summary <- coef(summary(res_overall_homo))
print(coef_summary, digits = 3)

beta <- res_overall_homo$beta
vb <- res_overall_homo$vb

# 定义每个变量的位置（查看 res_overall_homo$beta 的命名）
var_names <- c("taxa_grouped", "income_region", "wetland_type_grouped", "scale_grouped", "koppen_climate","reference_type")

# 循环计算 Wald 检验
for (var in var_names) {
  # 找到变量在系数向量中的位置
  term_pos <- grep(paste0("^", var), rownames(beta))
  
  # 计算 Wald 统计量（Type II：控制其他变量）
  wald_stat <- t(beta[term_pos, , drop = FALSE]) %*% solve(vb[term_pos, term_pos]) %*% beta[term_pos, ]
  p_value <- pchisq(wald_stat, df = length(term_pos), lower.tail = FALSE)
  
  # 输出结果
  cat("\nWald Test for:", var, "\n")
  cat("Chi2 =", wald_stat, "df =", length(term_pos), "p =", p_value, "\n")
}

##LRR shift file----
LRR_shift_weighted <-read.csv("LRR/LRR_shift_weighted.csv")
##mixed model and test for total----
res_overall_shift <- rma.mv(yi, vi,
                           mods = ~ taxa_grouped + income_region + wetland_type_grouped + scale_grouped + koppen_climate + reference_type, 
                           random = ~ 1 | Study_ID/Plot_ID,
                           data = LRR_shift_weighted, 
                           method = "REML")  

# 这将输出一个更整洁的系数表格
coef_summary <- coef(summary(res_overall_shift))
print(coef_summary, digits = 3)

beta <- res_overall_shift$beta
vb <- res_overall_shift$vb

# 定义每个变量的位置（查看 res_overall_shift$beta 的命名）
var_names <- c("taxa_grouped", "income_region", "wetland_type_grouped", "scale_grouped", "koppen_climate","reference_type")

# 循环计算 Wald 检验
for (var in var_names) {
  # 找到变量在系数向量中的位置
  term_pos <- grep(paste0("^", var), rownames(beta))
  
  # 计算 Wald 统计量（Type II：控制其他变量）
  wald_stat <- t(beta[term_pos, , drop = FALSE]) %*% solve(vb[term_pos, term_pos]) %*% beta[term_pos, ]
  p_value <- pchisq(wald_stat, df = length(term_pos), lower.tail = FALSE)
  
  # 输出结果
  cat("\nWald Test for:", var, "\n")
  cat("Chi2 =", wald_stat, "df =", length(term_pos), "p =", p_value, "\n")
}
