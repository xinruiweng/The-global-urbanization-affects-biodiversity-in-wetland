library(dplyr)
library(purrr)
library(readr)

input_dir <- "coordinate of each point"
output_dir <- "D"
dir.create(output_dir, showWarnings = FALSE)

calculate_stats <- function(file_path) {
  
  df <- read_csv(file_path, col_types = cols())
  
  # 距离矩阵
  dist_matrix <- dist(df[, c("X", "Y")]) %>% as.matrix()
  
  # 分组索引
  urban_idx <- which(df$Group == "urbanization")
  control_idx <- which(df$Group == "control")
  
  # =========================
  # --- urbanization ---
  # =========================
  urban_dist <- dist_matrix[urban_idx, urban_idx]
  urban_vals <- urban_dist[upper.tri(urban_dist)]
  
  n_urban <- length(urban_vals)
  mean_urban <- ifelse(n_urban > 0, mean(urban_vals), NA)
  sd_urban <- ifelse(n_urban > 1, sd(urban_vals), NA)
  
  # =========================
  # --- control ---
  # =========================
  control_dist <- dist_matrix[control_idx, control_idx]
  control_vals <- control_dist[upper.tri(control_dist)]
  
  n_control <- length(control_vals)
  mean_control <- ifelse(n_control > 0, mean(control_vals), NA)
  sd_control <- ifelse(n_control > 1, sd(control_vals), NA)
  
  # =========================
  # --- between ---
  # =========================
  between_vals <- dist_matrix[control_idx, urban_idx]
  
  n_between <- length(between_vals)
  mean_between <- ifelse(n_between > 0, mean(between_vals), NA)
  sd_between <- ifelse(n_between > 1, sd(between_vals), NA)
  
  # =========================
  # 输出
  # =========================
  data.frame(
    mean_urbanization = mean_urban,
    n_urbanization = n_urban,
    SD_urbanization = sd_urban,
    
    mean_control = mean_control,
    n_control = n_control,
    SD_control = sd_control,
    
    mean_between = mean_between,
    n_between = n_between,
    SD_between = sd_between
  )
}

# 批量处理
file_paths <- list.files(
  path = input_dir,
  pattern = "^\\d+\\.csv$",
  full.names = TRUE
)

walk(file_paths, function(path) {
  
  file_id <- tools::file_path_sans_ext(basename(path))
  
  results <- calculate_stats(path)
  
  output_path <- file.path(output_dir, paste0(file_id, "_d_n_SD.csv"))
  
  write_csv(results, output_path)
  
  message("Processed: ", file_id)
})

message("All files processed!")