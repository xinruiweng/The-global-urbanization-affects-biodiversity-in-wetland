
library(dplyr)
library(readr)

# 设置文件夹路径（替换为实际路径）
folder_path <- "D"

# 获取所有CSV文件列表
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# 批量读取并合并文件
combined_data <- lapply(file_list, function(file) {
  # 读取单个文件
  data <- read_csv(file, col_types = cols())
  
  # 提取文件名（不带扩展名）
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # 添加文件名列
  data %>% 
    mutate(file_id = file_name, .before = 1)  # 文件名作为第一列
}) %>% 
  bind_rows()  # 合并所有数据框

# 查看合并结果
head(combined_data)

# 保存为新的CSV文件
write_csv(combined_data, "comb_d_n_sd.csv")