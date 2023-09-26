# 將BRCA_Gene_Expression_profile_FPKM.txt數據資料讀進R
data <- read.table("C:/Users/User/Desktop/ntu/week3/BRCA_Gene_Expression_profile_FPKM.txt", row.names = 1, header = TRUE)     
data$Ensembl_ID <- rownames(data)
data <- data[, c("Ensembl_ID", colnames(data))]

# 1. 读取基因名字映射表
mapping_table <- read.table("C:/Users/User/Desktop/ntu/week3/ID2Name.txt", header = TRUE, stringsAsFactors = FALSE)


# 2. 使用映射表将EnsemblID转换为基因名字并累加表达量
library(dplyr)

converted_data <- data %>%
  left_join(mapping_table, by = c("Ensembl_ID" = "Ensembl_ID")) %>%
  group_by(Ensembl_ID) %>% .[, c("Gene_name", colnames(data))] %>% .[,-2] 
  
new_table <- converted_data %>%
  group_by(Gene_name) %>%
  summarize(across(where(is.numeric), sum))

duplicates <- duplicated(new_table$Gene_name)

# 打印包含重复值的行
if (any(duplicates)) {
  duplicated_rows <- new_table[duplicates, ]
  print(duplicated_rows)
} else {
  cat("No duplicates found in the column.")
}

