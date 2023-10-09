### 0.將BRCA_Gene_Expression_profile_FPKM.txt數據資料讀進R----------------------
# 檔案存在/Users/benson/Desktop/Transcriptome/week3/
data <- read.table("/Users/benson/Desktop/Transcriptome/week3/BRCA_Gene_Expression_profile_FPKM.txt",
                   row.names = 1, header = TRUE) 
dim(data) # 有10273個基因與1091個樣本
# 額外再新增一個Ensembl ID 的column存放Ensembl ID
data <- cbind(Ensembl_ID = rownames(data), data)

### 1.讀取基因名稱對照表 ID2Name.txt -------------------------------------------
mapping_table <- read.table("/Users/benson/Desktop/Transcriptome/week3/ID2Name.txt", 
                            header = TRUE, stringsAsFactors = FALSE)
# 使用對照表將Ensembl ID轉為基因名稱
# 將ID2Name.txt與BRCA_Gene_Expression_profile_FPKM.txt資料以Ensembl_ID去mapping合併
library(dplyr)
converted_data <- data %>%
  left_join(mapping_table, by = c("Ensembl_ID" = "Ensembl_ID")) %>% 
  .[,-1]   # 去除Ensembl ID

# 驗證converted_data中是否有重複的Gene_name
duplicates <- duplicated(converted_data$Gene_name)
if (any(duplicates)) {
  duplicated_rows <- converted_data[duplicates, ]
  print(duplicated_rows)
} else {
  cat("No duplicates found in the column.")
}
# converted_data依照Gene_name分組，將相同Gene_name的數值相加成為新的new_table
new_table <- converted_data %>%
  group_by(Gene_name) %>%
  summarize(across(where(is.numeric), sum))
# 驗證new_table中是否有重複Gene_name
duplicates <- duplicated(new_table$Gene_name)
if (any(duplicates)) {
  duplicated_rows <- new_table[duplicates, ]
  print(duplicated_rows)
} else {
  cat("No duplicates found in the column.")
}
dim(new_table) # 有10035個基因與1091(1092-1，最後一行為gene_name)個樣本

### 2.創造一個check_average_expression function可以數值是否大於一 --------------
check_average_expression <- function(x) {
  return(x > 1)
}
# 計算平均表現量作為average_column
new_table$average_column <- rowMeans(new_table[, -1])
filtered_genes <- sapply(new_table$average_column, check_average_expression)
print(filtered_genes)
more_than_one_data <- new_table[filtered_genes, -length(new_table)]. # 最後一行是平均，不需要了
dim(more_than_one_data) # 有3560個基因與1091(1092-1，第一行為gene_name)個樣本

### 3.Normalization ------------------------------------------------------------
# 前處理
more_than_one_data_df <- as.data.frame(more_than_one_data)
row.names(more_than_one_data_df) <- more_than_one_data$Gene_name
pre_nor_data <- more_than_one_data_df[,-1]
# 計算每個樣本第75百分位數
q3_values <- sapply(pre_nor_data, quantile, probs = 0.75) 
# 計算每個樣本第75百分位數的中位數
medianQ3 <- median(q3_values)
# 計算差異倍數
Xsample <- medianQ3 / q3_values
# upper quartile normalization
nor_data <- pre_nor_data %>%
  mutate(across(everything(), ~ . * Xsample[match(cur_column(), colnames(pre_nor_data))]))






