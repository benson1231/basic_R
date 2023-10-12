# 1.
nor_data <- read.table("/Users/benson/Desktop/Transcriptome/week5/week03_gene_expr_preprocessing_result.txt",
                   row.names = 1, header = TRUE) 
nonzero_values <- nor_data[nor_data != 0]  # 通过逻辑条件选择非零元素
min_nonzero <- min(nonzero_values)  # 找到非零元素中的最小值
plus_data_df <- nor_data + min_nonzero

# 2.
library("magrittr")
map_table <- read.table("/Users/benson/Desktop/Transcriptome/week5/BRCA_Subtypes.txt",row.names = 1) %>% t() %>% as.data.frame()
trans_df <- rbind(map_table, plus_data_df) 
# 查找包含 "unknown" 的行
v2_row <- trans_df[1,]
v2_row_unknow <- v2_row != "unknow"
rm_unknown <- trans_df[ ,v2_row_unknow]

# 3.
library(dplyr)
calculate_row_means <- function(data) {
  data <- as.data.frame(lapply(data, as.numeric))
  row_means <- rowMeans(data)
  return(row_means)
}

new_v2 <- rm_unknown[1,]
final_df <- rm_unknown[-1,]

basal_mean <- rm_unknown[-1, new_v2 == "Basal"] %>% calculate_row_means()
non_basal_mean <- rm_unknown[-1, new_v2 != "Basal"] %>% calculate_row_means()
final_df$Basal <- (basal_mean / non_basal_mean) %>% log(base = 2)

Her2_mean <- rm_unknown[-1, new_v2 == "Her2"] %>% calculate_row_means()
non_Her2_mean <- rm_unknown[-1, new_v2 != "Her2"] %>% calculate_row_means()
final_df$Her2 <- (Her2_mean / non_Her2_mean) %>% log(base = 2)

LumA_mean <- rm_unknown[-1, new_v2 == "LumA"] %>% calculate_row_means()
non_LumA_mean <- rm_unknown[-1, new_v2 != "LumA"] %>% calculate_row_means()
final_df$LumA <- (LumA_mean / non_LumA_mean) %>% log(base = 2)

LumB_mean <- rm_unknown[-1, new_v2 == "LumB"] %>% calculate_row_means()
non_LumB_mean <- rm_unknown[-1, new_v2 != "LumB"] %>% calculate_row_means()
final_df$LumB <- (LumB_mean / non_LumB_mean) %>% log(base = 2)

result_df <- final_df[,c(1042:1045)]
