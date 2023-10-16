# 1.missing-data imputation
nor_data <- read.table("/Users/benson/Desktop/Transcriptome/week5/week03_gene_expr_preprocessing_result.txt",
                   row.names = 1, header = TRUE)  # 讀檔
nonzero_values <- nor_data[nor_data != 0]  # 透過邏輯篩選非零值
min_nonzero <- min(nonzero_values)  # 找到非零值中的最小值
plus_data_df <- nor_data + min_nonzero  # 全部數值加上矩陣中非0的最小值

# 2.去除unknow樣本
library("magrittr")
map_table <- read.table("/Users/benson/Desktop/Transcriptome/week5/BRCA_Subtypes.txt",row.names = 1) %>% t() %>% as.data.frame()
trans_df <- rbind(map_table, plus_data_df) # 使用rbind將map_table合併到plus_data_df，但要注意排序
# 查找包含 "unknown" 的行
v2_row <- trans_df[1,]
v2_row_unknow <- v2_row != "unknow"   # 創建邏輯，去篩選是否為unknow
rm_unknown <- trans_df[ ,v2_row_unknow]  # 選擇非unknow樣本，即為去除unknow樣本

# 3.逐一計算subtype的logFC
library(dplyr)
# 創建一個函數回傳列平均值
calculate_row_means <- function(data) {
  data <- as.data.frame(lapply(data, as.numeric))
  row_means <- rowMeans(data)
  return(row_means)
}
# 透過v2這個row實行邏輯篩選
new_v2 <- rm_unknown[1,]  # 創建紀錄v2的list
final_df <- rm_unknown[-1,]  # 扣除v2 row
final_df$gene_name <- row.names(final_df)
# 用迴圈輸出並新增到final_df的最後面
subtypes <- c("Basal", "Her2", "LumA", "LumB")
for(i in subtypes){
  mean <- rm_unknown[-1, new_v2 == i] %>% calculate_row_means()
  non_mean <- rm_unknown[-1, new_v2 != i] %>% calculate_row_means()
  final_df[[i]] <- log(mean / non_mean, base = 2)
}
result <- final_df[,c(1042:1046)] 
rownames(result) <- 1:nrow(result)
# 輸出成txt檔
Basal_logFC <- result[,c(1,2)] %>% arrange(desc(Basal)) %>% 
  write.table("Basal_logFC.txt", row.names = F, quote = F, qmetho ="double")
Her2_logFC <- result[,c(1,3)] %>% arrange(desc(Her2)) %>% 
  write.table("Her2_logFC.txt", row.names = F, quote = F, qmetho ="double")
LumA_logFC <- result[,c(1,4)] %>% arrange(desc(LumA)) %>% 
  write.table("LumA_logFC.txt", row.names = F, quote = F, qmetho ="double")
LumB_logFC <- result[,c(1,5)] %>% arrange(desc(LumB)) %>% 
  write.table("LumB_logFC.txt", row.names = F, quote = F, qmetho ="double")
