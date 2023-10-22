# 0. 第五週先匯入 -----------------------------------------------------------------
library(magrittr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
nor_data <- read.table("/Users/benson/Desktop/Transcriptome/week5/week03_gene_expr_preprocessing_result.txt",
                       row.names = 1, header = TRUE)  # 讀檔
nonzero_values <- nor_data[nor_data != 0]  # 透過邏輯篩選非零值
min_nonzero <- min(nonzero_values)  # 找到非零值中的最小值
plus_data_df <- nor_data + min_nonzero  # 全部數值加上矩陣中非0的最小值
map_table <- read.table("/Users/benson/Desktop/Transcriptome/week5/BRCA_Subtypes.txt",row.names = 1) %>% t() %>% as.data.frame()
trans_df <- rbind(map_table, plus_data_df) # 使用rbind將map_table合併到plus_data_df，但要注意排序
v2_row <- trans_df[1,]
v2_row_unknow <- v2_row != "unknow"   # 創建邏輯，去篩選是否為unknow
rm_unknown <- trans_df[ ,v2_row_unknow]  # 選擇非unknow樣本，即為去除unknow樣本
calculate_row_means <- function(data) {
  data <- as.data.frame(lapply(data, as.numeric))
  row_means <- rowMeans(data)
  return(row_means)
}
new_v2 <- rm_unknown[1,]  # 創建紀錄v2的list
final_df <- rm_unknown[-1,]  # 扣除v2 row
final_df$gene_name <- row.names(final_df)
subtypes <- c("Basal", "Her2", "LumA", "LumB")
for(i in subtypes){
  mean <- rm_unknown[-1, new_v2 == i] %>% calculate_row_means()
  non_mean <- rm_unknown[-1, new_v2 != i] %>% calculate_row_means()
  final_df[[i]] <- log(mean / non_mean, base = 2)
}
result <- final_df[,c(1042:1046)] 
rownames(result) <- 1:nrow(result)
# 獲得log2FC資訊 
Basal_logFC <- result[,c(1,2)] 
Her2_logFC <- result[,c(1,3)] 
LumA_logFC <- result[,c(1,4)] 
LumB_logFC <- result[,c(1,5)] 
# 1. 第六週 畫出box-plot --------------------------------------------------------
unnor_data <- read.table("/Users/benson/Desktop/Transcriptome/week6/unnor_data.txt",
                        row.names = 1,header = TRUE)
nor_data <- read.table("/Users/benson/Desktop/Transcriptome/week6/week03_gene_expr_preprocessing_result.txt",
                       header = TRUE)
week6_sample <- read.table("/Users/benson/Desktop/Transcriptome/week6/week06_Samples.txt",
                           header = FALSE)  %>% t() %>% as.character()
unnor <- unnor_data[, week6_sample]
nor <- nor_data[, week6_sample]
unnor$gene_name <- row.names(unnor)
nor$gene_name <- row.names(nor)
# 用melt將資料重塑
unnor_melt <- melt(unnor,id=c("gene_name"),measure=week6_sample)
nor_melt <-  melt(nor,id=c("gene_name"),measure=week6_sample)
# 尚未標準化的資料進行box plot做圖
jpeg(filename = "/Users/benson/Desktop/Transcriptome/week6/unnormal.jpg", width = 500, height = 400, units = "px", res = 150)
p <- ggplot(unnor_melt, aes(x = variable, y = value, fill = variable, color = variable))
p + geom_boxplot(show.legend = FALSE, outlier.shape = NA, color = "black") +  # 隱藏outlier/legend並更改顏色
  coord_cartesian(ylim = c(0, 30)) + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())   # 不顯示sample名稱
dev.off()
# 標準化的資料進行box plot做圖
jpeg(filename = "/Users/benson/Desktop/Transcriptome/week6/normal.jpg", width = 500, height = 400, units = "px", res = 150)
p <- ggplot(nor_melt, aes(x = variable, y = value, fill = variable, color = variable))
p + geom_boxplot(show.legend = FALSE, outlier.shape = NA, color = "gray") +  # 隱藏outlier/legend並更改顏色
  coord_cartesian(ylim = c(0, 30)) +    # 調整y軸至可看到完整box
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())
dev.off()

# 2. 匯入資料並篩選平均表現量top 300基因 ---------------------------------------
Basal_top300 <- read.table("/Users/benson/Desktop/Transcriptome/week5/Basal_logFC.txt",
                         header = TRUE) %>% arrange() %>% .[1:300,1] %>% as.character()
Her2_top300 <- read.table("/Users/benson/Desktop/Transcriptome/week5/Her2_logFC.txt",
                         header = TRUE) %>% arrange() %>% .[1:300,1] %>% as.character()
LumA_top300 <- read.table("/Users/benson/Desktop/Transcriptome/week5/LumA_logFC.txt",
                         header = TRUE) %>% arrange() %>% .[1:300,1] %>% as.character()
LumB_top300 <- read.table("/Users/benson/Desktop/Transcriptome/week5/LumB_logFC.txt",
                         header = TRUE) %>% arrange() %>% .[1:300,1] %>% as.character()

# 透過邏輯篩選subtype以及非該subtype的資料將其分開
v2_row_new <- rm_unknown[1,]
Basal_sample <- rm_unknown[-1, v2_row_new == "Basal"]
non_Basal_sample <- rm_unknown[-1, v2_row_new != "Basal"]
Her2_sample <- rm_unknown[-1, v2_row_new == "Her2"]
non_Her2_sample <- rm_unknown[-1, v2_row_new != "Her2"]
LumA_sample <- rm_unknown[-1, v2_row_new == "LumA"]
non_LumA_sample <- rm_unknown[-1, v2_row_new != "LumA"]
LumB_sample <- rm_unknown[-1, v2_row_new == "LumB"]
non_LumB_sample <- rm_unknown[-1, v2_row_new != "LumB"]
# Basal樣本的heatmap
Basal <- Basal_sample[Basal_top300,] 
basal_row <- row.names(Basal)
new_basal <- as.data.frame(lapply(Basal, as.numeric))
row.names(new_basal) <- basal_row 
new_basal <- t(new_basal)
p1 <- pheatmap::pheatmap(new_basal,scale = "row")
ggsave("Basal_Heatmap.png", plot = p1,  device = "png",  units = "px", dpi = 150)
# Her2樣本的heatmap
Her2 <- Her2_sample[Her2_top300,] 
Her2_row <- row.names(Her2)
new_Her2 <- as.data.frame(lapply(Her2, as.numeric))
row.names(new_Her2) <- Her2_row 
new_Her2 <- t(new_Her2)
p2 <- pheatmap::pheatmap(new_Her2,scale = "row")
ggsave("Her2_Heatmap.png", plot = p2, device = "png", units = "px", dpi = 150)
# LumA樣本的heatmap
LumA <- LumA_sample[LumA_top300,] 
LumA_row <- row.names(LumA)
new_LumA <- as.data.frame(lapply(LumA, as.numeric))
row.names(new_LumA) <- LumA_row 
new_LumA <- t(new_LumA)
p3 <- pheatmap::pheatmap(new_LumA,scale = "row")
ggsave("LumA_Heatmap.png", plot = p3, device = "png", units = "px", dpi = 150)
# LumB樣本的heatmap
LumB <- LumB_sample[LumB_top300,] 
LumB_row <- row.names(LumB)
new_LumB <- as.data.frame(lapply(LumB, as.numeric))
row.names(new_LumB) <- LumB_row 
new_LumB <- t(new_LumB)
p4 <- pheatmap::pheatmap(new_LumB,scale = "row")
ggsave("LumB_Heatmap.png", plot = p4, device = "png", units = "px", dpi = 150)

# 3.1 Basal t test -------------------------------------------------------------
Basal_row_all <- row.names(Basal_sample)
Basal_all <- as.data.frame(lapply(Basal_sample, as.numeric))
row.names(Basal_all) <- Basal_row_all
non_Basal_row <- row.names(non_Basal_sample)
non_Basal <- as.data.frame(lapply(non_Basal_sample, as.numeric))
row.names(non_Basal) <- non_Basal_row
t_test_results <- data.frame(row_number = 1:nrow(Basal_all))
# Basal與非Basal兩個data frame的row互相進行 t test(個別基因表現量的t test)
for (i in 1:nrow(Basal_all)) {
  t_test_result <- t.test(Basal_all[i,], non_Basal[i,])  
  Basal_logFC[i, "p_value"] <- t_test_result$p.value
}
png("Basal_volcano.png") 
Basal_logFC %$% plot(Basal, -log10(p_value), pch = 16, col = "grey50", las = 1, main = "Basal",
              xlab = expression(paste(log[2], "FC", sep = "")),
              ylab = expression(paste(-log[10], "p", sep = "")))  # Basal column為我的log2FC
Basal_logFC %>% dplyr::filter(p_value < 0.05, Basal > 1) %$%
  points(Basal, -log10(p_value), pch = 16, col = "red") # up-DEGs
Basal_logFC %>% dplyr::filter(p_value < 0.05, Basal < -1) %$%
  points(Basal, -log10(p_value), pch = 16, col = "blue") # down-DEGs
dev.off()

# 3.2 Her2  t test -------------------------------------------------------------
Her2_row_all <- row.names(Her2_sample)
Her2_all <- as.data.frame(lapply(Her2_sample, as.numeric))
row.names(Her2_all) <- Her2_row_all
non_Her2_row <- row.names(non_Her2_sample)
non_Her2 <- as.data.frame(lapply(non_Her2_sample, as.numeric))
row.names(non_Her2) <- non_Her2_row
t_test_results <- data.frame(row_number = 1:nrow(Her2_all))
# Her2與非Her2兩個data frame的row互相進行 t test(個別基因表現量的t test)
for (i in 1:nrow(Her2_all)) {
  t_test_result <- t.test(Her2_all[i,], non_Her2[i,])
  Her2_logFC[i, "p_value"] <- t_test_result$p.value
}
png("Her2_volcano.png")
Her2_logFC %$% plot(Her2, -log10(p_value), pch = 16, col = "grey50", las = 1, main = "Her2",
                     xlab = expression(paste(log[2], "FC", sep = "")),
                     ylab = expression(paste(-log[10], "p", sep = "")))
Her2_logFC %>% dplyr::filter(p_value < 0.05, Her2 > 1) %$%
  points(Her2, -log10(p_value), pch = 16, col = "red")  # up-DEGs
Her2_logFC %>% dplyr::filter(p_value < 0.05, Her2 < -1) %$%
  points(Her2, -log10(p_value), pch = 16, col = "blue")  # down-DEGs
dev.off()

# 3.3 LumA  t test -------------------------------------------------------------
LumA_row_all <- row.names(LumA_sample)
LumA_all <- as.data.frame(lapply(LumA_sample, as.numeric))
row.names(LumA_all) <- LumA_row_all
non_LumA_row <- row.names(non_LumA_sample)
non_LumA <- as.data.frame(lapply(non_LumA_sample, as.numeric))
row.names(non_LumA) <- non_LumA_row
t_test_results <- data.frame(row_number = 1:nrow(LumA_all))
# LumA與非LumA兩個data frame的row互相進行 t test(個別基因表現量的t test)
for (i in 1:nrow(LumA_all)) {
  t_test_result <- t.test(LumA_all[i,], non_LumA[i,])
  LumA_logFC[i, "p_value"] <- t_test_result$p.value
}
png("LumA_volcano.png")
LumA_logFC %$% plot(LumA, -log10(p_value), pch = 16, col = "grey50", las = 1, main = "LumA",
                    xlab = expression(paste(log[2], "FC", sep = "")),
                    ylab = expression(paste(-log[10], "p", sep = "")))
LumA_logFC %>% dplyr::filter(p_value < 0.05, LumA > 1) %$%
  points(LumA, -log10(p_value), pch = 16, col = "red")
LumA_logFC %>% dplyr::filter(p_value < 0.05, LumA < -1) %$%
  points(LumA, -log10(p_value), pch = 16, col = "blue")
dev.off()

# 3.4 LumB  t test -------------------------------------------------------------
LumB_row_all <- row.names(LumB_sample)
LumB_all <- as.data.frame(lapply(LumB_sample, as.numeric))
row.names(LumB_all) <- LumB_row_all
non_LumB_row <- row.names(non_LumB_sample)
non_LumB <- as.data.frame(lapply(non_LumB_sample, as.numeric))
row.names(non_LumB) <- non_LumB_row
t_test_results <- data.frame(row_number = 1:nrow(LumB_all))
# LumB與非LumB兩個data frame的row互相進行 t test(個別基因表現量的t test)
for (i in 1:nrow(LumB_all)) {
  t_test_result <- t.test(LumB_all[i,], non_LumB[i,])
  LumB_logFC[i, "p_value"] <- t_test_result$p.value
}
png("LumB_volcano.png")
LumB_logFC %$% plot(LumB, -log10(p_value), pch = 16, col = "grey50", las = 1, main = "LumB",
                    xlab = expression(paste(log[2], "FC", sep = "")),
                    ylab = expression(paste(-log[10], "p", sep = "")))
LumB_logFC %>% dplyr::filter(p_value < 0.05, LumB > 1) %$%
  points(LumB, -log10(p_value), pch = 16, col = "red")
LumB_logFC %>% dplyr::filter(p_value < 0.05, LumB < -1) %$%
  points(LumB, -log10(p_value), pch = 16, col = "blue")
dev.off()
