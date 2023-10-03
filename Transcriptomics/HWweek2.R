# Q1
tmp <- cbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12))
colnames(tmp) <- c("Col1", "Col2", "Col3")
rownames(tmp) <- c("Row1", "Row2", "Row3", "Row4")
tmp

# Q2
library(dplyr)
B1 <- read.table("C:/Users/User/Desktop/ntu/week2/B1.ReadsPerGene.tsv", sep = "\t", header=TRUE)
B1 <- rename(B1, GeneID = "V1", B1="V2")
B2 <- read.table("C:/Users/User/Desktop/ntu/week2/B2.ReadsPerGene.tsv", sep = "\t", header=TRUE)
B2 <- rename(B2, GeneID = "V1", B2="V2")
B3 <- read.table("C:/Users/User/Desktop/ntu/week2/B3.ReadsPerGene.tsv", sep = "\t", header=TRUE)
B3 <- rename(B3, GeneID = "V1", B3="V2")
B4 <- read.table("C:/Users/User/Desktop/ntu/week2/B4.ReadsPerGene.tsv", sep = "\t", header=TRUE)
B4 <- rename(B4, GeneID = "V1", B4="V2")
B5 <- read.table("C:/Users/User/Desktop/ntu/week2/B5.ReadsPerGene.tsv", sep = "\t", header=TRUE)
B5 <- rename(B5, GeneID = "V1", B5="V2")
C1 <- read.table("C:/Users/User/Desktop/ntu/week2/C1.ReadsPerGene.tsv", sep = "\t", header=TRUE)
C1 <- rename(C1, GeneID = "V1", C1="V2")
C2 <- read.table("C:/Users/User/Desktop/ntu/week2/C2.ReadsPerGene.tsv", sep = "\t", header=TRUE)
C2 <- rename(C2, GeneID = "V1", C2="V2")
C3 <- read.table("C:/Users/User/Desktop/ntu/week2/C3.ReadsPerGene.tsv", sep = "\t", header=TRUE)
C3 <- rename(C3, GeneID = "V1", C3="V2")
C4 <- read.table("C:/Users/User/Desktop/ntu/week2/C4.ReadsPerGene.tsv", sep = "\t", header=TRUE)
C4 <- rename(C4, GeneID = "V1", C4="V2")
C5 <- read.table("C:/Users/User/Desktop/ntu/week2/C5.ReadsPerGene.tsv", sep = "\t", header=TRUE)
C5 <- rename(C5, GeneID = "V1", C5="V2")

# Q3
library(magrittr)
combined_df <- bind_cols(B1, B2, B3, B4, B5, C1, C2, C3, C4, C5)
cor(B1,B2,method = "spearman")
combined_df %$% cor(B2, B1, method = "spearman")
combined_df %$% cor(C1, B1, method = "spearman")
combined_df %$% cor(C1, B2, method = "spearman")
combined_df %$% cor(C2, B1, method = "spearman")
combined_df %$% cor(C2, B2, method = "spearman")
combined_df %$% cor(C2, C1, method = "spearman")

# Q4 
input_data <- merge(B1, B2, by="GeneID")
input_data <- merge(input_data, B3, by="GeneID")
input_data <- merge(input_data, B4, by="GeneID")
input_data <- merge(input_data, B5, by="GeneID")
input_data <- merge(input_data, C1, by="GeneID")
input_data <- merge(input_data, C2, by="GeneID")
input_data <- merge(input_data, C3, by="GeneID")
input_data <- merge(input_data, C4, by="GeneID")
input_data <- merge(input_data, C5, by="GeneID")

# Q5
as.data.frame(input_data)
class(input_data)

# Q6
library(magrittr)
library(dplyr)
data_dir <- "C:/Users/User/Desktop/ntu/week2"
data <- c("B1", "B2", "B3", "B4", "B5", "C1", "C2", "C3", "C4", "C5")
result_list <- list()  # 創建一個空的列表來存儲結果
for (i in data) {
  b_data <- paste0(data_dir, "/", i, ".ReadsPerGene.tsv") %>%
  read.table(., sep = "\t", header = TRUE) %>%
  rename(GeneID = "V1", !!i := "V2")  # 使用 !!i 來動態設定列名稱
  
  result_list[[i]] <- b_data
}
# result_list 中包含了 B1 到 C5 的data frame，每個資料框使用相應的名稱作為列名稱

