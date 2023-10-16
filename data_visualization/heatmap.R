# install packages ------------------------------------------
install.packages("devtools")
install_github("jokergoo/ComplexHeatmap")

# library ---------------------------------------------------
library(devtools)
library(ComplexHeatmap)

# read data. set file path please~ 
data_path <- "/Users/benson/Documents/data/yc/56h.csv"
df = read.csv(data_path, header = T)
df <- df[1:50,-1]
mat <- as.matrix(log2(df))

# set work space
setwd("/Users/benson/Documents/code/code_output")
# create new folder
file_name<- "56h"
dir<-dir.create(file.path(file_name),recursive = TRUE)
# set work space where to save figure
setwd("56h")

set.seed(123)
nr1 = 4; nr2 = 8; nr3 = 6; nr = nr1 + nr2 + nr3
nc1 = 6; nc2 = 8; nc3 = 10; nc = nc1 + nc2 + nc3
mat = cbind(rbind(matrix(rnorm(nr1*nc1, mean = 1,   sd = 0.5), nr = nr1),
                  matrix(rnorm(nr2*nc1, mean = 0,   sd = 0.5), nr = nr2),
                  matrix(rnorm(nr3*nc1, mean = 0,   sd = 0.5), nr = nr3)),
            rbind(matrix(rnorm(nr1*nc2, mean = 0,   sd = 0.5), nr = nr1),
                  matrix(rnorm(nr2*nc2, mean = 1,   sd = 0.5), nr = nr2),
                  matrix(rnorm(nr3*nc2, mean = 0,   sd = 0.5), nr = nr3)),
            rbind(matrix(rnorm(nr1*nc3, mean = 0.5, sd = 0.5), nr = nr1),
                  matrix(rnorm(nr2*nc3, mean = 0.5, sd = 0.5), nr = nr2),
                  matrix(rnorm(nr3*nc3, mean = 1,   sd = 0.5), nr = nr3))
)
mat = mat[sample(nr, nr), sample(nc, nc)] # random shuffle rows and columns
rownames(mat) = paste0("row", seq_len(nr))
colnames(mat) = paste0("column", seq_len(nc))

#change color
library(circlize)
col_fun = colorRamp2(c(-2, 0, 2), c("green", "white", "red"), space = "LAB")
column_name <- expression(control, DMSO)
Heatmap(scale(mat), name = "mat", col = col_fun, 
        column_title = "I am a column title", column_title_side = "bottom", 
        row_title = "I am a row title", row_title_side = "left"
        )



