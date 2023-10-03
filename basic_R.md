# change package path 
```R
.libPaths(c("/Users/benson/Desktop/R_code/r_package", .libPaths()))
```
# Script腳本
表達式Expression:符號結合上下語句，運算並回傳結果  
陳述式Statement:命令執行一系列操作，但不會回傳結果，通常使用控制結構(條件語句/循環語句)來創建

# function函式(輸入與參數/主體/輸出)
function_name(input1, input2, ..., argument1 = ..., argument2 = ...)  
參考:https://yijutseng.github.io/DataScienceRBook/function.html#%E5%87%BD%E6%95%B8%E7%B5%84%E6%88%90
![3053E44D-2486-4F91-844B-6765297ED60C](https://github.com/benson1231/R/assets/144930897/5a053fb0-b946-4c69-84a9-fe0dacb13f77)  

# Repositories套件倉庫
1. CRAN:官方 https://cran.r-project.org/  
2. Bioconductor:生資相關 https://www.bioconductor.org/  
3. GitHub:個人自由存放，無需審查

# Tidyverse
視覺化分析：ggplot2  
執行迴圈：purrr  
增強資料框架：tibble  
資料處理：dplyr  
精簡資料：tidyr  
字串整理：stringr  
資料輸入：readr  
處理類別變數：forcats  