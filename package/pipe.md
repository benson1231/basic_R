# 參考 https://blog.gtwang.org/r/r-pipes-magrittr-package/
# 參考 https://bookdown.org/tonykuoyj/eloquentr/data-workflow.html#forward-pipe
# 參考 https://bookdown.org/b08302310/R_learning_notes/%E4%BB%A5-magrittr-%E4%BD%BF%E7%94%A8-pipes.html
```R
install.packages("magrittr")
library(magrittr)
```
# %>% 管線運算子將左邊結果丟到後面的函數，用.可指定位置
```R
iris %>%
  lm(Sepal.Length ~ Sepal.Width, data = .) %>%
  summary
```
# %$% 運算子將其左方的物件套用至右方函數的 data 參數中
```R
iris %$%
  lm(Sepal.Length ~ Sepal.Width) %>%
  summary

mtcars %$% cor(disp, mpg)
```
# %<>% 運算子具有 %>% 管線運算子的串接功能之外，還可以將其右方的運算結果儲存至左方的變數中
# 假設我們想要讓一個變數進行一連串的運算後，再將結果儲存回原來的變數中，典型的寫法大概是像這樣：
```R
set.seed(3)
x <- rnorm(5)
x <- x %>% abs %>% sqrt
```
# 這種情況我們就可以用 %<>% 運算子將上面的程式碼改寫為：
```R
set.seed(3)
x <- rnorm(5)
x %<>% abs %>% sqrt
```
# %T>% tee 運算子的功能也是類似 %>% 管線運算子，只不過在運算完成後，它會傳回原本其左方的輸入值，而不是最終右方運算式計算完的結果
#假設我們產生了一個含有兩個行（column）的矩陣，想要將資料畫出來，並且同時也計算出兩行數值的總和，一般普通的寫法會像這樣：
```R
set.seed(3)
x <- matrix(rnorm(200), ncol = 2)
plot(x)
colSums(x)
```
#這裡的資料流結構跟前面的狀況不太一樣，第二行所建立的 x 同時傳給第三行與第四行來使用，這種將資料一分為二的情況就要使用 %T>% tee 運算子來處理，經過 %T>% 改寫後就會變成：

set.seed(3)
rnorm(200) %>%
  matrix(ncol = 2) %T>%
  plot %>%
  colSums
# 這裡使用 %T>% 的效果就是會讓 matrix 的計算結果同時傳給 plot 與 colSums 兩個數，達到預期的計算結果。
