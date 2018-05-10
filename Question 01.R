# Question 01
library(readxl)
European_Jobs <- read_excel("European_Jobs.xlsx")
head(European_Jobs)

# 主成分分析
pca <- prcomp(formula = ~Agr+Min+Man+PS+Con+SI+Fin+SPS+TC,# 選擇變數
              data = European_Jobs,                       # 資料
              scale = TRUE)                               # 標準化
pca
summary(pca)

# 使用plot()函式
plot(pca,                        # 放pca
     type="line",                # 用直線連結每個點
     main="Scree Plot for Jobs") # 主標題

# 用藍線標示出特徵值=1的地方
abline(h=1, col="blue") # Kaiser eigenvalue-greater-than-one rule

vars <- (pca$sdev)^2  # 從pca中取出標準差(pca$sdev)後再平方，計算variance(特徵值)
vars

# 正規化後(變異個數=特徵值總和)
sum(pca$sdev^2)

# 計算每個主成分的解釋比例 = 各個主成分的特徵值/總特徵值
props <- vars / sum(vars)    
props

# 累加每個主成份的解釋比例(aggregated effects)
cumulative.props <- cumsum(props)  # 累加前n個元素的值
cumulative.props

# 累積解釋比例圖
plot(cumulative.props)

# pca$rotation 選取三個主成分(>70%解釋變異)
top3_pca.data <- pca$x[, 1:3]
top3_pca.data

# 特徵向量(原變數的線性組合)
pca$rotation

# 取前三個主成份的特徵向量：
top3.pca.eigenvector <- pca$rotation[, 1:3]
top3.pca.eigenvector

# 排序
first.pca <- top3.pca.eigenvector[, 1]   #  第一主成份
second.pca <- top3.pca.eigenvector[, 2]  #  第二主成份
third.pca <- top3.pca.eigenvector[, 3]   #  第三主成份

# 第一主成份：由小到大排序原變數的係數
first.pca[order(first.pca, decreasing=FALSE)] 

# 使用dotchart，繪製主成份負荷圖
dotchart(first.pca[order(first.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC1",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="red")                                        # 顏色

# 第二主成份：由小到大排序原變數的係數
second.pca[order(second.pca, decreasing=FALSE)]

# 使用dotchart，繪製主成份負荷圖
dotchart(second.pca[order(second.pca, decreasing=FALSE)] ,  # 排序後的係數
         main="Loading Plot for PC2",                       # 主標題
         xlab="Variable Loadings",                          # x軸的標題
         col="blue")                                        # 顏色

# 第三主成份：由小到大排序原變數的係數
third.pca[order(third.pca, decreasing=FALSE)] 

# 使用dotchart，繪製主成份負荷圖
dotchart(third.pca[order(third.pca, decreasing=FALSE)] ,   # 排序後的係數
         main="Loading Plot for PC3",                      # 主標題
         xlab="Variable Loadings",                         # x軸的標題
         col="purple")                                     # 顏色

# 選取 PC1 和 PC2 繪製主成份負荷圖
biplot(pca, choices=1:2)
