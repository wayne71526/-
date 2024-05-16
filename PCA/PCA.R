# 讀取資料
crime = read.table('C:\\Users\\hp\\Desktop\\多變量分析練習\\PCA\\citycrime.txt', header = T)

# scatter plot matrix
pairs(crime)

# correlation matrix
cor(crime)

library(stats)
# 使用 data 的 correlation matrix 做 PCA
pca.crime = princomp(crime, cor=T)

# 查看結果
summary(pca.crime)

# 計算 loadings，it defines loadings without multiplying by the sqrt of lambda
# 各變數的係數
loadings(pca.crime)

# 計算 PC scores
pcs.crime = predict(pca.crime)

# 畫 screeplot
library('ggplot2')
eigen = eigen(cor(crime))$values
barplot(eigen, col='green', main='scree plot', xlab='Component', space = 0.5,
        names.arg = c('comp.1', 'comp.2', 'comp.3', 'comp.4', 'comp.5',
                     'comp.6', 'comp.7'), axes = T)

# Plot the first 2 PCs
plot(pcs.crime[,1:2], type = 'p', xlab = '1st', ylab = '2nd', cex=0.6,
     col='red', pch=8)
text(pcs.crime[,1:2], row.names(crime), cex = 0.7)

# The variables are scaled by lambda ^ scale and the observations are scaled by lambda ^ (1-scale)
biplot(pca.crime, scale=1, cex=0.7)
