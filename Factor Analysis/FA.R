# 讀取資料的 correlation matrix
cor = read.table('C:\\Users\\hp\\Desktop\\多變量分析\\Factor Analysis\\spearman.txt')
cor = as.matrix(cor)

# Factor Analysis with MLE(one factor)
fa.mle = factanal(covmat = cor, factors = 1, n.obs = 33)
fa.mle


# communality
communality = 1 - fa.mle$uniquenesses
communality


# Factor Analysis with MLE(two factor)
fa.mle2 = factanal(covmat = cor, factors = 2, n.obs = 33)
fa.mle2


# communality
communality2 = 1 - fa.mle2$uniquenesses
communality2


# two factor model with a non-orthogonal rotation "promax"
fa.mle2 = factanal(covmat = cor, factors = 2, n.obs = , rotation = 'promax')
fa.mle2


# communality
communality2 = 1 - fa.mle2$uniquenesses
communality2
