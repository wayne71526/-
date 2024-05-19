# 讀取資料
fish = read.table("C:\\Users\\hp\\Desktop\\多變量分析\\SVM\\fish.dat.txt", header=T)

# SVM(預設值：cost=1, kernel="RBF", gamma=1/feature的個數)
library(e1071)
feature = fish[, 2:7]
y = as.factor(fish[, 1])
fish.svm = svm(feature, y)
# 這邊使用的是 one against one
summary(fish.svm)
pred = predict(fish.svm, feature)
table(pred, y)  # 錯誤率：25/148

# svm(gamma=1)
fish.svm1 = svm(feature, y, gamma = 1)
pred1 = predict(fish.svm1, feature)
table(pred1, y) # 錯誤率：17/148

# svm(gamma=5)
fish.svm2 = svm(feature, y, gamma = 5)
pred2 = predict(fish.svm2, feature)
table(pred2, y) # 錯誤率：9/148

# svm(cost=50, gamma=10)
fish.svm3 = svm(feature, y, gamma = 10, cost = 50)
pred3 = predict(fish.svm3, feature)
table(pred3, y) # 錯誤率：0


# 10-fold cv
fish.svm4 = svm(feature, y, cost=0.1, gamma=0.1, cross=10)
summary(fish.svm4)


# grid search：選出較佳的(cost, gamma)=(800, 0.1)
search = tune(svm, factor(Species)~., data=fish,
              ranges = list(cost=100*1:10, gamma=0.1*1:10))
summary(search)
plot(search, xlab = 'cost', ylab = 'gamma')


# svm(cost=800, gamma=0.1)
fish.svm5 = svm(feature, y, gamma = 0.1, cost = 800)
pred5 = predict(fish.svm5, feature)
table(pred5, y) # 錯誤率：1/148
# 預測 test data
test_data = read.table("C:\\Users\\hp\\Desktop\\多變量分析\\SVM\\fish_test.txt", header = T)
test.pred = predict(fish.svm5, test_data)
print(test.pred)
