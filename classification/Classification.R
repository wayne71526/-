fish = read.table("C:\\Users\\hp\\Desktop\\多變量分析\\classification\\fish.dat.txt", 
                  header=T)

## decision tree(可以做variable selection)
library(rpart)
# minsplit：node中至少要有幾個資料點才能做分割
# minbucket：分割後node中必須至少有幾個點
# xval：number of cross-validations.
# rpart.control：用來控制tree中的各種參數
fish.control = rpart.control(minsplit = 10, minbucket = 3, xval=0)
fish.tree = rpart(Species~., data = fish, control = fish.control,
                  method = 'class')
# desicion tree的圖
library(rpart.plot)
rpart.plot(fish.tree)
printcp(fish.tree)

# prune tree
fish_prunetree = prune.rpart(fish.tree, cp=0.02)
plot(fish_prunetree)
text(fish_prunetree)
rpart.plot(fish_prunetree)
printcp(fish_prunetree)

# add new variable
L21 = fish$L2 - fish$L1
L31 = fish$L3 - fish$L1
L32 = fish$L3 - fish$L2
newfish = cbind(fish, L21, L31, L32)
cor(newfish[, 2:ncol(newfish)])
newfish_tree = rpart(Species~., data=newfish, 
                     control = fish.control, parms = list(split='information'),
                     method='class')
printcp(newfish_tree)
rpart.plot(newfish_tree)

newfish_control = rpart.control(minsplit = 10, minbucket = 3, xval=5)
newfish_tree2 = rpart(Species~., data=newfish, control = newfish_control,
                      method = 'class', parms = list(split='information'))
printcp(newfish_tree2)
plotcp(newfish_tree2)

# 做預測
fish_test = read.table("C:\\Users\\hp\\Desktop\\多變量分析\\classification\\fish_test.txt",
                       header = T)
fish_test$L21 = fish_test$L2 - fish_test$L1
fish_test$L31 = fish_test$L3 - fish_test$L1
fish_test$L32 = fish_test$L3 - fish_test$L2
predict(newfish_tree2, fish_test)


## LDA
library(MASS)
newfish.lda = lda(Species~., data=newfish) # 有collinear的情形發生
newfish.lda = lda(Species~Weight+L1+Height+Width+L21+L32, data=newfish)
newfish.lda

# 預測
newfish.lda.pred = predict(newfish.lda, newfish[,-1])
table(newfish$Species, newfish.lda.pred$class)

# 10 fold cv
library(caret)
trControl = trainControl(method = 'cv', number = 10)
fit_lda = train(Species~Weight+L1+Height+Width+L21+L32, data=newfish,
                method='lda', metric='Accuracy', trControl=trControl)
fit_lda

# 畫出object在前2個LDA的分布圖
eqscplot(newfish.lda.pred$x[, 1], newfish.lda.pred$x[, 2], xlab='LD1',
         ylab='LD2', type='n')
fish.colors = c(rep(1,33),rep(2,5),rep(3,18),rep(4,10),rep(5,12),rep(6,16),rep(7,54))
text(newfish.lda.pred$x[, 1], newfish.lda.pred$x[, 2], fish$Species,
     col=fish.colors)


## QDA
library(MASS)
fish.qda = qda(Species~., data=newfish) #類別white的個數太少
# 將類別為 white 的資料刪除
newfish_q = read.table("C:\\Users\\hp\\Desktop\\多變量分析\\classification\\new_fish.qdat.txt",
                         header = T)

newfish_qda = qda(Species~., data = newfish_q)
# 將有共線性的變數拿掉
newfish_qda = qda(Species~Weight+L1+Height+Width+L21+L32, data = newfish_q)
newfish_qda.pred = predict(newfish_qda, newfish_q)
table(newfish_q$Species, newfish_qda.pred$class)

# 對 test data 做預測
newfish_qda.test = predict(newfish_qda, fish_test)
newfish_qda.test$class

# 做 10 fold cv
trcontrol = trainControl(method = 'cv', number = 10)
fit_qda = train(Species~Weight+L1+Height+Width+L21+L32, data = newfish_q,
                trControl=trcontrol, metric='Accuracy', method='qda')


## NN(Nearest Neighbor)
newfish1 = newfish[,c(1,2,3,6,8,9)]
trControl = trainControl(method='cv', number=10)
set.seed(1)
fit_nn = train(Species~., data = newfish1, metric='Accuracy', method='knn',
               tuneGrid=expand.grid(k=1:10), trControl=trControl)
fit_nn
newfish1_test = fish_test[, c(1,2,5,7,8)]
nn_pred = predict(fit_nn, newfish1_test)
nn_pred


## logistic regrssion
library(nnet)
# maxit：最大的迭代次數
newfish_logistic = multinom(Species~., data=newfish, maxit=250)
newfish_logistic
table(newfish$Species, predict(newfish_logistic, newfish))

# 10 fold cv
library(glmnet)
x = as.matrix(newfish[, -1])
y = newfish[, 1]
cvfit = cv.glmnet(x, y, family='multinomial', type.measure = 'class',
                  nfolds = 10)
predict.value = predict(cvfit, x, s = "lambda.min", type='class')
table(newfish$Species, predict.value)

# 對 test data 做預測
test_pred = predict(newfish_logistic, fish_test)
test_pred
