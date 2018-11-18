require(MASS)
library(randomForest)
set.seed(1)
#== 划分数据集
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
boston.test <- Boston[-train, "medv"]

#==因为随机森林中的每颗树的观测都是自助样本，所以我们设定随机种子使结果重复出现
set.seed(1)


#== 写一个循环输出随机森林方法中不同树和mtry(每个分裂点可选的变量数)值的测试误差，我们用矩阵储存这些测试误差
rf.error<-matrix(NA,13,500)
for (i in 1:13){
  for(j in 1:500){
    rf.boston <- randomForest(medv ~., data = Boston, subset = train, importance = T, mtry = i,ntree=j)
    yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
    rf.error[i,j]<-mean((yhat.rf - boston.test)^2)
  }
}

#== 输出矩阵最小值和最大值
min(rf.error)
max(rf.error)

#== 绘制mtry取值1到13的情况下不同树的个数的测试误差图
x<-seq(1,500) # 产生一个向量，向量中元素的最小值为1，最大值为500
plot(x,rf.error[1,],col = "red",type = "l",xlab="Number of trees",ylab = "error"
     ,main="error of tree_method",ylim = c(10,76))
lines(x,rf.error[2,],col = "blue",type = "l")  # 添加mtry等于2的情况误差随树取值变化图
lines(x,rf.error[3,],col = "green",type = "l")
lines(x,rf.error[4,],col = "gray",type = "l")
error<-rbind(rf.error[5,],rf.error[6,],rf.error[7,],rf.error[8,],rf.error[9,]
             ,rf.error[10,],rf.error[11,],rf.error[12,],rf.error[13,])
matlines(x,t(error),col =5:13,lwd = 1, lty = 3)  # 要求行数和列数相等
legend("topright", c("m=1", "m=2", "m=3","m=4", "m=5", "m=6","m=7", "m=8", "m=9","m=10", "m=11", "m=12","m=13"),
       col =1:13, cex = 0.5, lty = 1)  # 添加图例


#== 划分数据集
set.seed(1)
train = sample(dim(Boston)[1], dim(Boston)[1]/2)
X.train = Boston[train, -14]
X.test = Boston[-train, -14]
Y.train = Boston[train, 14]
Y.test = Boston[-train, 14]

#== 设定mtry取值
mtry = dim(Boston)[2] - 1  # 预测变量总数-1
mtry.2 =mtry/2 
mtry.sq = sqrt(mtry)

#== 随机森林方法mtry取定值的情况下，需要种植的最大树为500，给出测试集在森林中不同树的个数下的预测值
rf.boston.mtry = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                              mtry = mtry, ntree = 500)
rf.boston.mtry.2 = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                                mtry = mtry.2, ntree = 500)
rf.boston.mtry.sq = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                                 mtry = mtry.sq, ntree = 500)
names(rf.boston.mtry)

#== 绘制mtry取值3个的情况下不同树的个数的测试误差图
plot(1:500, rf.boston.mtry$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.mtry.2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.mtry.sq$test$mse, col = "blue", type = "l")
legend("topright", c("m=mtry", "m=mtry/2", "m=sqrt(mtry)"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1) # 添加图例