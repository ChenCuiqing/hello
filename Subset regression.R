# === 在子集回归中实现交叉验证 ===
library(ISLR)
library(leaps)
head(Hitters)
fix(Hitters)
summary(Hitters)
dim(Hitters)
head(is.na(Hitters$Salary))  
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)  
dim(Hitters)
sum(is.na(Hitters))

# 子集回归中的预测函数
predict.regsubsets<-function(object,testdata,d){
  # object: 子集回归拟合
  # testdata: 测试数据
  # d: 子集回归得到d个变量的最优模型
  formula <- as.formula(object$call[[2]])  # 提取子集回归方程y~.
  mat <- model.matrix(formula, testdata)  # 将测试数据中的X转为矩阵
  coefd <- coef(object, d)  # 提取d个变量最优模型的系数
  xvars <- names(coefd)  
  mat[, xvars] %*% coefd  # d变量模型下测试数据的预测值
} 

# 子集回归中的交叉验证函数
cv.regsubsets<-function(formula, data, nvmax = 8, method = "exhaustive", k = 10){
  # formula: 回归方程
  # data: 数据集
  # nvmax: 最大自变量个数
  # method: 子集回归选择的方法
  # K: 交叉验证折数
  n<-rep(1:k,length=nrow(data))  
  index<-sample(n,nrow(data),replace = F)  
  err.mat<-matrix(NA,k,nvmax)  # 创建矩阵储存交叉验证均方误差
  for(i in 1:k ){train<-which(index != i)
  test<-(which(index == i))
  best.fit<-regsubsets(formula,data[train,],method = method,nvmax = nvmax)
  for(j in 1:nvmax){
    predj<-predict.regsubsets(best.fit, data[test, ], j)
    err.mat[i,j]<-mean((data[test,all.vars(formula)[1]]-predj)^2)
  }
  }
  error<-apply(err.mat,2,mean)
}

cv.error<-cv.regsubsets(Salary ~., data = Hitters, nvmax = 19, method = "forward", k=10)


# === boostrap在投资收益的应用 ===
library(ISLR)
head(Portfolio)
dim(Portfolio)

alpha<-function(X,Y,data,R){
  # R:抽取boostrap样本次数
  alpha.vec<-rep(0,R)  
  for(i in 1:R){
    index<-sample(1:nrow(data),nrow(data),replace = T)  # 有放回的抽取输出Boostrapy样本序号  
    X.boot <- data$X[index]
    Y.boot <- data$Y[index]
    alpha.vec[i] <- (var(Y.boot) - cov(X.boot, Y.boot)) / (var(X.boot) + var(Y.boot) - 2*cov(X.boot, Y.boot))
  }
  return(alpha.vec)  # 返回一组估计值标准差 
}

alpha.vec<-alpha(X,Y,Portfolio,100)
sd(alpha.vec)

# 输出估计值直方图
library(RColorBrewer)
miscolores<-brewer.pal(8,"Set2")[1:8]  # 调色板
par(mar = c(5,5,0.1,0.1))
hist(alpha.vec, col = miscolores[3], xlab = expression(alpha), main = "")
