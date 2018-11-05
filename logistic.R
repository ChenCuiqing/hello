
# === 逻辑回归中不同的系数估计法及交叉验证 ===
library(ISLR)
names(Smarket)
dim(Smarket)
X<-model.matrix(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,Smarket)  # 提取回归设计阵
y<-Smarket$Direction
y <- ifelse(y == "Up", 1, 0)  


# R自带函数系数估计
glm.fits<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket ,family=binomial)
summary(glm.fits)
coef<-coef(glm.fits)


# 梯度下降法估计系数
Graddescent<-function(x,y,alpha=0.005,maxCycles=10000){
  # alpha: 步长
  # maxCycle: 最大迭代次数
  beta<-rep(0,ncol(x))  # 系数初始值
  iterations<-0  
  for(i in 1:maxCycles){
    gradients = -t(x) %*% (y - (1/(1+exp(-x%*%beta))))  # 负梯度
    beta = beta - alpha * gradients  # 更新beta值 
    iterations = iterations + 1  # 记录迭代次数
   if(t(alpha * gradients) %*% (alpha * gradients) < 0.000001)
      break
  }
  return(list(beta = beta, iterations = iterations))  # 返回系数估计和迭代值
}

beta<-Graddescent(x = X, y = y)

# === NewtonRapshon ===
NewtonRapshon<-function(x, y, alpha=0.001, maxCycles=10000){
  # alpha: 步长
  # maxCycle: 最大迭代次数
  beta<-rep(0,ncol(X))  
  iterations<-0 
  for (i in 1:maxCycles){
    predProb  = 1/1+exp(-x%*%beta)  # 概率函数
    derivative = t(x)%*%(y - predProb)  # 一阶导
    H = t(x)%*%diag(c(predProb*(1 - predProb)))%*%x  # 二阶导
    updata = alpha*solve(H)%*%derivative  # 更新值
    beta = beta - updata  # 更新beta值
    iterations = iterations + 1
   if (t(updata) %*% updata < 0.000001)
      break
  }
  return(list(beta = beta, iterations = iterations))
}

NewtonRapshon(x = X, y = y)


# logistic预测函数
predict_glm <- function(xtest, ytest, beta){
  # xtest: 自变量的测试数据
  # ytest: 因变量的测试数据
  # beta: 系数估计值
  predProb = 1/(1+exp(-xtest%*%beta))  # 预测概率
  predvalue = ifelse(predProb>0.5, 1, 0)  # 预测值
  err.rate = mean(predvalue != ytest)  # 均方误差
  return(err.rate)
}





# === 在logistic回归不同系数估计法中中实现交叉验证 ===
cv_glm<-function(fomula,x,y,data,k=10){
  # formula: 回归方程式
  # data: 数据集
  # k: 交叉验证折数
  n<-nrow(x)
  fold<-rep(1:k,length=n)
  index<-sample(fold,n,replace = F)  
  error.glm<-rep(0,k)
  error.grad<-rep(0,k)
  error.newton<-rep(0,k)
  for(i in 1:k){
    train<-which(index!=i)
    test<-which(index==i)
    xtest<- x[test, ]
    ytest<-y[test]
    
    # R自带函数系数估计
    glm.fits<-glm(fomula, data=data[train, ],family=binomial)
    coef<-coef(glm.fits)
    error.glm[i]<-predict_glm(xtest,ytest,coef)
    
    # 梯度下降法估计系数
    beta.Grad<-Graddescent(x, y)
    error.grad[i]<-predict_glm(xtest,ytest,beta.Grad[[1]])
    
    # NewtonRapshon估计系数
    beta.nton<-NewtonRapshon(x, y)
    error.newton[i]<-predict_glm(xtest,ytest,beta.nton[[1]])
  }
return(list(error.glm=mean(error.glm),error.grad=mean(error.grad),error.newton=mean(error.newton)))  # 返回各系数估计法的均方误差
}
cv_glm(fomula = Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, x=X, y=y ,data = Smarket, k=10)
