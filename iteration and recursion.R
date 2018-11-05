# === 计算n的阶乘 ===
# == interation ==
factoriaIte<-function(x){
if(x==0|x==1) return(1)
  else for(i in 1:(x-1)){
    x=x*i
  }
  return(x)
 }
factoriaIte(4)

# == recursion ==
factoriaRec<-function(x){
if(x==0) return(1)
  else{
    return(x*factoriaIte(x-1))
  }
}
factoriaRec(4)

# 迭代5000次和递归5000次用时
system.time(factoriaIte(5000))
system.time(factoriaRec(5000))



# === 产生一个长度为n的斐波那契数列1，1，2，3，5，8，13... ===
# ==interation ==
FibonaIte<-function(n){
# 数列的长度
if(n<0) stop("n should be large than 0")
fibonacci<-rep(NA,n) 
  for(i in 1:n){
if(i==1|i==2){
  fibonacci[i]=1
   }
else{
  fibonacci[i]=fibonacci[i-1]+fibonacci[i-2]
  }
    }
return(fibonacci)
  }
FibonaIte(5)


# == recursion ==
FibonaRec<-function(n){
# 数列的长度
if(n==0|n==1){
  return(1)
}else{
    FibonaRec(n-1)+FibonaRec(n-2)
}
  Fibona<-rep(0,n)
  for(i in 1:n){
    Fibona[n]<-FibonaRec(n)
  }
  return(Fibona)
}
FibonaRec(5)

Fibona



# === 最小二乘法估计模型系数 === 
setwd("E:/")
Auto<-read.csv(file = "Auto.csv",header = TRUE,na.strings = "?")
Auto<-na.omit(Auto)
head(Auto)
summary(Auto)
fit<-lm(mpg~displacement+horsepower+weight,Auto)
coef(fit)

# ===最小二乘和极大似然法估计线性回归模型系数 ===
x<-model.matrix(mpg~displacement+horsepower+weight,Auto) # 提取回归设计阵
y<-Auto$mpg
beta<-solve(t(x)%*%x)%*%t(x)%*%y 

