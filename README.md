Logistic

Logistic文件中使用R自带的ISLR包里的Smarket数据，比较R自带glm函数的系数估计法、自行编写的梯度下降法
以及牛顿迭代法的预测错误率（根据交叉验证输出预测结果的均方误差）

结果表明自带函数的预测错误率最低，牛顿迭代法的预测错误率略低于梯度下降法


结果如下：
cv_glm(fomula = Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, x=X, y=y ,data = Smarket, k=10)

$error.glm
[1] 0.4864

$error.grad
[1] 0.5192

$error.newton
[1] 0.5184


Subset regression

1.在子集回归中实现交叉验证，结果显示包含12个变量的子集测试均方误差最小

结果如下：
> min(cv.error)
[1] 118074.5
> which.min(cv.error)
[1] 12


2.在投资收益的例子中抽取boostrap样本，编写函数输出boostrap样本估计值
