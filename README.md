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

