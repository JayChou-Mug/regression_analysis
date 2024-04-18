x=c(1:5)
y=c(10,10,20,20,40)
#散点图
plot(x,y,main="销售收入与广告费用关系图",xlab="广告费用",ylab="销售收入")
fit=lm(y~x) #最小二乘估计
summary(fit) #参数估计，参数检验，回归标准误及参数显著性检验p值，决定系数
confint(fit) #参数的区间估计
anova(fit) #方差分析
cor.test(x,y,alternative="two.side")#相关系数显著性检验
plot(x,fit$residuals,main="残差图")#残差图
#prediction是对y的实际值的预测，参数confidence是对y的期望值的预测
new=data.frame(x=4.2)
pred_y=predict(fit,new)
pred_plim=predict(fit,new,interval="prediction")
pred_clim=predict(fit,new,interval="confidence")
