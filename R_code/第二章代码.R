
例题2.1

x<-c(3.4,1.8,4.6,2.3,3.1,5.5,0.7,3.0,2.6,4.3,2.1,1.1,6.1,4.8,3.8)
#生成数值向量x并赋予距消防站距离的数据
y<-c(26.2,17.8,31.3,23.1,27.5,36.0,14.1,22.3,19.6,31.3,24.0,17.3,43.2,36.4,26.1)
#生成数值向量y并赋予火灾损失的数据
mean(x)    #计算变量x的均值
sd(x)   #计算变量x的标准差
plot(x,y,main="距消防站距离与火灾损失关系图",xlab="距消防站距离",ylab="火灾损失")  #散点图
lm2.1<-lm(y~x)  #以y为因变量x为自变量建立回归方程，并将结果赋给lm2.1其中默认回归方程是包含截距项的，如果是lm（y~x-1），则不包含截距项
abline(lm2.1,lwd=2, lty=1) # lty 参数用于设置线型，这里是实线，lwd数值越大线越粗

summary(lm2.1)   #输出回归分析的结果
anova(lm2.1)  #输出方差分析表
cor(x,y,method = "pearson")  #计算x和y的相关系数
cor.test(x,y,alternative = "two.sided",method = "pearson",conf.level = 0.95)  #检验相关系数的显著性代码


e<-resid(lm2.1,digits=5) #将残差赋值给变量e，并保留小数点后5位
e  #在窗口中显示e的值

# 绘制残差与拟合值的关系图
plot(fitted(lm2.1), e, 
     xlab = "x", ylab = "e",
     main = "Residual Plot for Simple Linear Regression")

abline(h = 0, lty = 2) # 添加一条水平参考线


ZRE<-e/2.316  #计算标准化残差
ZRE
SRE<-rstandard(lm2.1)  #计算学生化残差
SRE

new<-data.frame(x=3.5)   #输入新值3.5，此处必须以数据框的形式存储新点
ypred<-predict(lm2.1,new,interval = "prediction",level=0.95)
#计算预测值及预测区间并赋给ypred，此处level=0.95可以省略
yconf<-predict(lm2.1,new,interval = "confidence",level=0.95)
#计算预测值及置信区间并赋给ypred，此处level=0.95可以省略
ypred
yconf



例2.2

data2.2<-read.csv("C:/Users/11659/Documents/WPSDrive/994262655/WPS云盘/课程/2023-2024年第2学期/应用回归分析/46440-应用回归分析R语言版原始数据/应用回归分析R语言版原始数据/newdata2.2.csv",head = TRUE)
attach(data2.2)   #将数据添加到R的搜索路径
data_outline<-c(mean(x),sd(x),mean(y),sd(y))   #计算x和y的均值和方差
data_outline #输出结果
cor.test(x,y)   #x与y相关系数的显著性检验
lm2.2<-lm(y~x,data = data2.2)   #建立回归方程及其显著性检验
anova(lm2.2)       #输出线性回归的方差回归表
summary(lm2.2)      #输出回归方程及显著性检验结果
confint(lm2.2)      #计算回归系数95%的置信区间
SRE<-rstandard(lm2.2)      #计算学生化残差
plot(x,SRE,xlab = "城镇居民人均收入",ylab = "学生化残差")  #绘制残差散点图
abline(h = 0, lty = 2) # 添加一条水平参考线
detach(data2.2)      #与attach（）相对应，将数据框从搜索引擎路径中移除


