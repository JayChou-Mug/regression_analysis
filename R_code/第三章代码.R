

例题3-1
data3.1<-read.csv("D:/data3.1.csv",head=TRUE)
lm3.1<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=data3.1)   #建立回归方程
summary(lm3.1)


X1<-as.matrix(data3.1[,1:9])  #将数据列表中自变量部分数据提取并转化成矩阵形式存储
X<-cbind(1,X1)  #将元素全为1的列向量和X1合并生成矩阵X
XX<-t(X)%*%X   #计算矩阵X'X
sigma<-389.4  #残差的标准差为σ的估计值
covBeta<-sigma^2*solve(XX)   #计算协方差，solve()计算矩阵的逆
covBeta   #协方差阵


r<-matrix(nrow = 10,ncol = 10)  #建立10行10列矩阵，矩阵中元素为空
 
for (i in 1:10) {
  for (j in 1:10) {
    r[i,j] <- covBeta[i,j]/(sqrt(covBeta[i,i]) * sqrt(covBeta[j,j]))
  }
}  #计算相关阵中每个元素的值
r  #相关系数阵


install.packages("car")     #安装car包
library(car)                #加载car包
Anova(lm3.1,type = "III")   #输出方差分析表



lm3.1_drop9<-lm(y~x1+x2+x3+x4+x5+x6+x7+x8,data = data3.1)  #y对除x9外的变量作回归
summary(lm3.1_drop9)

lm3.1_drop976<-lm(y~x1+x2+x3+x5,data = data3.1)  #y对除x9外的变量作回归
summary(lm3.1_drop976)




例题3.2
data3.2<-read.csv("D:/data3.2.csv",head=TRUE)
lm3.2<-lm(y~x1+x2,data=data3.2)
summary(lm3.2)
r<-cor(data3.2)
r      #计算相关系数阵
install.packages("corpcor")
library(corpcor)
pcor3.2<-cor2pcor(r)   #由相关系数阵计算偏相关系数阵
pcor3.2




例题3.3

data3.3<-read.csv("D:/data3.3.csv",head=TRUE)
cor3.3<-cor(data3.3[,-1])   #出去第一列数据计算矩阵
round(cor3.3,digits = 3)  #结果保留三位小数
lm3.3<-lm(y~x1+x2+x3+x4+x5,data = data3.3)
summary(lm3.3)


