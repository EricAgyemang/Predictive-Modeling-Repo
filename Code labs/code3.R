###read data#### local or web####
plant=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/table_69.csv",sep=",")

############ anova for one factor##############
res.lm1=glm(weight~group,family=gaussian,data=plant)
summary(res.lm1)

res.lm0=glm(weight~1,family=gaussian,data=plant)
summary(res.lm0)


###############F test for both models#####################
anova(res.lm1, res.lm0)

############ anova for two factor##############
bal=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/Table_612.csv",sep=",")

res.glmint=glm(data~A*B, family=gaussian, data=bal)

summary(res.glmint)
res.glmadd=glm(data~A+B, family=gaussian, data=bal)

summary(res.glmadd)

anova(res.glmint,res.glmadd)

############ ANOCOVA##############
achieve=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/Table_613.csv",sep=",")

res.glm=glm(y~x+method, family=gaussian, data=achieve)
summary(res.glm)
anova(res.glm)

######################
library(class)
library(MASS)


##################################GAM############
attach(Boston)
n<-dim(Boston)[1]

set.seed(8)
train=sample(n,n/2)
lm.fit1 =glm(medv~.,data=Boston,subset=train)
summary(lm.fit1)

y.test<-medv[-train]

####################scatter plot####
plot(rm,medv)


#################polynomial fitting#####################################
p.fit=lm(medv~poly(rm,4,raw=T),data=Boston,subset=train)
summary(p.fit)

p.pred=predict(p.fit,newdata=Boston[-train,])


MSE1=mean((y.test-p.pred)^2)

p.fit1=lm(medv~rm,data=Boston,subset=train)
p.pred1=predict(p.fit1,newdata=Boston[-train,])  ###add_pi~library(ciTools)

plot(rm[-train],medv[-train])
points(rm[-train],p.pred1,col="red")
points(rm[-train],p.pred,col="blue")
#points(rm[-train],s.pred,col="green")

MSE2=mean((y.test-p.pred1)^2)

###############spline fitting#####################
####cubic spline####
library(splines)
s.fit<-lm(medv~bs(rm,knots=c(8)),data=Boston,subset=train)
s.pred<-predict(s.fit,newdata=Boston[-train,])

MSE3<-mean((y.test-s.pred)^2)

#################natural spline####################
ns.fit<-lm(medv~ns(rm,df=4),data=Boston,subset=train)
ns.pred<-predict(ns.fit,newdata=Boston[-train,])

MSE4<-mean((y.test-ns.pred)^2)

#################GAM fitting###########
library(gam)
gam.fit<-gam(medv~s(crim,4)+zn+s(rm,5),data=Boston,subset=train)  ##if you are fitting 0,1, use family="binomial"

gam.pred<-predict(gam.fit,newdata=Boston[-train,])

MSE4<-mean((y.test-gam.pred)^2)


################other building blocks####
gam.fit1<-gam(medv~lo(crim,span=.4)+zn+s(rm),data=Boston,subset=train)  ##if you are fitting 0,1, use family="binomial"

gam.pred1<-predict(gam.fit1,newdata=Boston[-train,])

MSE4<-mean((y.test-gam.pred1)^2)





