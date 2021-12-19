###read data#### local or web####
carbohydrate=read.csv("I:/dropbox//Dropbox/learning-related/ISU/EricAgyemang/ISU/MAT355-2018/lec6/Table 6.3 Carbohydrate diet.csv",sep="")

############ linear regression##############
res.lm=lm(carbohydrate~age+weight+protein,data=carbohydrate)
summary(res.lm)

res.lm1=lm(carbohydrate~weight+protein,data=carbohydrate)
summary(res.lm1)


###############F test for both models#####################
anova(res.lm, res.lm1)

####GLM model#
res.glm=glm(carbohydrate~age+weight+protein, family=gaussian,data=carbohydrate)
summary(res.glm)

##########hypothesis testing for GLM#########
library(car)

##approach 1####
linearHypothesis(res.glm, "age=0",test=c("F"))

##approach 2####
hm=c(0,1,0,0)
rhs=c(0)
linearHypothesis(res.glm,  hypothesis.matrix=hm,rhs=rhs,test=c("F"))

######outlier test###
outlierTest(res.glm)
#########influence measures########

influence.measures(res.glm)

#########diagnostics for glm#####
library(boot)
glm.diag.plots(res.glm)

##########normality test########
qqPlot(res.glm$residuals, main="QQ Plot",distribution = "norm",id=FALSE)

shapiro.test(res.glm$residuals)
ks.test(res.glm$residuals,"pnorm",mean=0,sd=sd(res.glm$residuals))

#########colinearity test##
vif(res.glm)


#########model selection#########
########################subset selection##################
library(leaps)

####stepwise selection based on AIC, could be based on BIC use k=2#####
 

m0<- lm(carbohydrate ~ 1,data=carbohydrate)

ansf1 <- step(m0, scope=list(lower= ~ 1,
                             upper=~age+weight+protein), direction="forward", data=carbohydrate,trace=1)

 
ansf2 <- step(res.lm, scope=list(lower= ~1,
                              upper=~age+weight+protein),
              direction="backward",  data=carbohydrate,trace=1)

##############################
library(olsrr)

ols_step_backward_p(res.lm, details=TRUE)
ols_step_forward_p(res.lm, details=TRUE)
ols_step_forward_aic(res.lm, details=TRUE)


##################Cross validation#############
library(cvTools)
cvFit(res.lm, data = carbohydrate, K = 5, R = 10, y = carbohydrate$carbohydrate)

#############Ridge Regression##################
####ridge regression###
library(MASS)
library(ISLR)
library(glmnet)

###creat X and Y variables

x<-model.matrix(carbohydrate~.,data=carbohydrate)[,-1] 
y<-carbohydrate$carbohydrate

###ridge regression
ridge.fit<-glmnet(x,y,alpha=0)  #standardizes variables

####coefficients########
ridge.fit$lambda  ##show lambda
coef(ridge.fit) 

###check the sum##
set.seed(8)

cv.out=cv.glmnet(x,y,alpha=0,nfolds=5)

plot(cv.out)
bestlam=cv.out$lambda.min


ridge.fit1<-glmnet(x,y,alpha=0,lambda=bestlam)  #standardizes variables
coef(ridge.fit1) 

###########lasso regression#########
lasso.fit<-glmnet(x,y,alpha=1)  #standardizes variables

####coefficients########
lasso.fit$lambda  ##show lambda
coef(lasso.fit) 

###check the sum##
set.seed(8)

cv.out=cv.glmnet(x,y,alpha=1,nfolds=5)

plot(cv.out)
bestlam=cv.out$lambda.min


lasso.fit1<-glmnet(x,y,alpha=0,lambda=bestlam)  #standardizes variables
coef(lasso.fit1) 


















