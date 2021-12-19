###read data#### local or web####
mortality=read.csv("I:/dropbox//Dropbox/teaching-related/Teaching-ISU/eric/spring-2019/MAT355/mortality.csv",sep="")

############ binomial regression##############
yall=cbind(mortality$y,mortality$n-mortality$y)
res.lm1=glm(yall~x,family=binomial(link="logit"),data=mortality)
summary(res.lm1)

fitted.lm1=fitted.values(res.lm1)  ###estimated proportions of deaths in each group
fit_y=mortality$n*fitted.lm1
fit_y

#####prediction##############
pred.lm1=predict.glm(res.lm1,newdata = data.frame(x=1.8),"response") 
 
############ probit##############
res.lm2=glm(yall~x,family=binomial(link="probit"),data=mortality)
summary(res.lm2)

fitted.lm2=fitted.values(res.lm2)  ###estimated proportions of deaths in each group
fit_y2=mortality$n*fitted.lm2
fit_y2

############ cloglog##############
res.lm3=glm(yall~x,family=binomial(link="cloglog"),data=mortality)
summary(res.lm3)

fitted.lm3=fitted.values(res.lm3)  ###estimated proportions of deaths in each group
fit_y3=mortality$n*fitted.lm3
fit_y3

 
#########################anthers data###########
anthers=read.csv("I:/dropbox//Dropbox/eric/spring-2019/MAT355/anthers.csv",sep=",")

ally=cbind(anthers[,1],anthers[,2]-anthers[,1])
anthers[1,]

res.glm3=glm(ally~storage*log(centrifuge),family=binomial(link="logit"),data=anthers)
summary(res.glm3)

res.glm4=glm(ally~storage+log(centrifuge),family=binomial(link="logit"),data=anthers)
summary(res.glm4)

res.glm5=glm(ally~log(centrifuge),family=binomial(link="logit"),data=anthers)
summary(res.glm5)

anova(res.glm3,res.glm4)
anova(res.glm4,res.glm5)


####################
senility=read.csv("I:/dropbox//Dropbox/eric/spring-2019/MAT355/sennility.csv",sep=",")

####for the ungroup data#########
res.glm=glm(s~x,family=binomial(link="logit"),data=senility)

summary(res.glm)

################fitted probabilities######
fitted.values(res.glm) 

######Pearson residuals########

x=resid(res.glm,type="pearson")

######standardized Pearson residuals####
library(boot)
glm.diag(res.glm)$rp

######Deviance residuals##############
rd=resid(res.glm)

####standardized deviance residuals####
srd=rstandard(res.glm)

###########Deviance#############
sum(rd^2)

####Pearson statistics##
sum(x^2)

#######################binomial fitting#################
library(doBy)
fun <- function(x){
  c(y=sum(x), n=length(x))
}

gdata=summaryBy(s~x,data=senility,FUN =fun)
names(gdata)=c("x","y","n")

 
res.glm=glm(cbind(y,n-y)~x,family=binomial(link="logit"),data=gdata)
summary(res.glm)

glm.diag.plots(res.glm)

qqPlot(res.glm$residuals, main="QQ Plot",distribution = "norm",id=FALSE)

shapiro.test(res.glm$residuals)
ks.test(res.glm$residuals,"pnorm",mean=0,sd=sd(res.glm$residuals))

#####################
library(ResourceSelection)
res.glm=glm(s~x,family=binomial(link="logit"),data=senility)
hoslem.test(s, fitted(res.glm), g = 3)


