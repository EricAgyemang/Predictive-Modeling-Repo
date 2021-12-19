###read data#### local or web####
stress=read.table("http://my.ilstu.edu/~eric/spring-2019/MAT355/stress.txt",sep="")
colnames(stress)=c("smoke","gender","age","stress","count")

##############EDA############
count11=sum(stress[stress$stress==1&stress$smoke==1,5])
count12=sum(stress[stress$stress==2&stress$smoke==1,5])
count13=sum(stress[stress$stress==3&stress$smoke==1,5])

count21=sum(stress[stress$stress==1&stress$smoke==2,5])
count22=sum(stress[stress$stress==2&stress$smoke==2,5])
count23=sum(stress[stress$stress==3&stress$smoke==2,5])

count31=sum(stress[stress$stress==1&stress$smoke==3,5])
count32=sum(stress[stress$stress==2&stress$smoke==3,5])
count33=sum(stress[stress$stress==3&stress$smoke==3,5])

smoke1=c(count11,count21, count31)
smoke2=c(count12,count22, count32)
smoke3=c(count13,count23, count33)

data.frame(smoke1,smoke2,smoke3)

smoke_freq=data.frame(level=c(rep(1,3),rep(2,3),rep(3,3)),smoke=c(1,2,3,1,2,3,1,2,3),freq=c(smoke1,smoke2,smoke3))


smoke1_glm<-glm(freq~level+smoke,family=poisson(),data=smoke_freq)
summary(smoke1_glm)


smoke2_glm<-glm(freq~level*smoke,family=poisson(),data=smoke_freq)
summary(smoke2_glm)

#######################################
library(MASS)

glm1=polr(factor(stress)~factor(gender)+factor(smoke)+factor(age),weights=count,data=stress)
summary(glm1)

glm2=polr(factor(stress)~factor(gender)+factor(smoke)+factor(gender):factor(smoke)+factor(age),weights=count,data=stress)
summary(glm2)

glm3=polr(factor(stress)~factor(gender)+factor(smoke)+factor(smoke):factor(age)+factor(age),weights=count,data=stress)
summary(glm3)

fitted(res.polr)

anova(glm2,glm3)

M1 <- logLik(glm1)
M2 <- logLik(glm4)
G <- -2*(M1[1] - M2[1])

pchisq(G,(12-7),lower.tail = FALSE)

############ logistic regression##############
library("nnet")
glm4=multinom(factor(stress)~factor(gender)+factor(smoke)+factor(age), weights=count,data=stress)
summary(glm4)

fitted(glm1)

##########################residual analysis#########
countagg=aggregate(x=stress$count, by=stress[,c(1,2,3)], FUN=sum)

prob_unqi=unique(fitted(glm1)) 

expected_all=as.numeric(t(countagg$x*prob_unqi))

stress$expected=expected_all

resid_all=sum((stress$count-stress$expected)^2/stress$expected)

####pearson residuals###
p_res=(stress$count-stress$expected)/sqrt(stress$expected*as.numeric(t(1-prob_unqi)))


plot(p_res)
qqnorm(p_res)
qqline(p_res)


