###read data#### local or web####
smoking=read.csv("I:/dropbox//Dropbox/eric/spring-2019/MAT355/smoking.csv")

colnames(smoking)[1]="age"

levels(smoking$age) <- c(1:5)
smoking$age=as.numeric(smoking$age)
smoking$smoke=with(smoking, 1*(smoking == "smoker"))
smoking$agesq=smoking$age^2


res.doc<-glm(deaths~age + agesq + smoke + smoke:age +offset(log(person.years)),family=poisson(),data=smoking)
summary(res.doc)
############Residual diagnostic##############
library(boot)
fitted(res.clm)

glm.diag(res.doc)

confint(res.doc, type = "Wald")

###########pearson chi square test#######

x=resid(res.doc,type="pearson")
ps=sum(x^2)

####p value#####
1-pchisq(ps,df=5)

########log-likelihood function#########
logLik(res.doc)

res.doc_min<-glm(deaths~1,family=poisson(),data=smoking)
summary(res.doc_min)

logLik(res.doc_min)


Rsq1=1-res.doc$deviance/res.doc$null.deviance

Rsq2=1-logLik(res.doc)/logLik(res.doc_min)


