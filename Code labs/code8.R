###read data#### local or web####
stroke_wide=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/stoke.csv")

library(nlme)

####reformate the data####
library(reshape2)
stroke=melt(stroke_wide, id.vars=c("Subject", "Group"))

stroke$ability=stroke$value
stroke$time=as.numeric(stroke$variable)


####independent model##
ind<-corIdent(form = ~ 1 | Subject)
gls.ind<-gls(ability~Group+time+Group*time, data=stroke,correlation=ind)
summary(gls.ind)

####exchangeable model##
exch<-corCompSymm(form = ~ 1 | Subject)
gls.exch<-gls(ability~Group+time+Group*time, data=stroke, correlation=exch)
summary(gls.exch)
####AR model##
ar1<-corAR1(form = ~ 1 | Subject)
gls.ar1<-gls(ability~Group+time+Group*time, data=stroke, correlation=ar1)
summary(gls.ar1)

####unstructed model##
un<-corSymm(form = ~ 1 | Subject)
gls.un<-gls(ability~Group+time+Group*time, data=stroke,  correlation=un)

AIC(gls.ind,gls.exch,gls.ar1,gls.un)

##########################library########
library(geepack)

gee.ind<-geeglm(ability~Group+time+Group*time,family=gaussian, data=stroke,id=Subject,wave=time,corst="independence")
summary(gee.ind)

gee.exch<-geeglm(ability~Group+time+Group*time,family=gaussian,data=stroke,id=Subject,wave=time,corst="exchangeable")
summary(gee.exch)

gee.ar1<-geeglm(ability~Group+time+Group*time,family=gaussian,data=stroke,id=Subject,wave=time,corst="ar1")
summary(gee.ar1)
stroke_new=data.frame(Group=factor(stroke$Group),time_week=as.numeric(stroke$time),Subject=stroke$Subject,
                      ability=as.numeric(stroke$ability))
anova(gee.ind,gee.exch)
#######not run#########
gee.un<-geeglm(ability~Group+time+Group*time,family=gaussian, data=stroke,id=Subject,waves=time,corst="unstructured") 

######################
library(gee)

gee.un<-gee(ability~Group+time+Group*time,family=gaussian, data=stroke,id=Subject,corst="unstructured") 
summary(gee.un)

library(nlme)

random_eff<-lme(ability~Group+time+Group*time,data=stroke,random=~1|Subject)
summary(random_eff)

random_eff1<-lme(ability~Group+time+Group*time,data=stroke,random=~1|Subject,cor=corAR1())
summary(random_eff1)

random_eff2<-lme(ability~time,data=stroke,random=~1|Subject/Group)   ###group nested in the subject
summary(random_eff2)


random_eff3<-lme(ability~Group*time,data=stroke,random=list(~1|Subject, ~1|Group))   ###group nested in the subject
summary(random_eff3)



