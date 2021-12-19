###read data#### local or web####
dd=read.csv("I:/dropbox/Dropbox/teaching-related/ISU/EricAgyemang/ISU/MAT355-2019/data/Table 2.7 Plant weight.csv")

####EDA###
summary(dd)
boxplot(dd)

#####t test####
t.test(dd[,1],dd[,2])

####test for the linear hypothesis##
library(car)

####################weibull data anlaysis################
weibull=read.csv("I:/dropbox/Dropbox/teaching-related/ISU/EricAgyemang/ISU/MAT355-2019/data/Table 4.1 Failure times of pressure vessels.csv",header=T)
hist(weibull[,1],freq = T)


#####fit distributioni####
library(fitdistrplus)

weibull_fit=fitdist(weibull[,1],"weibull")
norm_fit=fitdist(weibull[,1],"norm")

summary(weibull_fit)
summary(norm_fit)

plot(weibull_fit)
