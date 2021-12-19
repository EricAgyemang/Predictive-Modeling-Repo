###read data#### local or web####
car=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/Car.csv")

############ logistic regression##############
library("nnet")
res1.cars=multinom(response~factor(age)+factor(sex), weights=frequency,data=car)
summary(res1.cars)

#######relevel variables############
car$response=relevel(car$response,ref=c("no/little"))
car$sex=relevel(car$sex,ref=c("women"))
car$age=relevel(car$age,ref=c("18-23"))
res2.cars=multinom(response~factor(age)+factor(sex), weights=frequency,data=car)
summary(res2.cars)

#######create dummy variables############
car$men <- with(car, 1*(sex == "men"))
car$women <- with(car, 1*(sex == "women"))
car$age1<-with(car, 1*(age == "18-23"))
car$age2<-with(car, 1*(age == "24-40"))
car$age3<-with(car, 1*(age == "> 40"))
res3.cars=multinom(response~men+age2+age3, weights=frequency,data=car)
summary(res3.cars)

fitted(res3.cars)
residuals(res3.cars)
resid(res3.cars,type="pearson")

##########################ordinary logistic regression#########
library(MASS)
colnames(car)[1]="sex"
res.polr=polr(factor(response)~factor(age)+factor(sex),weights=frequency,data=car)

summary(res.polr)

fitted(res.polr)


library(ordinal)
res.clm=clm(factor(response)~factor(age)+factor(sex),weights=frequency,data=car)

summary(res.clm)

fitted(res.clm)

confint(res.clm, type = "Wald")






