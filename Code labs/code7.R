###read data#### local or web####
mela=read.csv("I:/dropbox//Dropbox/eric/spring-2019/MAT355/melanoma.csv")

res.mela1<-glm(frequency~site+type,family=poisson(),data=mela)
summary(res.mela1)


res.mela2<-glm(frequency~site*type,family=poisson(),data=mela)
summary(res.mela2)

################particular reference level######
mela$type=relevel(mela$type,ref=c("hutchinson's melanotic freckle"))
mela$site=relevel(mela$site,ref=c("head & neck"))

res.mela11<-glm(frequency~site+type,family=poisson(),data=mela)
summary(res.mela11)

expted=fitted(res.mela11)

res.mela22<-glm(frequency~site*type,family=poisson(),data=mela)
summary(res.mela22)
fitted(res.mela22)

anova(res.mela22,res.mela11,test="Chisq")

############aspirin use##############
aspirin=read.csv("I:/dropbox//Dropbox/eric/spring-2019/MAT355/ulcear.csv")

res1.as<-glm(frequency~ ulcer*case.control, family=poisson(), data=aspirin)

summary(res1.as)
res2.as<-glm(frequency~ulcer*case.control+aspirin, family=poisson(), data=aspirin)

summary(res2.as)



