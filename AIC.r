####AIC####
library(glmulti)
d1<-read.csv("meta_update.csv")

d2<-escalc(measure="ROM",data=d1,m1i=Total,sd1i=Totalsd,n1i=n,m2i=Total1,sd2i=Totalsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=Ba,sd1i=Basd,n1i=n,m2i=Ba1,sd2i=Basd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=Fu,sd1i=Fusd,n1i=n,m2i=Fu1,sd2i=Fusd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=Om,sd1i=Omsd,n1i=n,m2i=Om1,sd2i=Omsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=PP,sd1i=PPsd,n1i=n,m2i=PP1,sd2i=PPsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=H,sd1i=Hsd,n1i=n,m2i=H1,sd2i=Hsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=MI,sd1i=MIsd,n1i=n,m2i=MI1,sd2i=MIsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=EI,sd1i=EIsd,n1i=n,m2i=EI1,sd2i=EIsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=SI,sd1i=SIsd,n1i=n,m2i=SI1,sd2i=SIsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=CI,sd1i=CIsd,n1i=n,m2i=CI1,sd2i=CIsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=PPI,sd1i=PPIsd,n1i=n,m2i=PPI1,sd2i=PPIsd1,n2i=n)
d2<-escalc(measure="ROM",data=d1,m1i=SP,sd1i=SPsd,n1i=n,m2i=SP1,sd2i=SPsd1,n2i=n)

rma.glmulti <- function(formula, data, ...) { rma(formula, vi, data=data, method="ML", ...)}

modelselection<- glmulti(yi ~ Fertilizer+Climatetype+ecosystem+Duration,
                         data=subset(d2), level=1, 
                         fitfunction=rma.glmulti, crit="aicc", confsetsize=100)
plot(modelselection)

summary(modelselection@objects[[1]])

modelweights<- weightable(modelselection)
modelweights

summary(modelselection@objects[[1]])

bestmodelweights<- modelweights[modelweights$aicc <= min(modelweights$aicc) + 2,]
bestmodelweights

plot(modelselection, type="s")