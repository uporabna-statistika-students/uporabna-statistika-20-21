# ilustracija popolne kolinearnosti in kolinearnosti na generiranih podatkih

x1<- runif(100, min = 0, max = 10)
x2<-(-x1)
x3<- x1 + rnorm(100, mean = 0, sd = 0.5)
x4<-runif(100, min = 0, max = 10)
y<-x1 + x2 + x3 + x4 + rnorm(100, mean = 0, sd = 1)

# korelacijska matrika napovednih spremenljivk
round(cor(cbind(x1, x2, x3, x4)), 4)
# model v katerga vključimo vse napovedne spremenljivke
mod.0<-lm(y~x1+x2+x3+x4)
summary(mod.0)
summary(mod.0)$coef  # za x2 se ocena parametra ne izpiše
summary(mod.0)$r.squared
X.0<-model.matrix(mod.0)
det(t(X.0)%*%X.0)
library(car)
vif(mod.0) #se ne izračuna

# spremenljivko x2 izločimo
mod.1<-lm(y~x1+x3+x4)
vif(mod.1)
coef(summary(mod.1))
confint(mod.1)
avPlots(mod.1, ylim=c(-7,7), xlim=c(-5, 5))

library(effects)
plot(predictorEffects(mod.1, ~.), rows=1, cols=3, main="", ylim=c(0,16))
mod.1a<-lm(y~x1+x4)
vif(mod.1a)
summary(mod.1a)$coeff
summary(mod.1a)$r.squared
confint(mod.1a)
avPlots(mod.1a, ylim=c(-7,7))
plot(predictorEffects(mod.1a, ~.), rows=1, cols=2, main="", ylim=c(0,16))

# ilustracija ravnine na koreliranih spremenljivkah
# install.packages("rgl")
library(rgl)
open3d()
plot3d(lm(y~x1+x3))

# Primer: seatpos
library(faraway)
summary(seatpos)
# vrednosti za hipcenter v podatkovnem okviru setpos so negativne
# interpretacija je lažja, če so pozitivne
seatpos$hipcenter<-(-1)*seatpos$hipcenter

pairs(seatpos)
round(cor(seatpos, method="spearman"),2)
round(cor(seatpos, method="pearson"),2)

mod.0<-lm(hipcenter~., data=seatpos)
vif(mod.0)
avPlots(mod.0, ylim=c(-70,70), xlim=c(-20, 20))
summary(mod.0)$coef
summary(mod.0)$r.squared
mod.1<-update(mod.0, .~. -HtShoes, data=seatpos)
vif(mod.1)
summary(mod.1)$coef
summary(mod.1)$r.squared
mod.2<-update(mod.1, .~. -Seated -Leg, data=seatpos)
vif(mod.2)
summary(mod.2)
mod.3<-update(mod.2, .~. -Arm - Thigh, data=seatpos)
vif(mod.3)
anova(mod.3, mod.2)
par(mfrow=c(2,2), oma=c(0, 0, 2, 0))
plot(mod.3)
avPlots(mod.3, ylim=c(-100, 100), xlim=c(-20, 60))
crPlots(mod.3)
library(multcomp)
izpis<-glht(mod.3)
confint(izpis)$confint


