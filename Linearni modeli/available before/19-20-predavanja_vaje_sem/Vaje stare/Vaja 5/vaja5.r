library(MASS)
# help(Cars93) ## Data from 93 Cars on Sale in the USA in 1993
# names(Cars93)
Cars93$Poraba<-235.21/Cars93$MPG.highway  # v l/100 km
Cars93$Masa<-Cars93$Weight*0.45359/100    # v 100 kg
Cars93$Prostornina<-Cars93$EngineSize     # v litih
Cars93$Moc<-Cars93$Horsepower             # v KM
Cars93$Poreklo<-Cars93$Origin
Cars93$Tip<-Cars93$Type
avti <- subset(Cars93, select=c(Poraba, Masa, Prostornina, Moc, Poreklo, Tip))
rownames(avti)<-Cars93$Make   ### identifikator vozila na slikah
avti$Tip<-relevel(avti$Tip, ref="Van")
summary(avti)
library(ggplot2)
ggplot(data=avti, aes(x=Masa, y=Poraba, col=Tip)) +
  facet_grid(.~Poreklo) + geom_point() + geom_smooth(method="lm", se=FALSE) +
  xlab("Masa (100 kg)") + ylab("Poraba (l/100 km)")
ggplot(data=avti, aes(x=Prostornina, y=Poraba, col=Tip)) +
  facet_grid(.~Poreklo) + geom_point() + geom_smooth(method="lm", se=FALSE) +
  xlab("Prostornina (l)") + ylab("Poraba (l/100 km)")
ggplot(data=avti, aes(x=Moc, y=Poraba, col=Tip)) +
  facet_grid(.~Poreklo) + geom_point() + geom_smooth(method="lm", se=FALSE) +
  xlab("Moč (KM)") + ylab("Poraba (l/100 km)")
# library(gridExtra)
# grid.arrange(p1, p2, p3)
# Ali pa:
ggplot(data=avti, aes(x=Masa, y=Poraba)) + geom_point() +
  facet_grid(vars(Tip), vars(Poreklo)) + geom_smooth(method="lm",se=FALSE)


# koreliranost napovednih spremenljivk
library(car)
scatterplotMatrix(~Poraba+Masa+Prostornina+Moc|Poreklo, regLine=FALSE,
                  legend=TRUE, diagonal=FALSE, smooth=FALSE,
                  data=avti)
scatterplotMatrix(~Poraba+Masa+Prostornina+Moc|Tip, regLine=FALSE, legend=TRUE,
                  diagonal=FALSE, smooth=FALSE, data=avti)
# pairs(avti[,c(1:4)], col=c(1:2)[unclass=avti$Poreklo])
# pairs(avti[,c(1:4)], col=c(1:6)[unclass=avti$Tip])

model.0<-lm(Poraba~ Tip + Poreklo + Masa + Prostornina + Moc, data=avti)
par(mfrow=c(2,2),  oma = c(0, 0, 2, 0))
plot(model.0)
par(mfrow=c(1,1))
vif(model.0)
cor(avti[,c("Masa" ,"Prostornina", "Moc")], method="spearman")
model.1<-update(model.0, .~. - Prostornina)
vif(model.1)
compareCoefs(model.0,model.1)
summary(model.0)$r.squared
summary(model.1)$r.squared

influencePlot(model.0, id=list(n=2))
outlierTest(model.0)

confint(glht(model.0))


######################
# Nelinearnost
################################

#### VAJA Telesna masa in visina zensk

str(women)
plot(women, xlab = "Telesna visina (in)", ylab = "Telesna masa (lb)", pch=16)

mod.lin<-lm(weight~height, data=women)

par(mfrow=c(2,2), oma=c(0,0,3,0))
plot(mod.lin)
par(mfrow=c(1,1))

mod.kvad<-lm(weight~poly(height,2), data=women)
anova(mod.lin, mod.kvad)
mod.kub<-lm(weight~poly(height,3), data=women)
anova(mod.kvad, mod.kub)
mod.4<-lm(weight~poly(height,4), data=women)
anova(mod.kub, mod.4)

par(mfrow=c(2,2), oma=c(0,0,3,0))
plot(mod.kub)
par(mfrow=c(1,1))

library(splines)
vozl.0<-quantile(women$height, c(.33, .67));vozl.0
summary(mod.ns.2 <- lm(weight ~ ns(height, knots=vozl.0), data = women))
anova(mod.lin, mod.ns.2)

par(mfrow=c(2,2), oma=c(0,0,3,0))
plot(mod.ns.2)
par(mfrow=c(1,1))

plot(women, xlab = "Telesna višina (in)", ylab = "Telesna masa (lb)", pch=16)
ht <- seq(57, 73, length.out = 100)
lines(ht, predict(mod.ns.2, data.frame(height = ht)), col="blue")
lines(ht, predict(mod.kub, data.frame(height = ht)), col="green")
abline(mod.lin, col="red")
abline(v=c(62.62, 67.38),lty=2, col="blue")

# VAJA Place
library(ISLR)
str(Wage)

# odvisnost wage od starost, modeliranje nelinearnosti
mod.1<- lm(wage~age ,data=Wage)
mod.2<- lm(wage~poly(age ,2) ,data=Wage)
mod.3<- lm(wage~poly(age ,3) ,data=Wage)
mod.4<- lm(wage~poly(age ,4) ,data=Wage)
mod.5<- lm(wage~poly(age ,5) ,data=Wage)
anova(mod.1, mod.2, mod.3, mod.4, mod.5) 

coef(summary(mod.4))   #primerjave so neodvisne 
# za primerjavo z neortogonalnimi polinomi
mod.4a=lm(wage~poly(age, 4, raw =T), data=Wage)
coef(summary (mod.4a))

#  napovedi
age.grid=seq (from=range(Wage$age)[1], to=range(Wage$age)[2])
napovedi<-predict(mod.4 ,newdata =list(age=age.grid), se=TRUE)
se.meje<-cbind(napovedi$fit + 2*napovedi$se.fit, napovedi$fit - 2*napovedi$se.fit)
 
plot(Wage$age, Wage$wage, xlim=range(Wage$age), cex =.5, col ="darkgrey")
lines(age.grid, napovedi$fit, lwd =2, col=" blue")
matlines(age.grid, se.meje, lwd =2, col=" blue",lty =2)  
library(effects)
plot(Effect("age", mod.4), ci.style="bands", xlab="Age",
     ylab="Wage", main="", ylim=c(0, 320))

# modeliranje z regresijo zlepkov
library(splines)
mod.bs<- lm(wage~bs(age, knots=c(25, 40, 60) ), data=Wage)
napovedi.bs <- predict(mod.bs, newdata =list(age=age.grid), se=T)
mod.ns<- lm(wage~ns(age, df = 4) ,data=Wage)
napovedi.ns<-predict(mod.ns, newdata =list(age=age.grid), se=T)

plot(Wage$age, Wage$wage, col ="gray")
lines(age.grid, napovedi.bs$fit ,lwd =2)
lines(age.grid, napovedi.bs$fit + 2*napovedi.bs$se, lty=2)
lines(age.grid, napovedi.bs$fit - 2*napovedi.bs$se, lty=2)
lines(age.grid, napovedi.ns$fit, col="red", lwd =2)
lines(age.grid, napovedi.ns$fit + 2*napovedi.ns$se, col="red", lty=2)
lines(age.grid, napovedi.ns$fit - 2*napovedi.ns$se, col="red", lty=2)

plot(Effect("age", mod.bs), ci.style="bands", xlab="Age",
            ylab="Wage", main="", ylim=c(0, 320))
plot(Effect("age", mod.ns), ci.style="bands", xlab="Age",
            ylab="Wage", main="", ylim=c(0, 320))
# grid.arrange(plot1, plot2, ncol=2)

ggplot(data=Wage, aes(x=age, y=wage)) + geom_point() +
  facet_grid(.~education) + geom_smooth(se=FALSE)

ggplot(data=Wage, aes(x=age, y=logwage)) + geom_point() +
  facet_grid(.~education) + geom_smooth(se=FALSE)

ggplot(data=Wage, aes(x=year, y=logwage)) + geom_point() +
  facet_grid(.~education) + geom_smooth(se=FALSE)

par(mar=c(4,9,4,2))
boxplot(logwage~education, data=Wage, las=2, xlab="log(wage)",ylab="", horizontal = T)

str(ns(Wage$age, df=4))
head(ns(Wage$age, df=4))

mod.place.1<-lm(logwage ~ ns(age, df=4) + year + education, data=Wage)
vif(mod.place.1)

summary(mod.place.1)

mod.place.2<-lm(logwage~ns(age, df=3)+year+education, data=Wage)
anova(mod.place.2,mod.place.1)
mod.place.4<-lm(logwage~ns(age, df=5)+year+education, data=Wage)
anova(mod.place.1,mod.place.4)

mod.place.2a<-lm(logwage~ns(age, df=3)+year, data=Wage)
anova(mod.place.2a,mod.place.2)

par(mfrow=c(2,2), oma=c(0,0,3,0))
plot(mod.place.2)
par(mfrow=c(1,1))

outlierTest(mod.place.1)
plot(Effect(c("age","education"), mod.place.2), 
     multiline=T, ci.style="bands", main="")

plot(Effect(c("year","education"), mod.place.2), 
     multiline=T, ci.style="bands", main="")

mod.place.2.int<-lm(logwage~ns(age, df=3)*education+year, data=Wage)
anova(mod.place.2, mod.place.2.int)
summary(mod.place.2.int)$r.squared

plot(Effect(c("age","education"), mod.place.2.int), 
     multiline=T, ci.style="bands", main="")

plot(Effect(c("year","education"), mod.place.2.int), 
     multiline=T, ci.style="bands", main="")