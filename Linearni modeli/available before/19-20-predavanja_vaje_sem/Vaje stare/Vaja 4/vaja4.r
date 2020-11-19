library(ISLR)
data(Carseats)
str(Carseats)
#zamenjamo vrstni red ravni faktorja ShelveLoc
Carseats$ShelveLoc<-factor(Carseats$ShelveLoc, levels=c("Bad","Medium","Good"))

library(ggplot2)
p1<-ggplot(data=Carseats, aes(x=Price, y=Sales)) +
  facet_grid(.~ShelveLoc) + geom_point() +
  xlab("Price (USD)")  +   ylab("Sales (1000)")
p2<-ggplot(data=Carseats, aes(x=Advertising, y=Sales)) +
  facet_grid(.~ShelveLoc) +  geom_point() +
  xlab("Advertising (1000 USD)")  +   ylab("Sales (1000)")

library(gridExtra)
grid.arrange(p1,p2, nrow=2, ncol=1)

p1<-p1+geom_smooth(se=FALSE)
p2<-p2+geom_smooth(se=FALSE)
grid.arrange(p1,p2, nrow=2, ncol=1)

model.stol<-lm(Sales~ Price + Advertising + ShelveLoc, data=Carseats)
par(mfrow=c(2,2),  oma = c(0, 0, 2, 0))
plot(model.stol)
par(mfrow=c(1,1))
model.matrix(model.stol)
library(car)
avPlots(model.stol, ylim=c(-10,7))

########
Carseats$x.1 <-model.matrix(model.stol)[,"ShelveLocMedium"]
Carseats$x.2 <-model.matrix(model.stol)[,"ShelveLocGood"]
e.y <- residuals(lm(Sales~ Price + Advertising + x.2, data=Carseats))
e.x1 <- residuals(lm(x.1~Price + Advertising + x.2, data=Carseats))
mod.e1 <- lm(e.y~e.x1)
(b.e1 <- coef(summary(mod.e1))[2,1])
(s.b.e1 <- coef(summary(mod.e1))[2,2])
# b in standardna napaka na podlagi polnega modela
(b <- coef(summary(model.stol))[4,1]);(s.b <- coef(summary(model.stol))[4,2])
###########

plot(e.x1, e.y)
abline(reg=mod.e1)

crPlots(model.stol)

library(effects)
plot(Effect(c("Advertising","Price", "ShelveLoc"), model.stol, partial.residuals=TRUE),
     ci.style="none")

anova(model.stol)

model.stol.a<-lm(Sales~ ShelveLoc + Price + Advertising, data=Carseats)
model.stol.0<-lm(Sales~ Price + Advertising, data=Carseats)
anova(model.stol.0, model.stol.a)  

summary(model.stol)$r.squared
stol.izpis<-glht(model.stol)
summary(stol.izpis)
confint(stol.izpis)

plot(Effect(c("Price", "ShelveLoc"), model.stol), main="", 
     layout=c(3,1), ylim=c(0, 17))
plot(Effect(c("Advertising", "ShelveLoc"), model.stol), main="", 
     layout=c(3,1), ylim=c(0, 17))

summary(model.stol)
C <- rbind(c(0,1,0,0,0),c(0,0,1,0,0),c(0,0,0,1,0),c(0,0,0,0,1), c(0,0,0,-1,1))
library(multcomp)
test<-glht(model.stol, linfct=C)
summary(test)
confint(test)

############################
# VAJA 4 SPANJE
##############################################
library(ggplot2)
spanje<-read.table("SLEEP.txt", header=T, sep="\t", na.string="NA", dec=".")
str(spanje)
head(spanje)
rownames(spanje)<-spanje$Species
spanje$logBodyWt<-log(spanje$BodyWt)

ggplot(data=spanje, aes(x=BodyWt, y=TotalSleep)) +
  facet_grid(.~Danger3) + geom_point() 

ggplot(data=spanje, aes(x=logBodyWt, y=TotalSleep)) +
  facet_grid(.~Danger3) + geom_point() 

ggplot(data=spanje, aes(x=logBodyWt, y=log(TotalSleep))) +
  facet_grid(.~Danger3) + geom_point() + geom_smooth(method="lm",se=FALSE)

mod.1 <- lm(log(TotalSleep) ~ Danger3 + logBodyWt, data=spanje)

library(effects)
plot(Effect(c("logBodyWt", "Danger3"), mod.1, partial.residuals=TRUE), span=0.8, layout=c(3,1))

mod.1.int <- lm(log(TotalSleep) ~ Danger3 * logBodyWt, data=spanje) # ali je potrebno vklju�iti interakcijo?
anova(mod.1, mod.1.int)
anova(mod.1.int)

par(mfrow=c(2,2),  oma = c(0, 0, 2, 0))
plot(mod.1.int)
par(mfrow=c(1,1))

library(car)
avPlots(mod.1.int)
plot(Effect(c("logBodyWt", "Danger3"), mod.1.int, partial.residuals=TRUE),
     span=0.9, layout=c(3,1))

outlierTest(mod.1.int)
summary(mod.1.int)$r.squared
coef(summary(mod.1.int))
# imamo 62 podatkov ocenjujemo 6 parametrov modela, ve� parametrov v modelu bi pomenilo preprileganje

C<-rbind(c(0,0,0,1,0,0), c(0,0,0,1,1,0), c(0,0,0,1,0,1), 
         c(0,0,0,0,1,0), c(0,0,0,0,0,1), c(0,0,0,0,-1,1))
rownames(C)<-c("naklon majhna", "naklon srednja", "naklon velika", 
               "naklon srednja-majhna", "naklon velika-majhna","naklon velika-srednja")
library(multcomp)
test<- glht(mod.1.int, linfct=C)
summary(test)
confint(test)
plot(Effect(c("logBodyWt","Danger3"), mod.1.int), multiline=TRUE, ci.style="bands", main="")


##### nelinearnost?#####
library(splines)
# modeliranje nelinearnost v tem primeru ni smiselno, ker imamo premalo podatkov, 
# �e bi hoteli uporabiti naravne zlepke z enim vozli��em (polinom tretje stopnje) 
# to pomeni ocenjevanje 15-tih parametrov
mod.1.int.nelin <- lm(log(TotalSleep) ~ Danger3 * ns(logBodyWt, df=4), data=spanje)
anova(mod.1.int, mod.1.int.nelin)
summary(mod.1.int.nelin)
# preve� ocenjenih parametrov v modelu


# Ali lahko model popravimo �e z ostalimi spremenljivkami v podatkovnem okviru?

spanje$logBrainWt<-log(spanje$BrainWt)
round(cor(spanje[,c("TotalSleep", "logBodyWt","logBrainWt","Gestation","LifeSpan")],
          use="complete", method = "spearman"), 2)
scatterplotMatrix(~TotalSleep +logBodyWt+logBrainWt+Gestation+LifeSpan,
                  regLine=FALSE,smooth=FALSE, data=spanje)
mod.1 <- lm(TotalSleep ~ Danger3 + logBrainWt + logBodyWt + Gestation + LifeSpan ,
            data=spanje)
vif(mod.1)
mod.2 <- lm(TotalSleep ~ Danger3 + logBodyWt + Gestation + LifeSpan, data=spanje)
vif(mod.2)
compareCoefs(mod.1, mod.2)

par(mfrow=c(2,2))
plot(mod.2)
par(mfrow=c(1,1))

crPlots(mod.2)

mod.2a <- lm(TotalSleep ~ logBodyWt + Gestation + LifeSpan, data=spanje)
anova(mod.2a, mod.2)

# za primerjavo modelov je potrebno izlo�iti manjkajoce podatke
mod.2b <- lm(TotalSleep ~  Danger3 + logBodyWt, data=na.omit(spanje))
anova(mod.2b, mod.2)