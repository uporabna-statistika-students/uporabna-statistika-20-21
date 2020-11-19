
# POSTAJE

postaje<-read.table("POSTAJE.txt", header=TRUE, sep="\t")
str(postaje)
head(postaje)
summary(postaje)
rownames(postaje)<-postaje$Postaja
rownames(postaje)[is.na(postaje$x.gdol)]
rownames(postaje)[is.na(postaje$y.gsir)]
library(car)
scatterplot(padavine~z.nv, regLine=F, smooth=FALSE, spread=FALSE,
            boxplots='xy',  xlab=c("Nadmorska vi�ina (m)"),
            ylab=c("Padavine (mm)"), data=postaje,  pch=16,
            id=list(n=2, location="lr"))
model.0 <- lm(padavine~z.nv, data=postaje)
par(mfrow = c(2, 2),mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.0)
par(mfrow = c(1, 1))
qqPlot(model.0, id=TRUE) 
# id=list(method="y", n=2, cex=1, col=carPalette()[1], location="lr")
outlierTest(model.0)
length(model.0$resid)*outlierTest(model.0)$p  ### to je Bonferronnijev p
plot(postaje$z.nv,hatvalues(model.0), pch=16, 
     xlab=c("Nadmorska vi�ina (m)"), ylab=c("Vzvod"))
h_povp<-mean(hatvalues(model.0))
abline(h=2*h_povp, lty=2, col=2)
abline(h=3*h_povp, lty=2, col=3)

influencePlot(model.0, id=list(method="y", n=2, cex=1, location="lr"), 
              xlab="Vzvodi",
              ylab="Studentizirani ostanki")

postaje.brez<-subset(postaje, subset=postaje$Postaja!="Kredarica")
model.brez<-lm(padavine~z.nv, data=postaje.brez)

par(mfrow = c(2, 2),mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.brez, id.n=2)

par(mfrow = c(1, 1))
qqPlot(model.brez, id=TRUE)

summary(model.brez)
confint(model.brez)

compareCoefs(model.0, model.brez)

scatterplot(padavine~z.nv, regLine=list(lty=2), smooth=FALSE, spread=FALSE,
            boxplots="none", xlab=c("Nadmorska vi�ina (m)"), 
            ylab=c("Padavine (mm)"), data=postaje,  pch=16, lwd=2, id=TRUE)
lines(postaje.brez$z.nv, model.brez$fitted, lwd=2, lty=1, col="blue") 
legend("bottomright", legend=c("s Kredarico", "brez Kredarice"),
       bty="n",lty=c(2,1),lwd=2, col=c("blue"))

postaje <- postaje.brez
# koordinate geografske dol�ine izrazimo v km
postaje$x<-postaje$x.gdol/1000 
scatterplot(padavine~x, regLine=F, smooth=FALSE, spread=FALSE,
            boxplots='xy',  xlab=c("Geografska dol�ina (km)"), 
            ylab=c("Padavine (mm)"), data=postaje, id=list(n=3), pch=16)
model1 <- lm(padavine~x, data=postaje)
par(mfrow = c(2, 2), oma=c(0,0,3,0))
plot(model1)

par(mfrow = c(1, 1))
symbox(~padavine, xlab= "Lambda", ylab="Transformirane vrednosti za padavine",
       data=postaje)
summary(powerTransform(model1))
boxCox(model1)

# KOVINE

kovine0<-read.table("KOVINE.txt", header=TRUE, sep="\t")
kovine0$razdalja<-kovine0$razdalja.m/1000
# izlo�imo vzor�ne to�ke z oddaljenostjo ve� kot 10 km
kovine<-kovine0[kovine0$razdalja<10,] 
dim(kovine)
summary(kovine[,c("Pb","razdalja")])

scatterplot(Pb~razdalja, regLine=F, xlab="Razdalja (km)",
            ylab="Pb (mg/kg)", smooth=FALSE, spread=FALSE,
            boxplots='xy', span=0.5, data=kovine, pch=16)

scatterplot(log(Pb)~razdalja, regLine=F, smooth=FALSE,
            xlab="Razdalja (km)", ylab="log(Pb)", spread=FALSE,
            boxplots='xy', span=0.5, data=kovine, pch=16)

model.Pb <- lm(log(Pb)~razdalja, data=kovine)
par(mfrow = c(2, 2),  oma = c(0, 0, 2, 0))
plot(model.Pb)

par(mfrow = c(1, 1))
influencePlot(model.Pb, id.n=2)
outlierTest(model.Pb)

summary(model.Pb)
confint(model.Pb)

library(ggplot2)
ggplot(data = kovine, aes(x = razdalja, y = log(Pb)))+ 
  geom_point() + 
  stat_smooth(method = "lm") + 
  xlab("Razdalja (km)") +
  ylab("log(Pb)")

ggplot(data = kovine, aes(x = razdalja, y = Pb)) + 
  geom_point() + 
  xlab("Razdalja (km)") + ylab("Pb (mg/kg)") + 
  stat_function(fun=function(x) exp(5.36427-0.21302*x))

# PELOD

pelod<-read.table("PELOD.txt",   header=TRUE)
str(pelod)
summary(pelod)
plot(pelod$Sevanje,pelod$Kalivost, pch=16, xlab="Sevanje (Gy)", ylab="Kalivost")
model.p0<-lm(Kalivost~Sevanje, data=pelod)
par(mfrow = c(2, 2),oma=c(0,0,3,0))
plot(model.p0)

par(mfrow = c(1, 1))
plot(pelod$Sevanje,asin(sqrt(pelod$Kalivost)), pch=16, 
     xlab="Sevanje (Gy)", ylab="asin(sqrt(Kalivost))")
model.p1<-lm(asin(sqrt(Kalivost))~Sevanje, data=pelod)
par(mfrow = c(2, 2),oma=c(0,0,3,0))
plot(model.p1)
par(mfrow = c(1, 1))