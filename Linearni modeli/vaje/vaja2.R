
# PACIENTI
pacienti<-read.table("PACIENTI.TXT", header=TRUE, sep="\t")
str(pacienti)
summary(pacienti)
pairs(pacienti)
model.p<-lm(SKT ~ starost + masa, data=pacienti)
coef(summary(model.p))[1,1]
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
plot(model.p)
summary(model.p)

###############
n <- 20
beta_0 <- coef(summary(model.p))[1,1]
beta_1 <- coef(summary(model.p))[2,1]
beta_2 <- coef(summary(model.p))[3,1]
x1 <- pacienti$starost
x2 <- pacienti$masa
y <- beta_0 + beta_1*x1+beta_2*x2 + rnorm(n, 0, 0.5327)

mod <- lm(y~x1+x2)
par(mfrow=c(2,2))
plot(mod)
###############

# izračuni za grafikon dodane spremenljivke

e.y <- residuals(lm(SKT~starost, data=pacienti))
e.x <- residuals(lm(masa~starost, data=pacienti))
mod.e <- lm(e.y~e.x)
(b.e <- coef(summary(mod.e))[2,1])
(s.b.e <- coef(summary(mod.e))[2,2])

par(mfrow = c(1,1))
plot(e.x, e.y)
abline(reg=mod.e)

library(car)
avPlots(model.p, ylim=c(-10, 10))
crPlots(model.p, ylim=c(-10, 10))

summary(model.p)$r.squared
model.p$coeff
confint(model.p)

library(ellipse);
plot(ellipse(model.p, which=c(2,3)), type="l",
     xlab=expression(beta[1]), ylab=expression(beta[2]))
abline(v=confint(model.p)[2,], h=confint(model.p)[3,], lty=2, col="red")
points(model.p$coef[2],model.p$coef[3], pch=16, col="blue")

vcov(model.p)
n <- length(model.p$residuals)
b <- model.p$coef
k <- length(b)-1
(s2 <- sum(model.p$residuals^2)/(n-k-1))
X <- model.matrix(model.p)
head(X)
(A <- solve(t(X) %*% X))
(s2*A)

library(effects)
plot(predictorEffects(model.p, ~.), ylim=c(105,125),main="")

plot(predictorEffects(model.p, ~., partial.residuals=TRUE),
     ci.style="none", ylim=c(105,125),main="")

vrednosti<-data.frame(starost=50, masa=100)
povp.napoved<-predict(model.p, vrednosti, interval="confidence")
pos.napoved<-predict(model.p, vrednosti, interval="prediction")
print(data.frame(cbind(vrednosti, povp.napoved, pos.napoved)))


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
# koordinate geografske dolžine izrazimo v km
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
# izločimo vzorčne točke z oddaljenostjo več kot 10 km
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

