# GLS Modeliranje nekonstantne variance

# generiranje podatkov
# set.seed(777) # zaradi ponovljivosti
x<-sample(1:2500,100)
sim<-function(x){10+1.5*x+ rnorm(100,mean=0,sd=0.5*x)}
y<-sim(x)
primer1<-data.frame(x,y)
par(mfrow=c(1,1))
plot(x,y, pch=16)
mod1.lm<-lm(y~x, data=primer1)
par(mfrow=c(2,2))
plot(mod1.lm)
summary(mod1.lm)

# variančna funkcija varFixed iz paketa nlme (WLS)
library(nlme)

# določitev uteži
(vf<-varFixed(~x))
(vf<-Initialize(vf, primer1))
primer1$varW<-varWeights(vf) ### isto kot 1/sqrt(x)
head(primer1)

plot(x, varWeights(vf), pch=16)
mod1.gls1<-gls(y~x, weight=varFixed(~x), data=primer1, method="ML")
# mod1.lm1<-lm(y~x, weight=1/x, data=primer1)
summary(mod1.gls1)
plot(mod1.gls1, pch=16)


mod1.gls2<-gls(y~x, weight=varFixed(~x^2), data=primer1, method="ML")
# mod1.lm2<-lm(y~x, weight=1/x^2, data=primer1)
summary(mod1.gls2)

plot(mod1.gls2, pch=16)

anova(mod1.gls2, mod1.lm)

library(car)
compareCoefs(mod1.lm, mod1.gls2)
confint(mod1.lm)
intervals(mod1.gls2)

library(effects)
library(gridExtra)
plot1<-plot(Effect(c("x"), mod1.lm), ci.style="bands", main="")
plot2<-plot(Effect(c("x"), mod1.gls2), ci.style="bands", main="")
grid.arrange(plot1, plot2, ncol=2)

# modeliranje variance napak z varPower

mod1.gls3<-gls(y~x, weight=varPower(form=~x), method="ML")
summary(mod1.gls3)

par(mfrow=c(1,3))
plot(resid(mod1.gls3, type="p")~fitted(mod1.gls3), pch=16)
abline(h=0, lty=2)
plot(sqrt(abs(resid(mod1.gls3, type="p")))~fitted(mod1.gls3), pch=16)
lw1 <- loess(sqrt(abs(resid(mod1.gls3, type="p"))) ~ fitted(mod1.gls3))
j <- order(fitted(mod1.gls3))
lines(fitted(mod1.gls3)[j],lw1$fitted[j],col="red",lwd=3)

qqnorm(resid(mod1.gls3, type="p"), pch=16)
qqline(resid(mod1.gls3, type="p"), lty=2)

anova(mod1.gls2, mod1.gls3)
mod1.gls4<-gls(y~x, weight=varPower(form=~fitted(.)), method="ML")
summary(mod1.gls4)

par(mfrow=c(1,2))
plot(resid(mod1.gls4, type="p")~fitted(mod1.gls4), pch=16)
abline(h=0, lty=2)
qqnorm(resid(mod1.gls4, type="p"), pch=16)
qqline(resid(mod1.gls4, type="p"), lty=2)

compareCoefs(mod1.lm, mod1.gls2, mod1.gls3, mod1.gls4)

confint(mod1.lm)
intervals(mod1.gls4)

# modeliranje nekonstantne variance v primeru letne količine padavin v Sloveniji

data<-read.table("POSTAJE.txt", header=TRUE, sep="\t")
rownames(data)<-data$Postaja
data.brez<-subset(data, subset=data$Postaja!="Kredarica")
data64<-na.omit(data.brez)  ### upoštevajo se samo tisti zapisi, ki so brez NA
data64$x<-data64$x.gdol/1000
data64$y<-data64$y.gsir/1000

model.m2<-lm(padavine~z.nv*x, data=data64)
summary(model.m2)

par(mfrow=c(2,2),  oma = c(0, 0, 2, 0))
plot(model.m2)
par(mfrow=c(1,1))
plot(data64$x, residuals(model.m2))
abline(h=0)

model.m2.gls<-gls(padavine~z.nv*x, weight=varPower(form=~fitted(.)),
                  method="ML",data=data64)
anova(model.m2.gls, model.m2)

# plot(model.m2.gls, pch=16)
plot(fitted(model.m2),rstandard(model.m2))
points(fitted(model.m2.gls), resid(model.m2.gls, type="p"), col="red", pch=17)
legend(2500, 2.5, legend=c("model.m2","model.m2.gls"),
       pch=c(1,17), col=(1:2), box.lty = 1)
abline(h=0, lty=2)

summary(model.m2.gls)
compareCoefs(model.m2,model.m2.gls)
library(multcomp)
confint(glht(model.m2))  # glht na gls modelu

confint(glht(model.m2.gls))
plot2<-plot(Effect(c("x", "z.nv"), model.m2, xlevels=list(z.nv=c(250, 500, 750, 1500))),
            rows=1, cols=1, main="", layout=c(4,1), ylim=c(-2000, 4000))
plot1<-plot(Effect(c("z.nv","x"), model.m2, xlevels=list(x=c(390,460,540,610))),
            rows=1, cols=1, main="", layout=c(4,1), ylim=c(-2000, 4000))
grid.arrange(plot1, plot2, nrow=2)

plot2<-plot(Effect(c("x", "z.nv"), model.m2.gls,
                   xlevels=list(z.nv=c(250, 500, 750, 1500))),
            rows=1, cols=1, main="", layout=c(4,1), ylim=c(-2000, 4000))
plot1<-plot(Effect(c("z.nv","x"), model.m2.gls, xlevels=list(x=c(390,460,540,610))),
            rows=1, cols=1, main="", layout=c(4,1), ylim=c(-2000, 4000))
grid.arrange(plot1, plot2, nrow=2)

# varIdent

# generiranje podatkov
set.seed(777) # zaradi ponovljivosti
n=20
ya<-rnorm(n,2,1)
yb<-rnorm(n,7,3)
yc<-rnorm(n,6,5)
y<-c(ya,yb,yc)
skupina<-rep(c("A","B","C"),each=n)
primer2<-data.frame(skupina,y)

boxplot(y~skupina,data=primer2, xlab="Skupina", ylab="y")
mod2.lm<-lm(y~skupina, data=primer2)
summary(mod2.lm)

par(mfrow=c(2,2), oma=c(0,0,2,0))
plot(mod2.lm)
par(mfrow=c(1,1))
library(nlme)
mod2.gls1<-gls(y~skupina, weight=varIdent(form=~1|skupina), method="ML")
summary(mod2.gls1)

plot(mod2.gls1, pch=16, ylim=c(-3, 3))

anova(mod2.gls1,mod2.lm)
intervals(mod2.gls1) # izračun intervalov zaupanja za parametre gls modela
library(car)
compareCoefs(mod2.lm, mod2.gls1)

library(multcomp)
C<-rbind(c(0,1,0), c(0,0,1),c(0,-1,1))
rownames(C)<-c("B-A", "C-A", "C-B")
test<-glht(mod2.gls1, linfct=C)
confint(test)
test.lm<-glht(mod2.lm, linfct=C)
confint(test.lm)
library(effects)
p1 <-plot(Effect(c("skupina"), mod2.lm), ci.style="bars", lty=0, main="", ylim=c(0, 10))
p2 <-plot(Effect(c("skupina"), mod2.gls1), ci.style="bars", lty=0, main="", ylim=c(0, 10))
library(gridExtra)
grid.arrange(p1, p2, ncol=2)


# Modeliranje koreliranosti napak
# LESKA
leska<-read.table("LESKA.txt", header=T)
str(leska)
library(car)
scatterplot(cvet.dan~temp, data=leska, pch=16, smooth=F, regLine=F,
            xlab="Povpre�na temperatura zraka (�C)", ylab="Dan za�etka cvetenja leske")
scatterplot(cvet.dan~temp, data=leska, pch=16, smooth=F, regLine=F,
            xlab="Povpre�na temperatura zraka (�C)", ylab="Dan za�etka cvetenja leske")
mod.lm<-lm(cvet.dan~temp, data=leska)
par(oma=c(0,0,2,0), mfrow=c(2,2))
plot(mod.lm)
par(mfrow=c(1,1))
plot(leska$leto, residuals(mod.lm), type="b",pch=16, xlab="Leto", ylab="Ostanki")
abline(h=0, col="grey")

(DWT1<-durbinWatsonTest(mod.lm, max.lag=5))

acf(residuals(mod.lm), plot=FALSE)
pacf(residuals(mod.lm), plot=FALSE)

par(oma=c(0,0,2,0), mfrow=c(1,2))
acf(residuals(mod.lm), main="")
pacf(residuals(mod.lm), main="")

#  AR(1)
library(nlme)
mod.gls.ar<-gls(cvet.dan~temp, correlation=corARMA(p=1), data=leska, method="ML")
anova(mod.gls.ar, mod.lm)
summary(mod.gls.ar)

par(oma=c(0,0,2,0), mfrow=c(1,1))
acf(residuals(mod.gls.ar, type="p"), main="")
pacf(residuals(mod.gls.ar, type="p"), main="")
acf(residuals(mod.gls.ar, type="n"), main="")
pacf(residuals(mod.gls.ar, type="n"), main="")

# primerjava standardiziranih in normaliziranih ostankov
plot(leska$leto, residuals(mod.gls.ar, type="n"), type="b",pch=16, xlab="Leto",
     ylab="Ostanki", col="black", ylim=c(-2.5,3))
points(leska$leto, residuals(mod.gls.ar, type="p"), type="b",pch=2, xlab="Leto",
       ylab="Ostanki", col="darkgrey")
legend(x=1965, y=3.5, horiz=T, c("normalizirani", "standardizirani"), box.lty=0, pch=c(16,2),
       col=c("black", "darkgrey"))
abline(h=0, col="grey")

#  MA(1)

mod.gls.ma<-gls(cvet.dan~temp, correlation=corARMA(q=1), data=leska, method="ML")
anova(mod.gls.ma, mod.lm)
anova(mod.gls.ma, mod.gls.ar)
summary(mod.gls.ma)

compareCoefs(mod.gls.ar, mod.gls.ma, mod.lm)

library(effects)
library(gridExtra)
plot1<-plot(effect(c("temp"), mod.lm), ci.style="bands", main="")
plot2<-plot(effect(c("temp"), mod.gls.ar), ci.style="bands", main="")
grid.arrange(plot1, plot2, ncol=2)

intervals(mod.gls.ar)


# Hartnagel
data(Hartnagel)
str(Hartnagel)

# grafični prikaz časovnih vrst

plot(cbind(fconvict=ts(Hartnagel$fconvict, start=1931),
           tfr=ts(Hartnagel$tfr, start=1931),
           partic=ts(Hartnagel$partic, start=1931),
           degrees=ts(Hartnagel$degrees, start=1931),
           mconvict=ts(Hartnagel$mconvict, start=1931)), nc=1, main="")

pairs(Hartnagel[,c("fconvict", "tfr", "partic", "degrees", "mconvict")])

mod.H.lm<-lm(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel)
vif(mod.H.lm)
durbinWatsonTest(mod.H.lm, max.lag=5)

par(oma=c(0,0,2,0), mfrow=c(2,2))
plot(mod.H.lm)

par(mfrow=c(1,2))
acf(residuals(mod.H.lm), main="")
pacf(residuals(mod.H.lm), main="")

mod.H.gls<-gls(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel,
               weight=varPower(form=~fitted(.)), method="ML")
anova(mod.H.gls, mod.H.lm)

par(mfrow=c(1,2))
plot(resid(mod.H.gls, type="n")~fitted(mod.H.gls), pch=16)
abline(h=0, lty=2)
qqnorm(resid(mod.H.gls, type="n"), pch=16)
qqline(resid(mod.H.gls, type="n"), lty=2)

par(mfrow=c(1,2))
acf(residuals(mod.H.gls), main="")
pacf(residuals(mod.H.gls), main="")

mod.H.gls1<-gls(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel,
                weight=varPower(form=~fitted(.)),
                correlation=corARMA(p=1, q=1), method="ML")
mod.H.gls2<-gls(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel,
                weight=varPower(form=~fitted(.)),
                correlation=corARMA(p=2), method="ML")

anova(mod.H.gls1, mod.H.gls2)

par(mfrow=c(1,2))
plot(resid(mod.H.gls1, type="n")~fitted(mod.H.gls1), pch=16)
abline(h=0, lty=2)
qqnorm(resid(mod.H.gls1, type="n"), pch=16)
qqline(resid(mod.H.gls1, type="n"), lty=2)


par(mfrow=c(1,2))
acf(residuals(mod.H.gls1, type="n"), main="")
pacf(residuals(mod.H.gls1, type="n"), main="")

intervals(mod.H.gls1)
library(multcomp)
confint(glht(mod.H.gls1))

plot1<-plot(Effect(c("tfr"), mod.H.gls1), ci.style="bands",
            main="", ylim=c(50,140))
plot2<-plot(Effect(c("partic"), mod.H.gls1), ci.style="bands",
            main="", ylim=c(50,140))
plot3<-plot(Effect(c("degrees"), mod.H.gls1), ci.style="bands",
            main="", ylim=c(50,140))
plot4<-plot(Effect(c("mconvict"), mod.H.gls1), ci.style="bands",
            main="", ylim=c(50,140))
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
