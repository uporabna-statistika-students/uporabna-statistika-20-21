# ENA OPISNA SPREMENLJIVKA V lm MODELU

tlak<-read.table(file="SKT.txt", header = TRUE, stringsAsFactors = TRUE)
str(tlak)
levels(tlak$spol)
boxplot(SKT~spol, data=tlak, ylab=c("SKT (mm)"), xlab="Spol")
model.spol<-lm(SKT ~ spol, data=tlak)
tlak$spol
X<-model.matrix(model.spol)
X[1:5,]
X[38:42,]
par(mfrow = c(2, 2), oma=c(0,0,3,0))
plot(model.spol)
summary(model.spol)$coeff
summary(model.spol)$r.squared
confint(model.spol)

t.test(SKT~spol, alternative='two.sided', conf.level=.95, var.equal=TRUE,
       data=tlak)

# opisna spremnljivka z več vrednostmi

tlak$skupina<-factor(rep(c("A", "B","C"), times=c(20,20,29)))
tlak$skupina
tapply(tlak$SKT, tlak$skupina,  mean, na.rm=TRUE)
boxplot(SKT~skupina, data=tlak, ylab=c("SKT (mm)"), xlab="Skupina")
model.vec<-lm(SKT~skupina, data=tlak)
X<-model.matrix(model.vec)
X[18:23,]
X[38:43,]
anova(model.vec)

summary(model.vec)$coeff
confint(model.vec)

# izračun korelacijske matrike t-statistik pri hkratnem testiranju ničelnih domnev

b <- coefficients(model.vec)
k <- length(b)-1
round(b, 3) # ocene parametrov v lm modelu
varb<-vcov(model.vec); round(varb, 3) # variančno-kovariančna matrika ocen parametrov
C<-diag(3)  # matrika enostavnih primerjav
rownames(C)<-c("beta0 = 0","beta1 = 0", "beta2 = 0")
colnames(C)<-c("c0","c1","c2");C
sqrt(diag(C %*% varb %*% t(C))) # vektor ocen standardnih napak ocen parametrov
D<-diag(1/sqrt(diag(C %*% varb %*% t(C)))); D
t<-D %*% C %*% b; t  # t- statistike za 3 ničelne domneve
R<-D %*% C %*% varb %*% t(C) %*% t(D); R  #korelacijska matrika t-statistik
n <- length(tlak$SKT)
df<- n - k - 1; df # stopinje prostosti ostanka
library(mvtnorm)
# p-vrednosti izračunane po multivariatni t-porazdelitvi
# numeriča integracija (Genz in Bretz, 2009)
p.mvt1<-sapply(abs(t),
               function(x) {1 - pmvt(-rep(x, 3), rep(x, 3),
                                     delta=rep(0, 3), corr = R, df = df)})
round(p.mvt1,4)

# popravljene p-vrednosti za lm model s funkcijo glht
library(multcomp)
test.0<-glht(model.vec)
summary(test.0)
confint(test.0)

# matrika primerjav
C1<-rbind(c(0, 1, 0), c(0, 0, 1), c(0, -1, 1))
rownames(C1)<-c("mu_B-mu_A","mu_C-mu_A", "mu_C-mu_B")
colnames(C1)<-c("beta0","beta1","beta2");C1
test.1<-glht(model.vec,linfct=C1)
summary(test.1)
confint(test.1)


library(ggplot2)
ggplot(data=tlak, aes(x=starost, y=SKT, col=spol)) +
    geom_point() + xlab("Starost (leta)") + ylab("SKT (mm)")


# ena opisna in ena številska spremenljivka

# v modelu ni interakcije

model.vzporedni <- lm(SKT ~ spol + starost, data=tlak)
X<-model.matrix(model.vzporedni)
X[18:21,] # za ilustracijo
X[39:42,]
par(mfrow = c(2, 2),mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.vzporedni)
summary(model.vzporedni)
confint(model.vzporedni)
test.vzporedni<-glht(model.vzporedni)
summary(test.vzporedni)
confint(test.vzporedni)
library(effects)
plot(Effect(c("starost", "spol"), model.vzporedni),
     multiline=T, ci.style="bands",
     key.args=list(x=0.05, y=0.8, corner=c(0,0)),
     main="", lty=c(1:2))

# napovedi za izbrane vrednosti napovednih spremenljivk starost in spol:
n1<-max(tlak$starost)- min(tlak$starost)+1
x <-seq(from=min(tlak$starost), to=max(tlak$starost), by=1)
nap.x <- data.frame(starost = rep(x, times=2),
                    spol = rep(c("m","z"), each=n1))
mod<-model.vzporedni
# interval zaupanja za povprečno napoved
conf.int <- cbind(nap.x, predict(mod, nap.x, interval="confidence", level=0.95))
# interval zaupanja za posamično napoved
pred.int <- cbind(nap.x, predict(mod, nap.x, interval="prediction", level=0.95))
library(ggplot2)
p0 <- ggplot(data=tlak, mapping=aes(x=starost, y=SKT, colour=spol)) +
    ggtitle("IZ za povprečno napoved, lm") + geom_point() +
    geom_smooth(method="lm", se=TRUE) +
    xlab("Starost (leta)") + ylab("SKT (mm Hg)")
p1 <- ggplot(conf.int, aes(x = starost, y = fit, col=spol)) +
    ggtitle("IZ za povprečno napoved, predict()") +
    geom_point(data = tlak, aes(x = starost, y = SKT, col=spol)) +
    geom_smooth(data = conf.int, aes(ymin = lwr, ymax = upr), stat = "identity") +
    xlab("Starost (leta)") + ylab("SKT (mm Hg)")
p2 <- ggplot(pred.int, aes(x = starost, y = fit, col=spol)) +
    ggtitle("IZ za posamično napoved, predict()") +
    geom_point(data = tlak, aes(x = starost, y = SKT, col=spol)) +
    geom_smooth(data = pred.int, aes(ymin = lwr, ymax = upr), stat = "identity") +
    xlab("Starost (leta)") + ylab("SKT (mm Hg)")
library(gridExtra)
grid.arrange(p1,p2,p0, ncol=2)

# vrstni red napovednih spremenljivk

model.vzporedni.a<-lm(SKT~starost+spol, data=tlak)
coefficients(model.vzporedni.a)

# centriranje napovedne spremenljivke

tlak$starost.50<-tlak$starost-50
model.vzporedni.50 <- lm(SKT ~ spol + starost.50, data=tlak)
test.vzporedni.50<-glht(model.vzporedni.50)
summary(test.vzporedni.50)
confint(test.vzporedni.50)

# inerakcija opisne in številske spremenljivke v modelu

model.razlicni <- lm(SKT ~ spol*starost.50, data=tlak)
X<-model.matrix(model.razlicni)
X[18:21,]
X[39:42,]
par(mfrow = c(2, 2),mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.razlicni)
summary(model.razlicni)
test.razlicni<-glht(model.razlicni)
summary(test.razlicni)
confint(test.razlicni)
plot(Effect(c("starost.50", "spol"), model.razlicni),
     multiline=T, ci.style="bands",
     key.args=list(x=0.05, y=0.8, corner=c(0,0)),
     main="", lty=c(1:2))

# opisna spremenljivka ima več kot dve vrednosti, vzporedne premice

model.vzporedne<-lm(SKT~skupina+starost.50, data=tlak)
X<-model.matrix(model.vzporedne)
X[18:23,]
X[38:43,]
test.vzporedne<-glht(model.vzporedne)
summary(test.vzporedne)
confint(test.vzporedne)
plot(Effect(c("starost.50", "skupina"), model.vzporedne), multiline=T, ci.style="bands",
     key.args=list(x=0.05, y=0.8, corner=c(0,0)),  main="", lty=c(1:3))
C2<-rbind(c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, -1, 1, 0), c(0, 0, 0, 1))
colnames(C2)<-c("beta0","beta1","beta2","beta3")
rownames(C2)<-c("povp B|starost - povp A|starost",
                "povp C|starost - povp A|starost",
                "povp C|starost - povp B|starost",
                "naklon"); C2
test.vzporedne.2<-glht(model.vzporedne, linfct=C2)
summary(test.vzporedne.2)
confint(test.vzporedne.2)

# interakcija

model.razlicne<-lm(SKT~skupina*starost.50, data=tlak)
# X<-model.matrix(model.razlicne)
# X[18:21,]
# X[39:42,]
test.razlicne<-glht(model.razlicne)
summary(test.razlicne)
confint(test.razlicne)
nic<-c(0,0,0)
C3a<-rbind(c(nic, 1, 0, 0), c(nic, 1, 1, 0), c(nic,1, 0, 1))
rownames(C3a)<-c("naklon_A", "naklon_B", "naklon_C")
colnames(C3a)<-c("beta0","beta1","beta2","beta3","beta4","beta5"); C3a
test.3a<-glht(model.razlicne, linfct=C3a)
summary(test.3a)
