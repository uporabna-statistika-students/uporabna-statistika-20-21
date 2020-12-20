# POLINOMSKA REGRESIJA

koruza<-read.table(file="KORUZA.txt", header = TRUE)
str(koruza)
summary(koruza[,c("gostsetve", "prid.ha")])
koruza$prid1.ha<-koruza$prid.ha/1000
koruza$blok <- factor(koruza$blok)  # gre za poskusno zasnovo slučajni bloki

library(ggplot2)
ggplot(data=koruza, aes(x=gostsetve, y=prid1.ha, col=blok)) +
    geom_point() + geom_smooth(se=FALSE) +
    ylab("Pridelek (t/ha)") +
    xlab("Gostota setve")

model.lin <- lm(prid1.ha ~ gostsetve, data=koruza)
# Ali bloki pojasnijo kaj variabilnosti odzivne spremenljivke?
model.lin.blok <- lm(prid1.ha ~ blok + gostsetve, data=koruza)
anova(model.lin, model.lin.blok)


par(mfrow = c(2, 2), mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.lin)
model.kvad <- lm(prid1.ha ~ gostsetve + I(gostsetve^2), data=koruza)
summary(model.kvad)
# enak rezulat dobimo z uporabo funkcije poly
model.kvad.1 <- lm(prid1.ha ~ poly(gostsetve, degree=2, raw=TRUE), data=koruza)

library(car)
vif(model.kvad)  # v takem kontekstu je vif neuporaben, vedno so vrednosti velike
vif(model.kvad.1)  #  v tem primeru se izračuna gvif, če bi bili v modelu še drugi regresorji

anova(model.kvad) # sekvenčni F-testi

par(mfrow = c(2, 2),mar=c(4,4,2,2), oma=c(0,0,3,0))
plot(model.kvad)
coefficients(model.kvad)
summary(model.kvad)$r.squared

library(effects)
plot(Effect(c("gostsetve"), model.kvad, xlevels=list(gostsetve=seq(0, 172, 2))),
     ci.style="bands", xlab="Gostota setve",ylab="Pridelek (t/ha)",
     main="", ylim=c(0,8))

# Optimalna gostota setve

opt<--0.5*coefficients(model.kvad)[2]/coefficients(model.kvad)[3]
cat("Optimalna gostota =", opt)

# Napoved in interval zaupanja za pridelek pri optimalni gostoti
gostsetve.x<-data.frame(gostsetve=opt)
rownames(gostsetve.x) <-NULL
povp.napoved.pridelek<-predict(model.kvad,gostsetve.x, interval="confidence")
round(data.frame(cbind(gostsetve.x,povp.napoved.pridelek)), 2)

# Samovzorčenje primerov (neparametrični bootstrap)

R <- 1000
b <- data.frame()
for (i in c(1:R)){
    vzorec <- sample(c(1:dim(koruza)[1]),replace=TRUE)
    koruza1<-koruza[vzorec,]
    b <- rbind(b, coefficients(lm(prid1.ha ~ gostsetve + I(gostsetve^2), data=koruza1)))
}
names(b) <-c("b0","b1","b2")
b$opt <- -b$b1/(2*b$b2)
mean(b$opt)  # povprečje generiranih optimumov
(bias <- as.numeric(opt-mean(b$opt)))  # pristranskost bootstrap ocene za optimalno gostoto
(optIZ<-quantile(b$opt,c(0.025,0.975)) ) # centilni interval zaupanja za optimum

par(mfrow=c(1,2))
boxplot(b$opt, ylab="Bootstrap optimumi", ylim=c(100, 160))
abline(h=opt, lty=2, col="red"); abline(h=opt, lty=2, col="black");
abline(h=optIZ, lty=2, col="blue")
hist(b$opt, ylim=c(0, 500), xlim=c(100, 160),
     xlab="Bootstrap optimumi",  main="", ylab="število vzorcev")
abline(v=mean(b$opt), lty=2, col="red");
abline(v=opt, lty=2, col="black");
abline(v=optIZ, lty=2, col="blue")

# Bootstrap modela s funkcijo Boot iz paketa car

library(car)
betahat.boot<-Boot(model.kvad, R=1000,  f = coef, method = "case")
summary(betahat.boot)
head(betahat.boot$t)

betahat.boot$t$opt<--0.5*betahat.boot$t[,2]/betahat.boot$t[,3]
bootIZ<-quantile(betahat.boot$t$opt,c(0.025,0.975))
round(cbind(mean(betahat.boot$t$opt),t(bootIZ)), 2)

# še druga možnost uporabe funkcije Boot za bootstrap optimume:

betahat.boot.1<-Boot(object=model.kvad, R=1000, labels="optimum",
                     f = function(object) -0.5*coef(object)[2]/coef(object)[3])
summary(betahat.boot.1)
opt.bootstrap<-as.numeric(summary(betahat.boot.1)[2]+summary(betahat.boot.1)[3])
round(opt.bootstrap,2)
confint(betahat.boot.1, type="perc")
par(mfrow=c(1,1))
hist(betahat.boot.1, ci="perc",, xlim=c(100, 160))

# REGRESIJA ZLEPKOV

library(splines)

bs.0<-bs(koruza$gostsetve) #  df=NULL, knots=NULL, degree=3
str(bs.0)
head(bs.0)
#  df=4, knots=NULL
bs.1<-bs(koruza$gostsetve, df=4) #  knots=1, degree=3
str(bs.1)
head(bs.1)


# določimo vrednosti gostsetve za vozlišča
vozl<-quantile(koruza$gostsetve, probs = c(0.25, 0.5, 0.75), na.rm=T)
bs.3<-bs(koruza$gostsetve, knots=vozl, degree=3)
# enakovreden ukaz je
# bs.3<-bs(koruza$gostsetve,df=6)
str(bs.3)
# vrednosti baznih funkcij zlepkov polinomov tretje stopnje s tremi vozlišči
# za dane vrednosti gostsetve
head(bs.3)

par(mfrow=c(2,dim(bs.3)[2]/2))
x<-seq(min(koruza$gostsetve), max(koruza$gostsetve),1 )
for (i in 1:dim(bs.3)[2])
{plot(koruza$gostsetve,bs.3[,i], pch=16, ylim=c(0,1.1),
      ylab=paste("bs.3[,", i,"]"))
    lines(x, bs(x, knots=vozl)[,i])
    abline(v=vozl, col="blue", lty=2)}


# model regresije kvadratnih zlepkov brez vozlišč
model.bs2.0<-lm(prid1.ha ~ bs(gostsetve, degree=2), data=koruza)
coef(summary(model.bs2.0))
# za primerjavo izpišimo ocene parametrov polinomske regresije
coef(summary(model.kvad))
# še polinomska regresija z baznimi funkcijami,
# ki predstavljajo ortogonalne kvadratne polinome regresorjev,
# degree=2, raw=FALSE
model.kvad.1 <-lm(prid1.ha ~ poly(gostsetve, 2), data=koruza)
coef(summary(model.kvad.1))
# koeficienti determinacije za vse tri modele
summary(model.kvad)$r.squared
summary(model.kvad.1)$r.squared
summary(model.bs2.0)$r.squared

head(model.matrix(model.kvad))
head(model.matrix(model.kvad.1))
head(model.matrix(model.bs2.0))

# Grafični prikaz napovedi

novi.x<-data.frame(gostsetve=seq(0, 180, 5))
par(mfrow=c(1,1))
plot(koruza$gostsetve,koruza$prid1.ha, pch=16,
     ylab="Pridelek (t/ha)", xlab="Gostota setve",)
lines(novi.x$gostsetve, predict(model.kvad, novi.x), lwd=2, col="black")
lines(novi.x$gostsetve, predict(model.kvad.1, novi.x),lwd=2, col="red")
lines(novi.x$gostsetve, predict(model.bs2.0, novi.x),lwd=2, col="blue")
legend(100, 2.5, legend=c("model.kvad","model.kvad.1","model.bs2.0"),
       col=c("black","red","blue"), lwd=2, lty=1)

anova(model.kvad, model.bs2.0)

model.bs.3<-lm(prid1.ha ~ bs(gostsetve, knots=vozl), data=koruza)
model.ns.3<-lm(prid1.ha ~ ns(gostsetve, knots=vozl), data=koruza)
coef(summary(model.bs.3))
coef(summary(model.ns.3))

anova(model.kvad, model.bs.3)
anova(model.kvad, model.ns.3)

novi.x<-data.frame(gostsetve=seq(0, 180, 5))
plot(koruza$gostsetve,koruza$prid1.ha, pch=16,
     ylab="Pridelek (t/ha)", xlab="Gostota setve",)
lines(novi.x$gostsetve, predict(model.kvad, novi.x), lwd=2, col="black")
lines(novi.x$gostsetve, predict(model.bs.3, novi.x),lwd=2, col="blue")
lines(novi.x$gostsetve, predict(model.ns.3, novi.x),lwd=2, col="red")
legend(105, 2.5, legend=c("model.kvad","model.bs.3","model.ns.3"),
       col=c("black","blue","red"), lwd=2, lty=1)

plot1<-plot(Effect(c("gostsetve"), model.kvad, xlevels=list(gostsetve=seq(0, 172, 2))),
            ci.style="bands", xlab="Gostota setve",ylab="Pridelek (t/ha)",
            main="", ylim=c(0,8))

# modelu model.bs.3 določimo df, ker samo na tako definiranem modelu lahko uporabimo funkcijo Effect
model.bs.3a<-lm(prid1.ha ~ bs(gostsetve, df=6), data=koruza)
plot2<-plot(Effect(c("gostsetve"), model.bs.3a, xlevels=list(gostsetve=seq(0, 172, 2))),
            ci.style="bands", xlab="Gostota setve",ylab="Pridelek (t/ha)",
            main="", ylim=c(0,8))
model.ns.3a<-lm(prid1.ha ~ ns(gostsetve, df=4), data=koruza)
plot3<-plot(Effect(c("gostsetve"), model.ns.3a, xlevels=list(gostsetve=seq(0, 172, 2))),
            ci.style="bands", xlab="Gostota setve",ylab="Pridelek (t/ha)",
            main="", ylim=c(0,8))
library(gridExtra)
grid.arrange(plot1, plot2,plot3, ncol=3)

