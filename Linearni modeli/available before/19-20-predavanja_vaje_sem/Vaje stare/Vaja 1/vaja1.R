tlak <- read.table("SKT.txt", header=TRUE, sep="\t")
head(tlak)
str(tlak)
summary(tlak)

library(ggplot2)
ggplot(data=tlak) +
  geom_point(mapping=aes(x=starost, y=SKT)) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")

model.SKT <- lm(SKT~starost, data=tlak)
names(model.SKT)
model.SKT$coeff
model.SKT$fitted.values

ggplot(data=tlak, mapping=aes(x=starost, y=SKT)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")

par(mfrow=c(2,2))
plot(model.SKT)
par(mfrow=c(1,1))
# peš izraèun standardiziranih ostankov iin kvantilov standardizirane normalne porazdelitve

e <- model.SKT$residuals

# vektor vzvodov
h <- hatvalues(model.SKT)

# standardna napaka regresije
s <- sqrt(sum(e^2)/(length(e)-2)) 

# standardizirani ostanki
e.stand <- e/(s*sqrt(1-h))
summary(e.stand)

# kvantili standardizirane normalne porazdelitve od 1/69 do 1 po 1/69
q <- qnorm(seq(from=1/69, to=1, by=1/69), mean=0, sd=1)

# Q-Q graf, kvantilni grafikon
plot(q,sort(e.stand))
abline(a=0,b=1, lty=2)
abline(h=0, lty=2, col="grey")

#############################################################

# povzetek modela
summary(model.SKT)

# peš izraèun koeficienta determinacije
SS_model<-sum((model.SKT$fitted-mean(tlak$SKT))^2);SS_model
SS_res<-sum(model.SKT$residual^2);SS_res
R2<-SS_model/(SS_model+SS_res); R2

# varianèno kovarianèna matrika ocen parametrov
vcov(model.SKT)
# standardne napake ocen parametrov
sqrt(diag(vcov(model.SKT)))

# t-test peš
t.b0<-model.SKT$coef[1]/sqrt(vcov(model.SKT)[1,1]); t.b0
t.b1<-model.SKT$coef[2]/sqrt(vcov(model.SKT)[2,2]); t.b1

#tabela anova za model
anova(model.SKT)

# intervali zaupanja za parametre
confint(model.SKT) 


# napovedovanje na osnovi modela

# izbrane vrednosti napovedne spremenljivke
starost.napovedi<-data.frame(starost=c(30,60))

# povpreène napovedi
povp.napovedi.SKT<-predict(model.SKT, starost.napovedi, interval="confidence")
data.frame(cbind(starost.napovedi,povp.napovedi.SKT ))

# posamiène napovedi
pos.napovedi.SKT<-predict(model.SKT, starost.napovedi, interval="prediction")
data.frame(cbind(starost.napovedi,pos.napovedi.SKT ))

##################################################################
# SIMULACIJA
################################################################

# vrednosti za starost vzamemo iz podatkovnega okvira tlak
starost<-tlak$starost

# velikost vzorca
n<-length(tlak$starost)

# standardni odklon napak
sigma<-11

# izbrana parametra modela
beta0<-103
beta1<-0.98

#  izraèun variance ocene b0
var.b0<-sigma^2*(1/n+mean(starost)^2/sum((starost-mean(starost))^2))
sqrt(var.b0)

#  izraèun varinace ocene b1
var.b1<-sigma^2/sum((starost-mean(starost))^2)
sqrt(var.b1)

# za ponovljivost rezultatov
set.seed(77) 

# vrednosti sluèajnih napak
epsilon<-rnorm(n, mean=0, sd=sigma)

# simulirane vrednosti odzivne spremenljivke
SKT<-beta0 + beta1*starost + epsilon

# model enostavne linearne regresije na simuliranih podatkih
mod<-lm(SKT~starost)
(b0<-coef(mod)[1])
(b1<-coef(mod)[2])
(p<-coefficients(summary(mod))[2,4])
(sp.meja.b1<-confint(mod)[2,1])
(zg.meja.b1<-confint(mod)[2,2])
plot(starost, SKT, pch=16)
abline(reg=mod, col="red")

# simulacijo ponovimo Nsim krat
Nsim <- 1000
reg.sim <- function(x, beta0, beta1, sigma, Nsim) {
  # pripravimo prazne vektorje za rezultate simulacij, oceni parametrov b0 in b1,
  # p-vrednost za testiranje domneve beta1=0, 
  # spodnjo in zgornjo mejo intervala zaupanja za beta1
  b0 <- numeric(Nsim) 
  b1 <- numeric(Nsim)
  p <- numeric(Nsim)
  sp.meja.b1 <- numeric(Nsim)
  zg.meja.b1 <- numeric(Nsim)
  n <- length(x)
  for (i in 1:Nsim) {
    epsilon<-rnorm(n, mean=0, sd=sigma)
    y<-beta0+beta1*x+epsilon
    mod<-lm(y~x)
    b0[i]<-coef(mod)[1]
    b1[i]<-coef(mod)[2]
    p[i]<-coefficients(summary(mod))[2,4]
    sp.meja.b1[i]<-confint(mod)[2,1]
    zg.meja.b1[i]<-confint(mod)[2,2]
  }
  return(data.frame(b0,b1,p,sp.meja.b1,zg.meja.b1))
}
rez.1000<-reg.sim(x=tlak$starost, beta0=103, beta1=0.98, sigma=11, Nsim=1000)

# 2.5 in 97.5 centil za b1 na podlagi simulacij
(centili<-quantile(rez.1000$b1, probs=c(0.025, 0.975)))

# ocena verjetnosti za napako II. vrste 
sum(rez.1000$p>0.05)/Nsim

# ocena moèi testa na podlagi simulacij
(moc.testa<-1-sum(rez.1000$p>0.05)/Nsim)

# porazdelitev preseèišè  
boxplot(rez.1000$b0, ylab="b0") 
abline(h=beta0, col="red")

# porazdelitev naklonov
boxplot(rez.1000$b1, ylab="b1")
abline(h=beta1, col="red")
abline(h=centili, col="blue")

# povezanost b0 in b1
plot(rez.1000$b0, rez.1000$b1, pch=1, xlab="b0", ylab="b1")


ggplot(rez.1000[1:100,], aes(x=1:100,y=b1, ymin=sp.meja.b1, ymax=zg.meja.b1)) +
  geom_pointrange(size=0.2, shape=16) + 
  geom_hline(yintercept=beta1) 

# delež intervalov zaupanja, ki ne vsebujejo prave vrednosti parametra beta1, to je ocena velikosti testa
(delez.brez<-sum(rez.1000$sp.meja.b1>beta1 | rez.1000$zg.meja.b1<beta1)/Nsim)

# LINEARNI MODEL V MATRIÈNI OBLIKI
X<-model.matrix(model.SKT)   # modelska matrika
X[1:5,]   # prvih 5 vrstic
t(X) %*% X   # kaj je v tej matriki: n,  vsota x, vsota x*x
t(X) %*% tlak$SKT  # kaj je v tej matriki: vsota y, vsota x*y
b<-solve(t(X) %*% X) %*% t(X) %*% tlak$SKT; b  # oceni parametrov 
H<-X %*% solve(t(X) %*% X) %*% t(X)
dim(H)
round(H[1:10,1:10],3)  # izpis prvih 10 stolpcev in 10 vrstic matrike H
round(hatvalues(model.SKT),3)

# centriranje spremenljivke starost
tlak$starost.cent<- tlak$starost-mean(tlak$starost)
model.SKT.cent<- lm(SKT~starost.cent, data=tlak)
par(mfrow=c(2,2))
plot(model.SKT.cent)
summary(model.SKT.cent)
vcov(model.SKT.cent)

# vkljuèitev spremenljivke spol v model
ggplot(data=tlak, mapping=aes(x=starost, y=SKT)) +
  facet_grid(.~spol) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  xlab("Starost (leta)") +
  ylab("SKT (mm Hg)")

