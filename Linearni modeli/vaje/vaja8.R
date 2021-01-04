
# Izbira modela z namenom najboljše kakovosti napovedi

library(GLMsData)
data(cheese)
str(cheese)
pairs(cheese)
mod.cheese<-lm(Taste ~ Acetic + H2S + Lactic, data=cheese)

summary(mod.cheese)
plot(mod.cheese)

# PRESS statistika

# izračun na podlagi definicije PRESS ostankov n modelov
e<-residuals(mod.cheese)
(n <- length(residuals(mod.cheese)))
e.press<-numeric()
for (i in 1:n){
    mod<-lm(Taste ~ Acetic + H2S + Lactic, data=cheese[-i,])
    novi<-cheese[i,]
    e.press[i]<-cheese[i,"Taste"] - predict(mod, newdata=novi)
}
head(round(data.frame(e,e.press),2))
plot(e,e.press, xlim=c(-20,30),ylim=c(-20,30))
abline(a=0,b=1, h=0, col=(c("red","blue")))
boxplot(abs(e.press-e))
PRESS<-sum(e.press^2)

# na podlagi vzvodov, računanje se zelo poenostavi
h<-hatvalues(mod.cheese)
press.ost<-residuals(mod.cheese)/(1-h)
# sum(e.press-press.ost)
PRESS<-sum(press.ost^2)
PRESS


# primerjava PRESS statistik po vseh možnih modelih
PRESS<-numeric()
nap.sprem <- names(cheese)
nap.sprem <- nap.sprem[! nap.sprem %in% "Taste"]
n <- length(nap.sprem)
# za vse mo?ne kombinacije
id <- unlist(lapply(1:n,function(i) combn(1:n,i,simplify=FALSE)), recursive=FALSE)
id
formule <- sapply(id, function(i) paste("Taste~", paste(nap.sprem[i], collapse="+")))
formule
for (i in (1:length(formule))){
    mod<-lm(formule[i], data=cheese)
    h<-lm.influence(mod)$hat
    press.ost<-residuals(mod)/(1-h)
    PRESS[i]<-sum(press.ost^2)
}
rez <- data.frame(formule, PRESS)
rez[order(PRESS),]


# Navzkrižno preverjanje

n <- dim(cheese)[1]
n.u <- n.t <- n/2
# naredimo vektor z vrednostmi TRUE in FALSE, vsaka vrednost po 15 krat
ind <- rep(c(TRUE, FALSE), each=n.u)
ind
# slučajno razporedimo vrednosti
(ind <- sample(ind))
cheese.ucni <- cheese[ind,]
cheese.test <- cheese[!ind,]

# izračun CVC za en primer modela

mod.ucni<-lm(Taste~H2S+Lactic, data=cheese.ucni)
y.nap<-predict(mod.ucni, cheese.test)

(CVC<-sum((cheese.test$Taste-y.nap)^2))
(RMSE<-sqrt(CVC/n.t))


# izračun CVC za 5 različnih delitev na učni in testni vzorec

tabela<-data.frame(formule)
for (j in 1:5) {
    izbor<-rep(c(TRUE, FALSE), each=n.u)
    set.seed(j*10)
    izbor<-sample(izbor)
    cheese.ucni<-cheese[izbor,]
    cheese.test<-cheese[!izbor,]
    CVC<-numeric()
    for (i in (1:length(formule))){
        mod<-lm(formule[i], data=cheese.ucni)
        y.nap<-predict(mod, cheese.test)
        CVC[i]<-sum((cheese.test$Taste-y.nap)^2)
    }
    # za primerjavo v nadaljevanju izračunamo tudi RMSE
    tabela<-data.frame(tabela, round(CVC, 1), round(sqrt(CVC/n.t),1))
}
names(tabela)<-c("formula", "CVC1", "RMSE1","CVC2", "RMSE2", "CVC3",
                 "RMSE3", "CVC4", "RMSE4", "CVC5", "RMSE5")
tabela[, c(1:2,4,6,8,10)]

library(cvTools)
cv<-numeric()

# navzkrižno preverjanje brez ene enote (leave one out cross validation), K=n
for (i in (1:length(formule))){
    mod<-lm(formule[i], data=cheese)
    mod.cv<-cvFit(mod, data=cheese, y=cheese$Taste, K=n, seed=7)
    cv[i]<-mod.cv$cv
}
# primerjamo rezultate z izračuni PRESS statistike
data.frame(formule, cv=round(cv,2), round(sqrt(rez$PRESS/n),2) )

# za primerjavo
tabela[, c(1,3,5,7,9,11)]


# Cp Malow-a statistika

library(olsrr)
polni_model <- lm(Taste~.,data=cheese)
Cp<-numeric()
for (i in (1:length(formule))){
    mod<-lm(formule[i], data=cheese)
    Cp[i] <-ols_mallows_cp(mod, polni_model)
}
izpis<-data.frame(formule, Cp)
izpis<-izpis[order(Cp),]; izpis[1:5,]

# library(leaps)
# cp<-leaps(x=cheese[, 2:4], y=cheese$Taste, names=names(cheese)[2:4], method="Cp")
# cbind(cp$which, Cp=cp$Cp)

# AIC Akaike informacijski kriterij

AIC<-numeric()
for (i in (1:length(formule))){
    mod<-lm(formule[i], data=cheese)
    AIC[i]<-AIC(mod)
}
data.frame(formule, AIC=round(AIC,1))

#  Sekvenčne metode

library(MASS)
mod.nul<-lm(Taste~1, data=cheese)
step<-stepAIC(mod.nul, scope=~H2S+Lactic+Acetic, direction="forward")


mod.polni<-lm(Taste~H2S+Lactic+Acetic, data=cheese)
step<-stepAIC(mod.polni, direction="backward")


mod.prvi<-lm(Taste~Acetic, data=cheese)
step<-stepAIC(mod.prvi, scope=~H2S+Lactic+Acetic, direction="both")

# Vaja 2.1 Napovedovanje porabe goriva

library(car)
library(MASS)
Cars93$Poraba<-235.21/Cars93$MPG.highway  # v l/100 km
Cars93$Masa<-Cars93$Weight*0.45359/100    # v 100 kg
Cars93$Prostornina<-Cars93$EngineSize     # v litih
Cars93$Moc<-Cars93$Horsepower             # v KM
Cars93$Poreklo<-Cars93$Origin
Cars93$Tip<-Cars93$Type
avti <- subset(Cars93, select=c(Poraba, Masa, Prostornina, Moc, Poreklo, Tip))
rownames(avti)<-Cars93$Make   ### identifikator vozila na slikah

#  formule za vse možne modele
nap.sprem<-names(avti)
(nap.sprem<-nap.sprem[!nap.sprem %in% c("Poraba")])
n<-length(nap.sprem)
id<-unlist(lapply(1:n, function(i) combn(1:n, i, simplify=FALSE)), recursive=FALSE)
formule<-sapply(id, function(i) paste("Poraba~", paste(nap.sprem[i], collapse="+")))
formule[1:10]
(length(formule))


