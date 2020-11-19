table(studenti$starost)
starosti = sort(unique(studenti$starost))

# funkcija za izracun povprecne teze za starost
povpTeza = function(x){
  return(mean(studenti$masa[studenti$starost == x]))
}
povprecja = sapply(starosti, FUN = povpTeza)

tabelaPovpTez = cbind(starosti,povprecja)
# tabelo izpisemo v porocilo s pomocjo npr. "kable"
# za "kable" potrebujemo knjiznico "knitr"
library(knitr)
kable(tabelaPovpTez,digits = c(0,1))

# druga varianta (npr. da so starosti faktor)
starostiF = as.character(sort(unique(studenti$starost)))
tabelaPovpTez = cbind(starostiF,povprecja)
str(tabelaPovpTez)
# spremenimo tabelo spet v stevila! (ce zelimo zapis za doloceno st. decimalnih mest natancno)
sprTabela = matrix(as.numeric(tabelaPovpTez),ncol=2)
kable(sprTabela,digits = c(0,1))

# ANALIZA VARIANCE - test
# H0: mu_20 = mu_21 = mu_22 = mu_23
rezA = aov(masa~as.factor(starost),data = studenti)
summary(rezA)
# post-hoc testi! (le, ce dobimo stat. znacilen rezultat)
# upostevajo popravke za veckratne primerjave
# za 1 stat. test je napaka 1. vrste (zavrnemo H0, ceprav je ne bi smeli) = alpha
# ponavadi je to 0.05
# kaksna je napaka 1. vrste, ce naredimo 6 stat. testov?
# pravilno sklepamo v 95% primerov (za 1 test)
# kaksna je torej verjetnost pravilnega sklepanja pri 6 stat. testih?
# P(test1 pravilno sklepanje)* P(test2 pravilno) * ... * P(test6 pravilno)
0.95^6
# torej je napaka 1. vrste v 6 testih enaka
1-0.95^6
TukeyHSD(rezA) # adjusted p-value
# Wikipedia - multiple testing, multiple comparisons problem

# linearna regresija
# Y = b0 + b1*X,   masa = b0 + b1*starost
rezL = lm(masa~starost,data=studenti)
summary(rezL)

rezL2 = lm(masa~starost + visina + starost:visina ,data=studenti)
rezL2 = lm(masa~starost * visina ,data=studenti)
# starost:visina --> dodaj se interakcijo v model
# masa = b0 + b1*starost + b2*visina + b3*starost*visina

rezL3 = lm(masa~starost + visina,data=studenti)
summary(rezL3)
confint(rezL3)

# -----------------------------------------------------------
#### druga analiza z linearno regresijo

plot(studenti$mati,studenti$oce,pch=16)
rezMO = lm(oce~mati,data=studenti)
summary(rezMO)
# predpostavke linearne regresije
# 1 - enote neodvisne
# 2 - ostanki normalno porazdeljeni
# 3 - povezava med spremenljivkama lahko linearna
# 4 - homoskedasticnost ostankov
# za 4: narisemo razporejenost ostankov glede na napovedane vrednosti
plot(rezMO$fitted.values,rezMO$residuals)
abline(h=0,lty=2)
plot(rezMO)

# izrisimo model
plot(studenti$mati,studenti$oce,pch=16)
# premica linearnega modela
abline(a = rezMO$coefficients[1], b= rezMO$coefficients[2])

# ggplot - graf s premico linearnega modela (DOMA)

# dodajmo v data.frame studenti se ostanke in pricakovane vrednosti iz modela
studenti$ostanki = rezMO$residuals # NAPAKA zaradi razl. dolzine vektorjev
# izlocimo iz podatkov enote z manjkajocimi vrednostmi
studenti2 = na.omit(studenti[,c("mati","oce")])
rezMO2 = lm(oce~mati,data=studenti2)
summary(rezMO2)
studenti2$ostanki = rezMO2$residuals
