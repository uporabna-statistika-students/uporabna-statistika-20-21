# GGPLOT za izris binomske porazdelitve - pred5

N = 350
n = 80 # me tukaj ne zanima
p = 0.2

vzorec = rbinom(1000,size=N,prob = p)
# opazovana porazd - empiricna porazdelitev
opazF = table(vzorec)
df = as.data.frame(opazF)
str(df)
# pretvori st. biserov v stevilsko spremenljivko
df$biseri = as.numeric(as.character(df$vzorec))
# teoreticna porazd - teor. pricakovane frekvence
# E(X) = N*p (Binomska porazdelitev)
df$teor = dbinom(df$biseri,size=N,prob=p)*1000

# na zacetek! - uvozimo knjiznico ggplot2!
library(ggplot2)
ggplot(df,aes(x=biseri,y=Freq)) + geom_bar(stat="identity") +
  geom_point(aes(y=teor),col="red")

# ------------------------------------------------
# testi t
# test t za neodvisna vzorca (dvostranski test)
# H0: mu_F = mu_D
rez1 = t.test(x=studenti$visina[studenti$spol=="M"],
              y=studenti$visina[studenti$spol=="F"])
str(rez1)
rez1$conf.int # izpis intervala zaupanja
# lepsi izpis, na 2 decimalki natancno
round(rez1$conf.int[1],2) # izpis spodnje meje
round(rez1$conf.int[1:2],2) # izpis celega intervala

# enostranski test
# H0: mu_F <= mu_D
# formula: Y ~ X (v nasem primeru, X skupina)
# v nasem primeru odstevamo mu_D - mu_F 
# H0: 0 <= mu_D - mu_F, oziroma mu_D - mu_F >= 0
t.test(visina~spol,data=studenti,alternative="less",
       conf.level=0.99)

# ZAPIS IZVEN R-chunka (PONOVLJIVOST POROCILA)
# Lahko z več kot `r trunc((1-rez1$p.value)*100)`% zaupanjem trdimo, da je na populaciji študentov te univerze povprečna višina fantov višja od povprečne višine deklet, in sicer je 95% interval zaupanja za razliko med povprečnima višinama [`r round(rez1$conf.int[1],1)`, `r round(rez1$conf.int[2],1)`].


# ------------------------------------------------
# hi-kvadrat testi
# opazovane frekvence
table(studenti$lasje,studenti$spol)
# pričakovane frekvence
# kaj sploh pricakujemo? o tem lahko govorimo, ce poznamo H0
# H0: pi_D = pi_F = pi (skupni delez)
# test hi-kvadrat
# izracunali bi pric. frekv. za primer pi (skupnega deleza
# svetlolasih)
skDelez = sum(studenti$lasje=="S")/nrow(studenti) # nimamo NA
# pricakovano st. studentov in studentk s svetlimi lasmi
# st. stud in studentk * skDelez
table(studenti$spol)*skDelez
# cela kont. tabela pric. frekvenc
# dva skupna deleza (S,T)
skDeleza = table(studenti$lasje)/nrow(studenti) 
# druga variant
c(skDelez,1-skDelez)
# sestavimo matriko
tmp1 = rbind(table(studenti$spol),table(studenti$spol))
tmp2 = cbind(skDeleza,skDeleza)
tmp1 * tmp2 # mnozenje po elementih
#%*% # matricno mnozenje!

# statisticni test hi-kvadrat
rez2 = chisq.test(table(studenti$lasje,studenti$spol))
rez2$expected 
# zaradi predpostavk uporaba hi-kvadrat testa ni primerna
rez3 = fisher.test(table(studenti$lasje,studenti$spol))

# izracun razmerja obetov (sv.dekleta vs. sv. fantje)
# obeti = p/(1-p)
delezi = table(studenti$lasje,studenti$spol) / tmp1
obetiDsvetle = delezi[1,1]/delezi[2,1]
obetiFsvetle = delezi[1,2]/delezi[2,2]
OR = obetiDsvetle/obetiFsvetle


table(studenti$mesec)
# H0: delezi/verj. za rojstvo so v vseh mesecih enaki
# 1/12 za vse mesece
chisq.test(table(studenti$mesec))
chisq.test(table(studenti$mesec),p = rep(1/12,12),simulate.p.value = TRUE)

# ------------------------------------------------
# ANOVA - primerjava povprecij
# zanima nas, ali so povprecne teze po starostih razlicne
# H0: mu_starost1 = mu_starost2 = ... = mu_starostN
# ANALIZA VARIANCE
table(studenti$starost)
starosti = sort(unique(studenti$starost))

# funkcija za izracun povprecne teze za starost
povpTeza = function(x){
  return(mean(studenti$masa[studenti$starost == x]))
}
sapply(starosti, FUN = povpTeza)

# ANALIZA VARIANCE - test
rezA = aov(masa~as.factor(starost),data = studenti)
summary(rezA)

