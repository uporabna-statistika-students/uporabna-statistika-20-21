#library("foreign")
library("e1071")
library("mclust")
library("cluster")
library("questionr") 
library("car")
# install.packages("multiUS", repos="http://R-Forge.R-project.org")
library("multiUS")
library("blockmodeling")
#######################################################################################
#                            PRIPRAVA PODATKOV
#######################################################################################
# uvoz podatkov SPSS
# to so podatki, ki smo jih shranili na 
# privh vajah
podatki <- readRDS("../Vaje 1/ESS_SLO_rekodirane")

odprtost <- c("drugaènost", "užitek", "vznemerljivost", "zabava", "kreativnost", "svoboda")
konservativnost  <- c("varnost", "ponižnost", "obramba", "tradicija", "ubogljivost", "sprejemljivost")

Sch <- c(odprtost, konservativnost)

# pri razvrscanju bomo uporabljali samo enote, ki imajo vrednosti pri vseh spremenljivkah
com <- complete.cases(podatki[, Sch])
sum(com); mean(com)*100
SchData <- podatki[com, Sch]
varLabs <- attributes(podatki)$variable.labels
#######################################################################################
#                            HIERARHICNO RAZVRSCANJE
#######################################################################################
# standardizirane vrednosti - z standardizacija 
SSchData <- scale(SchData)
# matrika razlicnosti - evklidska razdalja
d <- dist(SSchData, method="euc")
# kvadridana evklidska razdalja
d2 <- d^2

par(mfrow = c(2, 2))
# uporabimo metodo minimalna metoda ali enojna (single linkage) povzeanost
# primerna za dolge, neelipticne skupine, ki so jasno locene
single <- hclust(d = d2, method= "single")
plot(single, hang = -1, 
     main = "single\n(minimalna metoda)", 
     ylab = "razdalja", 
     xlab = "enote",
     sub = "")

# uporabimo maksimalno metodo ali polno (complete linkage) povezanost 
# primerna za okrogle skupine
complete <- hclust(d = d2, method = "complete")
plot(complete, hang = -1, 
     main = "complete\n(maksimalna metoda)", 
     ylab = "razdalja", 
     xlab = "enote", 
     sub = "")

# uporabimo McQuittyjevo metodo ("povprecna" povzeanost)
mcquitty <- hclust(d = d2, method = "mcquitty")
plot(mcquitty, hang = -1, 
     main = "mcquitty\n(povprecna metoda)", 
     ylab = "razdalja", 
     xlab = "enote", 
     sub = "")

# uporabimo wardovo metodo
# primerna za elipticne skupine
ward <- hclust(d = d2, method = "ward.D")
plot(ward, hang = -1,
     main = "Ward\n(Wardova metoda)", 
     ylab = "razdalja", 
     xlab = "enote", 
     sub = "")
# ce zelite, lahko skupine obrobite
rect.hclust(ward, k=4, border="red")

# shranimo rezultate razlicnih metod
# za stevilo skupin se odlocimo na podlagi dendrograma
singleK3 <- cutree(single, k = 3)
compK2 <- cutree(complete, k = 2)
compK3 <- cutree(complete, k = 3)
mcquittyK2 <- cutree(mcquitty, k = 2)
mcquittyK3 <- cutree(mcquitty, k = 3)
wardK3 <- cutree(ward, k = 3)
wardK4 <- cutree(ward, k = 4)

# izracunamo vrednosti Wardove kriterijske funkcije - manj je bolje
# primerjamo lahko samo razvrstitve z enakim stevilom skupin, saj 
# visanje stevila skupin v splosnem niza vrednost KF
# razlicne metode pogojno
rezWard <- matrix(nrow = 4, ncol = 3)
rownames(rezWard) <- c("minimalna metoda", "maksimalna metoda", "povprecna metoda", "Wardova metoda")
colnames(rezWard) <- c("2", "3", "4")

rezWard[1, 2] <- wardKF(X = SSchData, clu = singleK3)
rezWard[2, 1] <- wardKF(X = SSchData, clu = compK2)
rezWard[2, 2] <- wardKF(X = SSchData, clu = compK3)
rezWard[3, 1] <- wardKF(X = SSchData, clu = mcquittyK2)
rezWard[3, 2] <- wardKF(X = SSchData, clu = mcquittyK3)
rezWard[4, 2] <- wardKF(X = SSchData, clu = wardK3)
rezWard[4, 3] <- wardKF(X = SSchData, clu = wardK4)
rezWard

# narisemo povprecja odgovorov po skupinah - nestandardizirani podatki in standardizirani podatki
# (za najboljso razvrstitev glede na Wardovo KF)
# namesto z argumentom "ylab" se lahko privzeto ime spremeni tudi v sami funkciji plotMeans
par(mfrow = c(1, 2), mar = c(10, 5, 1, 1))
plotMeans(SchData, by = wardK4, 
          ylim = c(1, 6), 
          plotLegend = FALSE, 
          ylab = "povprecja\nnestandardiziranih spremenljivk", 
          xlab = "", 
          main = "Wardova metoda")
# abline(v = 6.5)
plotMeans(SSchData, by= wardK4, 
          ylim = c(-2, 2), 
          plotLegend = FALSE, 
          ylab = "povprecja\nstandardiziranih spremenljivk", 
          xlab = "", 
          main = "Wardova")
# abline(v = 6.5)

# silhueta meri, kako blizje je enota enotam iz svoje skupine, 
# kakor enotam iz druge najblizje skupine
# spodaj gledamo povprecno silhueto (za vse enote) za razlicno
# stevilo skupin
# poenostavljeno: visoke vrednosti pomenijo, dolge razdalje do enot
# druge skupine in kratke do enot iste skupine
sis <- NULL
for (i in 2:10){
  si <- silhouette(cutree(ward, k = i), daisy(SSchData))
  sis[i] <- mean(si[,3])
}
plot(sis, type = "b", xlab = "stevilo skupin", ylab = "povprecna silhueta")
#######################################################################################
#                            NEHIERARHICNO RAZVRSCANJE
#######################################################################################
# http://shabal.in/visuals/kmeans/2.html

# k-means; stevilo skupin lahko dolocimo tudi na podlagi vsote kvadratov razdalij znotraj skupin
# vrednost bo vedno padala s stevilom skupin -> gledamo koleno 
# (ce ni kolena je to lahko indikator, da skupine niso jasno locene)
Kmax <- 12
kmCrit<-numeric(Kmax)
for(k in 1:Kmax){
  kmCrit[k] <- kmeans(x = SSchData, centers = k, nstart = 100, iter.max = 20)$tot.withinss
}
plot(kmCrit, 
     type="o", 
     xlab = "stevilo skupin (k)", 
     ylab = "vrednost Wardove kriterijske funkcije")

# povecaj stevilo iteracij (iter.max), v primeru, da algortiem kmeans ne skonvergira
# iter.max = maksimlano stevilo iteracij
# nstart = stevilo ponovitev algoritma (da se izognemo lokalnemum optimumu)
# B = stevilo bootstrap vzorcev
# K.max = maksimalno stevilo skupin
# d.power je eksponent za Evklidske razdalje
# poglej ?clusGap !
gskmn <- clusGap(SSchData, FUN = kmeans, iter.max = 100, nstart = 100, K.max = 8, B = 100, d.power = 2)
plot(gskmn, main = "GAP statistika za razlicno\nstevilo skupin", xlab = "stevilo skupin")

# nastavimo seme, da bomo dobili vsi enake rezultate
set.seed(123)
km3 <- kmeans(x = SSchData, centers = 3, nstart = 1000) 

par(mfrow=c(1,2), mar = c(10, 5, 1, 1))
plotMeans(SchData, by= km3$cluster, 
          ylim = c(1, 6), 
          ylab = "povprecja\nnestandardiziranih spremenljivk", 
          plotLegend = FALSE, 
          xlab = "")
plotMeans(SSchData, by=km3$cluster, 
          ylim = c(-2, 2), 
          ylab = "povprecja\nstandardiziranih spremenljivk", 
          plotLegend = FALSE, 
          xlab = "")

# silhueta
sis <- NULL
for (i in 2:10){
  cluster <- kmeans(x = SSchData, centers = i, nstart = 100, iter.max = 100)$cluster
  si <- silhouette(cluster, daisy(SSchData))
  sis[i] <- mean(si[,3])
}
plot(sis, type = "b", xlab = "stevilo skupin", ylab = "povprecna silhueta")
#######################################################################################
#                            RAZVRSCANJE NA PODLAGI MODELOV
#######################################################################################
# Metoda poredpostavlja multivariatno normalno porazdelitev. 
# V primeru da predpostavka drzi, se v vecini simulacij metoda izkaze kot optimalna.
# https://journal.r-project.org/archive/2016/RJ-2016-021/index.html
# Morda uporabno gradivo: https://bradleyboehmke.github.io/HOML/model-clustering.html


# naredimo razvrstitev na originalnih podatkih
# funkcija sama izbere najprimernejsi model
# G pomeni stevilo skupin za razvrscaje (od-do)
mc <- Mclust(SchData, G=1:10)
# funkcija summary izpise ime izbranega modela in velikosti skupin
# model je izbran na podlagi kriterija BIC 
# VVE -> elipsoidne skupine, ki so enako velike (v smislu prostora, ki ga zajemajo v
# koordinatnem sistemu), razlicno usmerjene in razlicnih oblik
summary(mc)
# pogledamo vrednosti BIC (Bias information critery)
# se za ostale modele
plot(mc, what = "BIC")

# priorControl za dolocitev prir probabilities
# Rezultira v bolj stabilnih ocenah, a lahko povzroci pristranskost.
# Uporabimo, ko imamo tezave z oceno modelov.
# V trenutni implementaciji se lahko uporabi samo za
# oceno 10 osnovnih modelov.
mcP <- Mclust(SchData, G=1:10, prior = priorControl())
summary(mcP)
plot(mcP, what = "BIC")

# STANDARDIZIRANI PODATKI
# Med rezultati, kjer analiziramo standardizirane in nestandardizirane podatke, 
# po teoriji pri modelih, kjer porazdelitev ni okrogla (spherical), 
# (se pravi pri oznaki modela na sredini ni "I"), ne bi smelo biti razlik. 
mcStdP <- Mclust(SSchData, G=1:10)
plot(mcStdP, what="BIC")
summary(mcStdP)

par(mfrow=c(1,3))
plot(mc, what="BIC", legendArgs = list(x = "bottomright"))
title(main="no prior, no std")
plot(mcP, what="BIC", legendArgs = list(x = "topleft"))
title(main="prior, no std")
plot(mcStdP, what="BIC", legendArgs = list(x = "bottomright"))
title(main="no prior, std")

# OMEJITEV NA SPECIFICNE MODELE
# Vcasih je bolje omejiti nabor modelov, recimo na samo tiste, ki imajo 
# enako velikost, morda celo na okrogle (kar je potem skoraj enakovredno K-means ali Ward).
# EII je model, ki ga isce k-means. Poleg tega je tudi model, ki isce okrogle, 
# enako velike skupine (v smilsu volumna). 
# Ce volumna ne fisiramo, lahko dobimo skupine, kjer je ena znotraj druge. 
# Ce drugi dve komponenti ne nastavimo vsaj na EE (obe), potem lahko dobimo skupine, ki se krizajo. 
# Oboje je lahko vcasih nezazeljeno. Oboje sicer preprecimo tudi z modelom EEE. 
mcSEL <- Mclust(SSchData, G=2:7, modelNames = c("EII", "VVE"))
plot(mcSEL, what="BIC", legendArgs = list(x = "bottomright"))
mcEII3 <- Mclust(SSchData, G = 3, modelNames = "EII")
mcVVE3 <- Mclust(SSchData, G = 3, modelNames = "VVE")

# resitve lahko pogledamo tudi graficno
# (ne vkljuciti v domaco nalogo)
plot(mcVVE3, what="classification", asp = 1) # tocke so pobarvane glede na pripadnost k skupini
plot(mcVVE3, what="uncertainty", asp = 1) # vecje tocke pomenijo bolj negotovo razvrstitev

plot(mcEII3, what="classification", asp = 1) # tocke so pobarvane glede na pripadnost k skupini
plot(mcEII3, what="uncertainty", asp = 1) # vecje tocke pomenijo bolj negotovo razvrstitev

# narisemo povprecja odgovorov po skupinah 
par(mfrow=c(2,2), mar = c(10, 5, 1, 1))
plotMeans(SchData, by= mcEII3$classification, ylim = c(1, 6), main="", ylab = "povprecja\nnestandardiziranih spremenljivk", plotLegend = FALSE) 
plotMeans(SSchData, by=mcEII3$classification, ylim = c(-2, 2), main="", ylab = "povprecja\nstandardiziranih spremenljivk", plotLegend = FALSE) 
plotMeans(SchData, by= mcVVE3$classification, ylim = c(1, 6), main="", ylab = "povprecja\nnestandardiziranih spremenljivk", plotLegend = FALSE) 
plotMeans(SSchData, by=mcVVE3$classification, ylim = c(-2, 2), main="", ylab = "povprecja\nstandardiziranih spremenljivk", plotLegend = FALSE) 

#######################################################################################
#                            PRIMERJAVA RAZVRSTITEV
#######################################################################################
# iz vsake metode (hierarhicne, nehierahicne, na podlagi modelov) izberemo najboljso razvrstitev
# in ponovno pogledamo povprecja po skupinah ter jih primerjamo
par(mfrow = c(1, 3), mar = c(10, 5, 1, 1))
plotMeans(SSchData, by=km3$cluster, ylim = c(-2, 2), ylab = "povprecja\nstandariziranih spremenljivk", plotLegend = FALSE, main = "k-means", xlab = "") 
plotMeans(SSchData, by=wardK4, ylim = c(-2, 2), ylab = "povprecja\nstandariziranih spremenljivk", plotLegend = FALSE, main = "Ward", xlab = "") 
plotMeans(SSchData, by=mcEII3$classification, ylim = c(-2, 2), ylab = "povprecja\nstandariziranih spremenljivk", plotLegend = FALSE, main = "na podlagi modelov", xlab = "") 

# kako podobni sta si razvrstitvi
crand(table(wardK4, km3$cluster))
crand(table(wardK4, mcEEE3$classification))
crand(table(km3$cluster, mcEII3$classification))

# kontingencna tabela
# razmislite o smiselnosti kontingencne tabele (vrstni red vrstic in stolpcev)
# diag = delez enot na diagonali
# kappa = normaliziran delez enot na diagonali
kont.tabela <- table(km3$cluster, mcEII3$classification)
kont.tabela <- kont.tabela[, c(3, 2, 1)]
classAgreement(kont.tabela)

kont.tabela <- table(km3$cluster, wardK4)
classAgreement(kont.tabela[, c(4, 1, 3, 2)])

# katera razvrstitev je boljsa glede na WKF?
wardKF(X = SSchData, clu = wardK4)
wardKF(X = SSchData, clu = km3$cluster)
wardKF(X = SSchData, clu = mcEEE3$classification) 

#######################################################################################
#                            PREDSTAVITEV REZULTATOV -- INTERPRETACIJA
#######################################################################################
par(mfrow = c(1, 2), mar = c(8, 5, 3, 1))
plotMeans(SchData, by=km3$cluster, ylim = c(1, 6), 
          ylab = "povprecja\nnestandariziranih spremenljivk", 
          plotLegend = FALSE, 
          main = "k-means", 
          xlab = "") 
plotMeans(SSchData, by=km3$cluster, ylim = c(-2, 2), 
          ylab = "povprecja\nstandariziranih spremenljivk", 
          plotLegend = FALSE, 
          main = "k-means", 
          xlab = "") 

# nekako poimenujemo skupine in pripadnost k skupini shranimo v podatke
SchData$raz <- raz <- factor(km3$cluster, labels=c("podpovprecno odprti", "podpovprecno konservativni", "univerzalisti"))
SchData$raz <- factor(SchData$raz, labels = levels(raz)) 

# dodamo legendo
par(mfrow = c(1, 2))
plotMeans(SchData[, Sch], by=raz, ylim = c(1, 6), 
          ylab = "povprecja\nnestandariziranih spremenljivk", 
          plotLegend = TRUE, 
          main = "k-means", 
          xlab = "", 
          xleg = "bottomright") 
plotMeans(SSchData[, Sch], by=raz, ylim = c(-2, 2), 
          ylab = "povprecja\nstandariziranih spremenljivk", 
          plotLegend = TRUE, 
          main = "k-means", 
          xlab = "") 

# velikost skupin
table(raz)

# Narisemo enote v 3D prostoru (likartovih lestvic)
# Ali v 2D prostoru Likertovih lestvic (ce imamo 2 Lik. lestvici)
# Spremeni "plot" v "pairs
par(mfrow = c(1, 1))
lik <- cbind(rowMeans(SchData[, odprtost]), 
             rowMeans(SchData[, konservativnost]))
likS <- jitter(lik, amount = 0.1)
colnames(likS) <- c("odprtost", "konservativnost")

plot(x = likS[,1], y = likS[,2], 
     xlab = "odprtost", ylab = "konservativnost",
      pch = 16, cex = 0.5,
      col = SchData$raz,
      asp = TRUE,
      ylim = c(1, 6), xlim = c(1, 6))
abline(h = 3, v = 3)
par(xpd=TRUE)
legend("bottomleft", 
       legend = levels(SchData$raz), 
       col = 1:3, 
       pch = 16,
       xpd = TRUE)

# vpliv dodatnih spremenljivk na razvrstitev v skupino
# preverimo graficno
# opravimo statisticni test
# preverimo moc povezanosti, kjer je smiselno
cbind(prop.table(table(raz, podatki[com, "spol"]), 2), prop.table(table(raz)))
barplot(prop.table(table(raz, podatki[com, "spol"]), 2), 
        beside = FALSE,
        legend = TRUE,
        ylim = c(0, 1),  
        names.arg = levels(podatki$spol),
        col = 1:3)
chisq.test(table(podatki[com, "spol"], raz))
cramer.v(table(podatki[com, "spol"], raz))

par(mar = c(20, 5, 2, 2))
barplot(prop.table(table(raz, podatki[com, "izobrazba"]), 2), 
        beside = FALSE,
        legend = TRUE,
        ylim = c(0, 1),  
        names.arg = levels(podatki$izobrazba),
        col = 1:3, las = 2)
# kruskal-wallisov test se uporablja kot neparametricna 
# alternativa ANOVI (ANOVA na rangih)
kruskal.test(x = podatki[com, "izobrazba"], g = raz)
izobrazbaTabela <- table(podatki[com, "izobrazba"], raz)
# Za ordinalne spremenljivke obstajajo tudi bolj primerne
# mere velikosti ucinka, a jih ne bomo obravnavali.
cramer.v(izobrazbaTabela[-c(1, 14),])

cbind(prop.table(table(raz, podatki[com, "sektor"]), 2), prop.table(table(raz)))
barplot(prop.table(table(raz, podatki[com, "sektor"]), 2), 
        beside = FALSE,
        legend = TRUE,
        ylim = c(0, 1),  
        names.arg = levels(podatki$sektor),
        col = 1:3, las = 2)
# ce so frekvence v kaksnih celicah zelo majhne
# lahko dobite opozorilo o nevljavnih p-vrednostih
# mozna resitev je uporaba bootstrap vzorcev za oceno p-vrednosti
chisq.test(table(podatki[com, "sektor"], raz)) # .. ali ...
chisq.test(table(podatki[com, "sektor"], raz), simulate.p.value = TRUE, B = 10000) 
cramer.v(table(podatki[com, "sektor"], raz))

plotmeans(podatki[com, "starost"] ~ raz, legends = levels(raz), 
          ylab = "starost", 
          xlab = "skupina")
leveneTest(podatki[com, "starost"] ~ raz)
oneway.test(starost ~ raz, data=podatki[com, ], var.equal = FALSE) 
###################################################################
#                    THE END
###################################################################
