library(car)
library(blockmodeling)
library(multiUS)

# PRIPRAVA PODATKOV -------------------------------------------------------

# uvoz podatkov SPSS
podatki <- readRDS("../Vaje 1/ESS_SLO_rekodirane")

odprtost <- c("drugaènost", "užitek", "vznemerljivost", "zabava", "kreativnost", "svoboda")
konservativnost  <- c("varnost", "ponižnost", "obramba", "tradicija", "ubogljivost", "sprejemljivost")

Sch <- c(odprtost, konservativnost)

# uporabljali bomo samo enote, ki imajo vrednosti pri vseh spremenljivkah
com <- complete.cases(podatki[, c(Sch, "sektor")])
schData <- podatki[com, c(Sch, "sektor")]

# PREDPOSTAVKE DISKRIMINANTNE ANALIZE -------------------------------------

# izracunamo frekvence po kategorijah
# (vsaj dve enoti morata biti v vsaki skupini)
# p < (n-1) (p je stevilo spremenljivk, n je stevilo enot)
(status_t <- table(as.character(schData$sektor)))
# izracunamo deleze po kategorijah (+ za utemeljitev predpostavljene verjetnosti na populaciji)
round(prop.table(status_t)*100, 2)

freqTab(schData$sektor)

# preverimo multikolinearnost
plotMat(cor(schData[, Sch]), main = "")

# nobena spremenljivka ne sme biti linarna kombinacija ostalih spremenljivk

# variancno-kovariancna matrika je v vsaki populacijski skupini enaka
# ce je predpostavka krsena, lahko uporabimo kvadratno diskriminantno analizo
# pogledamo tabele
par(mfrow = c(2, 2))
matrike <- by(data = schData[, Sch], INDICES = schData$sektor, FUN = cov)
for (i in names(matrike)){
  plotMat(matrike[[i]], main = i, mar = rep(5, 4), title.line=0, print.x.axis.val = FALSE)
} 

# Box M-test 
# Ho: Vse variancno-kovariancne matrike so enake.
# (test ni robusten na odstopanja od normalnosti)
# ce je ta predpostavka krsena, bo vecja verjetnost, da bodo 
# enote prej razvrscene v skupino z vecjimi kovaraincami
BoxMTest(X = schData[, Sch], 
         cl = as.factor(schData$sektor), 
         alpha = 0.05, 
         test = FALSE)

# ce so razlike v povprecjih majhne, lahko pricakujemo, da bomo
# z diskriminatnimi funkcijami slabo locevali med skupinami
# pogledamo povprecja po skupinah glede na odvisno spremenljivko
dev.off()
par(mar = c(10, 5, 1, 1))
plotMeans(x = schData[, Sch], 
          by = schData[, "sektor"], 
          ylim = c(1, 6), 
          ylab = "Povprecja", 
          xleg = "bottomright")

# MANOVA
# Ho: vsa povprecja intervalnh spremenljivk so enaka med skupinami
summary(manova(as.matrix(schData[, Sch]) ~ schData$sektor))

# preverimo, med katerimi kategorijami obstajajo razlike v povprecjih
rez <- matrix(NA, ncol = 3, nrow = length(Sch))
for (i in 1:length(Sch)) {
  a <- summary(aov(schData[, Sch[i]] ~ schData$sektor))
  rez[i, 1] <- Sch[i]
  rez[i, 2] <- paste0("F(", a[[1]][1,1], "; ", a[[1]][2,1], ") = ", round(a[[1]][1,4], 3))
  rez[i, 3] <- printP(a[[1]][1,5])
}
rez

# POMEMBNOST IN STEVILO DISKRIMINANTNIH FUNKCIJ ---------------------------

# izvedemo linearno diskriminantno analizo na standardiziranih podatkih
# ker nismo nastavili parametra "prior", funkcija oceni verjetnosti po skupinah iz podatkov
# te vrejtnosti se uporabijo pri klasifikaciji
# CV = cross validation (jackknife)
LDARes <- ldaPlus(x = schData[, Sch],
                  grouping = schData$sektor,
                  pred = TRUE,
                  CV = TRUE)

# pogledamo vrednosti kanonicnih korelacij
# kako mocno sta povezana sklopa spremenljivk
# (ce gledamo linearni kobinaciji spremenljivk sklopov s takimi
# vrednostmi regresijskih utezi, da je korelacija maksimalna)
# % - ita DF pojasni X% razlik med povprecji
# Cor - korelacija med ito DF in dummy spremenljvko je X
# Sq. Cor. - ita DF pojasni X% variabilnosti v spremenljivkah, ki predstavljajo skupine
round(LDARes$eigModel, 3)

# preverimo statisticno znacilnost diskriminatnih funkcij
# Ho: povprecja diskriminantnih funkcij po skupinah so enaka
options(scipen = 999)
round(LDARes$sigTest, 3)

# POMEN DISKRIMINANTNIH FUNKCIJ -------------------------------------------

# najprej nas zanima, med katerimi skupinami locuje posamezna diskriminantna funkcija
round(LDARes$centroids, 3)
# 1DF:   podjetje v javni lasti vs. podjetje v zasebni lasti
# 2DF:   javni sektor vs. ostalo

# sedaj nas zanimajo spremenljivke, ki imajo najvecji vpliv
# na vrednosti diskriminantne funckcije (spremenljivke, ki najbolj locujejo med skupinami)
# spodaj so standardizirane vrednosti diskriminantne funkcije
plotMat(LDARes$standCoefTotal, mar = c(1, 5, 5, 1), main = "")

# za interpretacijo lahko uporabimo tudi korelacije med
# vrednostmi diskriminatne funkcije in pojasnjevalno spremenljivko
# pogosto se gledajo "pooled" corelacije zntoraj skupin (glej ?ldaPlus)
korelacije <- LDARes$corr
plotMat(korelacije, mar = c(1, 5, 5, 1), main = "")

# KVALITETA OCENJENEGA MODELA ---------------------------------------------

# Pogledamo, klasifikacijsko tabelo 
# Enoto uvrstimo v skupino, katere centorid  v prostoru
# diskriminantnih spremenljivk ji je najbližje.
# (frekvence in delezi in delez pravilno razvrscenih) na vse podatkih
LDARes$class

# pogledamo, klasifikacijsko tabelo 
# (frekvence in delezi in delez pravilno razvrscenih) z uporabo CV
LDARes$classCV

# za boljso oceno, kako dober je model, je morda dobro pogledati se enkrat 
# prior in dva izracuna na podlagi le-teh:
LDARes$prior
# delez enot, ki bi jih razvrstili pravilno, ce bi vse uvrstili v najveco skupino
max(LDARes$prior)*100   
# delez enot, ki bi jih razvrstili pravilno, èe bi enote razvrscali povsem slucajno
sum(LDARes$prior^2)*100 

# sedaj predpostavimo, da so na populaciji vse skupine enako velike
# Ali je to v nasem primeru smiselno glede na vedenje o populacijskih vrednostih?
LDAResEqPrior <- update(LDARes,  prior = rep(1/4, times = 4))
# pogledamo rezultate
LDAResEqPrior$class
LDAResEqPrior$classCV

# primerjamo modela
par(mfrow = c(1, 2))
plotMat(LDARes$classCV$perTab[,1:4], main = "originalne verjetnosti",  mar = c(1, 10, 5, 1), title.line = -5)
plotMat(LDAResEqPrior$classCV$perTab[,1:4], main = "enake verjetnosti",  mar = c(1, 10, 5, 1), title.line = -5)

# razlicne velikosti skupin predpostavimo pri izracunu diskriminantne funkcije
# pri klasifikaciji pa prdpostavimo enake velikosti skupin
LDAResPriorZaIzracun <- ldaPlus(x = schData[, Sch],
                                 grouping = schData$sektor,
                                 # spodnji argument, ce je FALSE, potem se prior verjetnosti
                                 # NE uporabi pri ocenjevanju parametrov
                                 # (namesto tega se uporabi verjetnosti na podlagi vzorca)
                                 usePriorBetweenGroups = FALSE,
                                 prior = rep(1/4, times = 4),
                                 pred = TRUE,
                                 CV = TRUE)

# narisemo rezultate in jih primerjamo (koeficiente in klasifikacijske tabele)
par(mfrow = c(2, 3))                                                            # R = razlicno, E = enako
plotMat(LDARes$scaling, main = "R ocena \n R razvršèanje", title.line = 1.1)    # R oceno + R za razvrscanje (vsebovano v oceni)
plotMat(LDAResEqPrior$scaling, main = "E ocena \n E razvršèanje", title.line = 1.1)         # E oceno + E za razvrscanje
plotMat(LDAResPriorZaIzracun$scaling, main = "R ocena \n E razvršèanje", title.line = 1.1)  # R oceno + E za razvrscanje

plotMat(LDARes$classCV$perTab[, 1:4], main = "", mar = c(1, 10, 5, 1))
plotMat(LDAResEqPrior$classCV$perTab[, 1:4], main = "", mar = c(1, 10, 5, 1))
plotMat(LDAResPriorZaIzracun$classCV$perTab[, 1:4], main = "", mar = c(1, 10, 5, 1))

# ENOTE V PROSTORU DVEH DISKRIMINANTNIH FUNKCIJ ---------------------------

dev.off()
# narisemo navaden razsevni grfikon - tocke so pobarvane glede na pripadnost k skupini
plot(LDARes$pred$x[,1:2],
     col=as.numeric(as.factor(schData$sektor)),
     cex=0.5,
     pch = 16)
# dodamo legendo
legend(x="bottomright", col=1:5, pch=19, legend = levels(as.factor(schData$sektor)))
# izracunamo vrednosti centroidov
centroidi <- aggregate(LDARes$pred$x, by = list(schData$sektor), FUN = mean)
# narisemo centroide
points(centroidi[, 2:3], col = 1:5, cex = 2, pch = 19)
# narisemo osi
abline(h = 0, v = 0)

# sedaj narisemo graf, kjer barve predstavljajo razvrstitev na podlagi modela crke pa dejanske vrednosti
plot(LDARes$pred$x[,1:2], 
     col=LDARes$pred$class,
     pch=c("A", "B", "C", "D")[as.numeric(as.factor(schData$sektor))],
     cex=0.5)
# dodamo legendi
legend(x="bottomright", col = 1:5, pch=19, levels(LDARes$pred$class))
legend(x="bottomleft", pch=c("A", "B", "C", "D"), levels(LDARes$pred$class))
# izracunamo centroide
# vedno je potrebno pogledati spodnji izpis in
# prilagoditi col v funkciji points, ker se lahko zgodi,
# da bodo nekatere kategorije prazne in bodo zato barve napacne
centroidi <- aggregate(LDARes$pred$x, by = list(LDARes$pred$class), FUN = mean)
points(centroidi[, 2:3], cex = 2, pch = 19, col = 1:4)
# dodamo osi
abline(h = 0, v = 0)

# KVADRATNA DISKRIMINANTNA ANALIZA ----------------------------------------

# izvedemo kvaratno diskriminantno analizo
qdasektor <- qda(x = scale(schData[, Sch]), grouping = schData$sektor)
# napovemo pripadnost k skupini (vrednost 'odvisne' spremenljivke)
qdasektor$pred <- predict(qdasektor)
# napovemo pripadnost k skupini (vrednost 'odvisne' spremenljivke) na podlagi CV
qdasektor$predCV <- update(qdasektor, CV = TRUE)
# naredimo klasifickacijsko tabelo
tab <- table(pred = qdasektor$pred$class, obs = schData$sektor)
prop.table(tab, margin = 2)*100
sum(diag(tab))/sum(tab)*100
# naredimo klasifickacijsko tabelo - CV
tab <- table(pred = qdasektor$predCV$class, obs = schData$sektor)
prop.table(tab, margin = 2)*100
sum(diag(tab))/sum(tab)*100

