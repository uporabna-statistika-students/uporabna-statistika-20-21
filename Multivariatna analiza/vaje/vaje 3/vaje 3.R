# Ce boste imeli tezave s funkcijo biplot, potem ponovno instalirajte paketek blockmodeling.
# install.packages("blockmodeling", repos="http://R-Forge.R-project.org")
library("blockmodeling")
library("ppcor")
library("gplots")
library("corrplot")
library("GPArotation")
library("car")
library("multiUS") # preverite, ce uporabljate najnovejso verzijo
# install.packages("multiUS", repos="http://R-Forge.R-project.org")
library("psych")
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

# uporabljali bomo samo enote, ki imajo vrednosti pri vseh spremenljivkah
com <- complete.cases(podatki[, Sch])
schData <- podatki[com, Sch]
#######################################################################################
#                            KORELACIJSKA MATRIKA
#######################################################################################
# uporabimo jo za preverjanje smiselnosti uporabe metod (glavne komponente in faktorska analiza)
(R <- cor(schData))
(n <- nrow(schData))
plot.mat(R, main = "")
# alternativni prikaz
corrplot(R, method = "number") 

# Bartlettov test
# Ho: korelacijska matrika je identiteta (na diagonali so enke, izven diagonale pa nule)
cortest.bartlett(R = R, n = n)
#######################################################################################
#                            METODA GLAVNIH KOMPONENT
#######################################################################################
# izvedemo metodo glavnih komponent 
# x = korelacijska ali variancno-kovariancna matrika ali matrika podatkov
# cor = ali naj uporabi korelacijsko matriko
# scores = ali naj izracuna tudi score (vrednosti GK)
PC <- princomp(x = schData, cor = TRUE, scores = TRUE)
# podobno lahko naredimo s funkcijo principal
# utezi, ki jih vrne funkcija principal so reskalirane 
# (korelacije med j-to glavno komponento in i-to spremenljivko)
# vrednosti glavnih komponent (scores) pa so standardizirane
# psych::principal(r = R, nfactors = 2)

# ZANIMIVOST
# Metodo glavnih komponent lahko izvedemo tako, da korelacijsko matriko
# ali variancno-kovariancno matriko razcepimo na lastne vrednosti
# in lastne vektorje.
# Vsota variabilnosti originalnih spremenljivk in vsota lastnih vrednosti
# glavnih komponent sta enaki. 
sum(eigen(cov(schData))$values) 
sum(diag(cov(schData)))

# ukaz summary nam vrne 
# - standardne odklone posameznih glavnih komponent (standard deviation)
# - delez pojasnjene variabilnosti z izbrano glavno komponento (proportion of variance)
# - kumulativne deleze pojasnjene variabilnosti (cumulative proportion)
summary(PC)

# dostpamo do utezi
PC$loadings[,]

# ZANIMIVOST
# Vsota kvadriranih utezi pri posameznih
# glavnih komponentah je enaka 1.
sum(PC$loadings[,1]**2)

# biplot nam pokaze polozaj enot v prostoru glavnih komponent in
# utezi za posamezne spremenljivke
# s to funkcijo lahko razlikujemo samo med dvema dimenzijama / GK
biplot(PC, pc.biplot = TRUE)

# izbira stevila GK
# GLAVNI KRITERIJI:
# - tako da zadnja GK pojasni vsaj 5% variabilnosti
# - dokler so lastne vrednosti vecje od povprecne variabilnosti spreemnljivk 
#   (lastna vrednost standardiziranih spremenljivk je > 1)
# - tam kjer je koleno scree diagrama (stevilo nad kolenom)
# - primerjava z lastnimi vrednostmi na podlagi podatkov generiranih iz nekoreliranih spremenljivk
# n.iter = koliko slucajnih podatkov generirati
# fa = ali naj izracuna lastne vrednosti za GK ("pc") ali FA ("fa")
fa.parallel(x = R, fa = "pc", n.iter = 100, n.obs = n)
abline(h=1, lty = 2, col = "green")

par(mfrow = c(1,2))
# pogledamo utezi za izbrane glavne komponente
# spodaj so nereskalirane utezi (torej regresijski koeficienti)
plot.mat(PC$loadings[,1:2], main = "", cex.axes = 0.7)
# reskalirane urezi so korelacije med j-to glavno komponento in i-to spremenljivko
# reskalirane utezi so (vrednost utezi j-te komponente) * (sqrt(delez pojasnjene var z j-to komponento))
loadingsRe <- cor(schData, PC$scores[,1:2])
plot.mat(loadingsRe,  main = "", cex.axes = 0.7)

# koliko variabilnosti posamezne spremenljivke
# pojasnimo z izbranim stevilom glavnih komponent
# vsota kvadriranih reskaliranih urezi po spremenljivkah je varianca spremenljivke
sort(round(rowSums(loadingsRe**2) ,2))

# ZANIMIVOST
# Vstore reskaliranih utezi po komponentah 
# so lastne vrednosti.
sum(loadingsRe[,1]**2)
PC$sdev[1]**2

# shranimo scores
schData$PC1_odprtost <- PC$scores[, 1]
schData$PC2_nekonservativnost <- PC$scores[, 2]

#######################################################################################
#                            FAKTORSKA ANALIZA
#######################################################################################
# ali so podatki primerni?
# prvi pogoj je, da so spremenljivke med sabo korelirane
# Bartletov test (glej zgoraj)

# Anti image korelacijska matrika
# Izven diagonale so parcialni korelacijski koeficienti pomnozeni z -1.
# Ce si spremenljivke "delijo" skupne faktorje, potem so njihovi 
# parcialni korelacijski koeficienti (po absolutni vrednosti) majhni. 
# Na diagonali so vrednosti kaiser-Meyer-Olkin-ove mere (KMO)
# (delez spremenljivke, ki bi lahko bil pojasnjen s skupnimi faktoji)
# KMO je blizu 1, ko si spremenljivke delijo skupne faktorje.
# (vrednosti za KMO pod 0.5 so zelo slabe, vrednosti nad 0.8 pa zelo dobre)
# Ko so vrednosti KMO > 0.5 recemo, da so spremenljivke dovolj korelirane in so
# primerne za faktorsko analizo.
AI <- antiImage(schData[,Sch])$AIR
plotMat(AI, main = "")
antiImage(schData[,1:7])$KMO

# ocenjevanje stevila faktorjev
# na podlagi kolena scree-diagrama 
# na podlagi simuliranih nekoreliranih podatkov
# fm = katero faktorsko metodo naj uporabi? 
# metode: glej ?fa (maximum likelihood = "ml", predostavlja vecrazsezno normalno porazdelitev, veliki vzorci n > 400)
#                  (metoda glavnih osi = "pa")   
fa.parallel(x = R, n.obs = n, fa = "fa", fm = "ml", n.iter = 100) 

# ----- VERY SIMPLE STRUCTURE
# VSS = 1 - sumsquares(R*)/sumsquares(R) kjer je R* matrika odklonov 
# izberemo stevilo, kjer najvecje utezi pri spremenljivkah najvec pojasnijo (je vrh)
# pri izracunu R* so upostevane samo najvisje utezi pri spremenljivkah (1 najvisja utez, 2 najvisji utezi, ...)
# ----- COMPLEXITY (ni treba interpretirati, niti vkljuciti v nalogo)
# na koliko faktorjih ima vsaka spremenljivka mocno utez (tukaj gledamo povprecno) - zelimo, da je cim blizje 1
# ----- BIC
# pri katerem stevilu faktorjev je BIC najmanjsi?
# temelji na odklonih korelacijske matrike, popravljenih za stevilo enot in stevilo faktojev
# ----- ROOT MEAN RESIDUAL
# Koren povprecnega kvadratnega reziduala odklonov modelskih korelacij od pravih korelacij. 
# koren povprecnega odklona modelskih korelacij od pravih korelacij
# (vrednost standardiziranega RMR < 0.8 je ok)
### funkcija
# n = koliko faktrojev upoštevati (naj bo vecje, kot mislimo, da je prava vrednost st. faktorjev)
# glej ?nfactors
nfactors(x = R, n.obs = n, n = 4, fm = "ml", rotate = "varimax")

# faktorska analiza
# najprej uporabimo posevno rotacijo, v nasem primeru "oblimin"
# pri posevnih rotacijah lahko spremenljivke proiciramo na faktorje na dva nacina:
# ---vzporedno: dobimo regresijske koeficiente med spremenljivkami in faktorji (pattern utezi),
# ---pravokotno: dobimo korelacijske koeficiente med spremenljivkami in faktorji (strukturne utezi)
# lahko damo tudi korelacijsko matriko, a potem ne dobimo faktorskih vrednosti (lahko jih izracunamo s funckijo factor.scores())
(FA.oblimin <- fa(r = schData[, Sch], 
                  nfactors = 2, 
                  n.obs = n, 
                  rotate = "oblimin",
                  scores = TRUE, 
                  fm = "ml", 
                  max.iter = 1000)) 

# ce faktorji niso zelo korelirani med sabo (npr., r > 0.2), potem izvedemo pravokotno rotacijo, npr. "varimax"
(FA.varimax <- fa(r = schData[, Sch], 
                  nfactors = 2, 
                  n.obs = n, 
                  rotate = "varimax", 
                  scores = TRUE, 
                  fm = "ml",
                  max.iter = 1000))
# v domaco nalogo vkljucite samo eno resitev 
# (v vsakem primeru pa navedite, kaksna bi bila korelacija
# ce bi predpostavili posevno rotacijo)

# IZPIS
# com = Hoffmanov indeks kompleksnosti --> kako dobro spremenljivka "locuje" med faktorjema 
# oz. koliko faktorja lezita na doloecni spremenljivki
# h2 in u2 bomo interpretirali kasneje
# sum of squared loadings: absolutno pojasnjena varianca
# proportion var: delez pojasnjene variance
# cumulative var: kumulative deleza pojasnjene variance
# proportion explained: koliko od tega kar pojasnimo z izbranim stevilom faktorjev pojasnimo s posameznim faktorjem

# korelacije na podlagi modela
# na diagonali je h2 = komunialitete 
# (tisti del variabilnosti, ki je pojasnjen s skupnimi faktorji)
# (h2 naj bo nad 0.2, sicer recemo, da ta spremenljivka ni dobro pojasnjena s skupnim faktorskim modelom)
# za pravokotne rotacije
CM.varimax <- FA.varimax$loadings %*% t(FA.varimax$loadings) 
plot.mat(CM.varimax,  main = "")
# za posevne rotacije (potrebno je upostevati tudi korelacije med faktorji)
CM.oblimin <- FA.oblimin$loadings %*% FA.oblimin$r.scores %*% t(FA.oblimin$loadings)
plot.mat(CM.oblimin, main = "")

# odkloni korelacij na podlagi modela
# lahko uporabimo funkcijo FA.varimax$residual
# manjsi kot so odkloni, bolj se model prilega
# spodaj so torej reziduali, kar pomeni, da lahko ekvivalentno dobimo s FA.oblimin$residual
# na diagonali je u2 = unikvitete =  varianca spremenljivke, ki ni pojasnjena s skupinimi faktorji 
OCM <- R - CM.varimax
plotMat(OCM, main = "") 

# ZANIMIVOST
# Vrsta rotacije ne vpliva na prileganje modela.
sum(FA.oblimin$residual**2)
sum(FA.varimax$residual**2)

# INTERPRETACIJA
# Na vajah pokazemo vec resitev, v nalogo pa vkljucite
# samo tiste, ki so relevantne za vas primer.

# izpisemo pattern utezi (reg. koeficienti)
par(mfrow = c(2, 2))
plot.mat(FA.oblimin$loadings[,], main = "oblimin - pattern")
plot.mat(FA.varimax$loadings[,], main = "varimax - pattern")

# izpiseno stukturne utezi (kor. koeficienti)
# v primeru pravokotne rotacije so faktorske in strukturne utezi enake
plot.mat(FA.oblimin$Structure[,], main = "oblimin - strukturne")
plot.mat(FA.varimax$Structure[,], main = "varimax - strukturne")

# biplot
biplot(FA.oblimin, main = "oblimin")
biplot(FA.varimax, main = "varimax")

# ocena faktorskih vrednosti
# spdaj je uporabljena regresijska metoda
# v nalogi ni potrebno opisovati regresijske metode
# povejte samo, da ste uporabili regresijsko metodo
schData$F1_odprtost <- FA.varimax$scores[,1]
schData$F2_konservativnost <- FA.varimax$scores[,2]
#######################################################################################
#                            ANALIZA KORELACIJ
#######################################################################################
dev.off()

schData$L1_konservativnost <- rowMeans(schData[, konservativnost])
schData$L2_odprtost <- rowMeans(schData[, odprtost])

korelacije <- round(cor(schData[, c("L1_konservativnost", "L2_odprtost",
                                    "PC2_nekonservativnost", "PC1_odprtost",
                                    "F2_konservativnost", "F1_odprtost")]), 2)
plotMat(korelacije, main = "", mar = c(1, 15, 18, 1), clu = c(1,1,2,2,3,3))
# korelacije med spremenljivkamai so blizu nic, pri GK = 0 po definiciji
# glede na korelacije so spremenljivke ustrezno poimenovane, korelacije med 
# spremenljivkami (lik, FA, GK), ki naj bi merile isto latentno 
# spremenljivko so visoke 
##################################################################################################
#           POVEZANOST FAKTORJEV Z OSTALIMI SPREMENLJIVKAMI
##################################################################################################
# izberemo samo tiste enote, ki imajo vrednosti pri vseh obravnavanih spremenljivkah
tmp <- cbind(schData[com, c("F1_odprtost", "F2_konservativnost")], 
             podatki[com, c("spol", "izobrazba", "sektor")])

# narisemo povprecja po faktorjih
par(mfrow = c(1,2), mar = c(10, 5, 1, 1))
plotmeans(formula = tmp[, 1] ~ tmp[,"spol"], las = 2, ylab = "odprtost", xlab = "", ylim = c(-0.5, 0.5))
plotmeans(formula = tmp[, 2] ~ tmp[, "spol"], las = 2, ylab = "konservativnost", xlab = "", ylim = c(-0.5, 0.5))

t.test(tmp[, "F1_odprtost"] ~ tmp[, "spol"])
t.test(tmp[, "F2_konservativnost"] ~ tmp[, "spol"])

plotmeans(formula = tmp[, 1] ~ tmp[,"sektor"], las = 2, ylab = "odprtost", xlab = "", ylim = c(-0.5, 0.5))
plotmeans(formula = tmp[, 2] ~ tmp[, "sektor"], las = 2, ylab = "konservativnost", xlab = "", ylim = c(-0.5, 0.5))

leveneTest(F1_odprtost ~ sektor, data = tmp)
oneway.test(formula = F1_odprtost ~ sektor, data = tmp, var.equal = TRUE)
leveneTest(F2_konservativnost ~ sektor, data = tmp)
oneway.test(formula = F2_konservativnost ~ sektor, data = tmp, var.equal = TRUE)

par(mar = c(20, 5, 1, 1))
plotmeans(formula = F1_odprtost ~ izobrazba, data = tmp, las = 2, ylab = "odprtost", xlab = "", ylim = c(-2, 2))
plotmeans(formula = F2_konservativnost ~ izobrazba, data = tmp, las = 2, ylab = "konservativnost", xlab = "", ylim = c(-2, 2))

cor.test(tmp[, "F1_odprtost"], as.numeric(tmp[, "izobrazba"]), method = "spearman", use = "com", exact = FALSE) 
cor.test(tmp[, "F2_konservativnost"], as.numeric(tmp[, "izobrazba"]), method = "spearman", use = "com", exact = FALSE)
##################################################################################################
#           NARISEMO ENOTE V PROSTORU GLAVNIH KOMPONENT, POBARVANE PO SKUPINAH IZ RAZVRSCANJA
##################################################################################################
# nastavimo seme za dosego istih rezultatov
set.seed(123)

# izvedemo razvrscanje v skupine (nehierahicno, K-means)
km <- kmeans(x = scale(schData[,Sch]), centers = 3, nstart = 1000) 

# narisemo povprecja za spremenljivke o opravicljivosti po skupinah
par(mar = c(12, 5, 1, 1))
tmp <- scale(schData[Sch])
plotMeans(tmp, by = km$cluster, ylim = c(-2, 2), plotLegend = FALSE) 

# poimenujemo skupine in imena shranimo v podatke
schData$razKMk3 <- factor(km$cluster, labels = c("podpovprecno odprti", 
                                                 "podpovprecno konservativni", 
                                                 "univerzalisti"))

# nastavimo kot faktor
schData$razKMk3 <- factor(schData$razKMk3, labels = levels(schData$razKMk3))

legend(...)
# V nalogi interpetirajte enega izmed spodnjih prikazov.

# narisemo enote v prostoru faktorjev, pobarvane glede na rezultat iz razvrscanja v skupine
# ce imate 3 ali vec faktorjev (ali glavnih komponent) uporabite funkcijo pairs
par(mar = c(5, 5, 1, 1))
plot(schData[, c("F1_odprtost", "F2_konservativnost")], 
     xlab = "odprtost",  
     ylab = "konservativnost", 
     col = schData[, "razKMk3"],
     pch = 1, 
     cex = 0.5, 
     asp = TRUE)
legend("topleft", legend = levels(schData$razKMk3), col = 1:3, pch = 16)

# narisemo enote v prostoru dveh glavnih komponent, pobarvane glede na rezultat iz razvrscanja v skupine 
plot(schData[, c("PC1_odprtost", "PC2_nekonservativnost")], 
      xlab = "1 GK (odprtost) (22 %)", 
      ylab = "2 GK (nekonservativnost) (20 %)",
      col = schData[, "razKMk3"],
      pch = 16, 
      cex = 0.5, 
      asp = TRUE)
par(xpd=NA)
legend("topleft", legend = levels(schData$razKMk3), col = 1:3, pch = 16)
