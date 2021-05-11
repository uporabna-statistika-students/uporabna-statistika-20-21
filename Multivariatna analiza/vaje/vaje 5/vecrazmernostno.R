library("XML")
library("MASS")

# MESTA -------------------------------------------------------------------

#### podatki razdalj med mesti
# Med evropskimi mesti - vir: http://www.mapcrow.info/european_travel_distance.html
# nalozimo podatke kar s spleta
euroDist <- readHTMLTable(doc="http://www.mapcrow.info/european_travel_distance.html")
# priprava podatkov
# shranimo matriko brez prvega stolpca (imena mest)
euroDist <- euroDist[[1]][,-1]
# dodamo imena mest v imena vrstic
rownames(euroDist) <- colnames(euroDist)
# matriko pretvorimo v matriko tipa "dist"
euroDist <- as.dist(euroDist)
# na matriki razdalj izvedemo klasicno vecrazmernostno lestvicenje
# d = matrika razdalij
# k = stevilo dimenzij
# eig = ali naj izracuna tudi lastne vrednosti?
cmdsEuro <- cmdscale(d = euroDist, k = 2, eig = TRUE)

# STRESS1 izraèunamo takole:
sum((dist(cmdsEuro$points) - euroDist)**2/dist(cmdsEuro$points)**2)

# narisemo mesta v 2D prostoru, z mnozenjem matrik spremenimo predznake
plot(cmdsEuro$points%*%diag(c(-1,-1)), asp=1, pch="", ylab = "sever-jug", xlab = "zahod-vzhod")
par(xpd=NA)
text(x = cmdsEuro$points%*%diag(c(-1,-1)), labels = rownames(cmdsEuro$points))
# ocenjevanje prileganja
# koliko pojasnimo s posamezno dimenzijo
plot(cmdsEuro$eig, type="o") 
# kako se orig. razdalje med tockami razlikujejo od razdalij za 2 dimenziji 
DcmdsEuro <- dist(cmdsEuro$points)
plot(DcmdsEuro ~ euroDist)
abline(a=0,b=1, col="red")

# POLITICNE STRANKE 1 ------------------------------------------------------

# nalozimo podatke / 24ur.com podatki
stranke <- read.table("stranke.txt", sep = "\t", header = TRUE)
# skrajsamo imena vrstic
rownames(stranke) <- substr(rownames(stranke), start = 1, stop = 15)
# transponiramo matriko, tako da so po vrsticah enote
stranke <- t(stranke)
# izracunamo matriko razdalij med enotami
razdalje <- dist(stranke, method = "binary")

# izvedemo vecrazmernostno lestvicenje 
cmsStranke <- cmdscale(d = razdalje, k = 2, eig = TRUE)
# izvedemo ne-metricno vecrazmernostno lestvicenje
isoStranke <- isoMDS(d = razdalje, k = 2)

# narisemo lastne vrednosti za metricno lestvicenje (pri metricnem tega ni)
plot(cmsStranke$eig, type = "b")
DcmdsStranke <- dist(cmsStranke$points)

# narisemo odnos med origniralnimi razdaljami med strankami in 
# razdaljami med strankami z upostevanjem samo prvih dveh dimenzij
par(mfrow = c(1, 2))
plot(DcmdsStranke ~ razdalje, 
     ylab = "razdalje med strankami (MDS)", 
     xlab = "razdalje med strnakami (ORIG)",
     main = "metricno", 
     ylim = c(0, 1), xlim = c(0,1))
abline(a=0, b=1, col="red")

plot(dist(isoStranke$points) ~ razdalje, 
     ylab = "razdalje med strankami (MDS)", 
     xlab = "razdalje med strnakami (ORIG)",
     main = "ne-metricno", 
     ylim = c(0, 1), xlim = c(0,1))
abline(a=0, b=1, col="red")

# narisemo resitev
plot(cmsStranke$points, 
     asp=1, 
     col = "white", 
     xlab = "dim 1 (levo-desno)", ylab = "dim 2 (?)",
     ylim = c(-0.5, 0.5), xlim = c(-0.5, 0.5),
     main = "metricno")
text(x = cmsStranke$points, labels = rownames(cmsStranke$points))

plot(isoStranke$points, 
     asp=1, 
     col = "white", 
     xlab = "dim 1 (levo-desno)", ylab = "dim 2 (?)",
     ylim = c(-0.5, 0.5), xlim = c(-0.5, 0.5),
     main = "nemetricno")
text(x = isoStranke$points, labels = rownames(cmsStranke$points))

# POLITICNE STRANKE 2 -----------------------------------------------------

# How would you personally estimate distances between pairs of parties in the political space?
stranke <- read.table("stranke2.txt", sep = "\t", header = TRUE, dec = ",")
rownames(stranke) <- stranke[, 1]
stranke <- stranke[, -1]
# ker so vrednosti podobnosti (namesto razlicnosti, jih trensformiramo)
strankeD <- 300 - stranke

# izvedemo ne-metricno vecrazmernostno lestvicenje
mdsStranke <- isoMDS(d= as.dist(strankeD), k = 2)
# narisemo resitev
plot(mdsStranke$points, 
     asp=1, 
     col = "white", 
     xlab = "dim 1 (levo-desno)", ylab = "dim 2 (?)",
     ylim = c(-400, 400), xlim = c(-400, 400),
     main = "nemetricno")
text(x = mdsStranke$points, labels = rownames(mdsStranke$points))
# narisemo razmerje med dejsnskimi rezlikami in reproduciranimi (na podlagi vecrazmernostnega lestvicenja)
# zaradi redukcije stevila dimenzij, ne pricakujemo, da bodo tocke na ravni crti
plot(dist(mdsStranke$points) ~ as.dist(strankeD))

