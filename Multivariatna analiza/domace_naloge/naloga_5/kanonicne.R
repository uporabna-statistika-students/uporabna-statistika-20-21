library("MASS")
library("blockmodeling")
library("CCA")
library("multiUS")
source("plotCCA.R")

# PRIPRAVA PODATKOV -------------------------------------------------------

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

# KANONICNE KORELACIJE ----------------------------------------------------

# pogledamo korelacijsko matriko
plot.mat(cor(schData), main = "")

# izracunamo kanonicno resitev (linearne kombinacije in korelacije)
# na standardiziranih podatkih
# x - prvi sklop spremenljivk
# y - drugi sklop spremenljivk
ccRes <- cancorPlus(x = scale(schData[, odprtost]), 
                    y = scale(schData[, konservativnost]), 
                    useCCApackage = TRUE)

# eigenvalues - lastne vrednosti
# % - delez pojasnjene variabilnosti vseh spremenljivk s kanonicnimi spremenljivkami
# Cor - kanonicne korelacije 
# Sq. Cor. - vrednost mere prekrivanja sklopov 
round(ccRes$eigModel, 3)

# preverimo, katere korelacije so statisticno znacilno razlicne od 0 
# (pri doloceni stopnji statisticne znacilnosti)
# Ho: ta kanonicna korelacija in vse naslednje so enake nic.
# Wilks Lambda - Wilksonova lamba (produkt vrednosti 1- Sq. Cor)
round(ccRes$sigTest, 3)

# pogledamo regresijske koeficiente za kanonicno spremenljivko prve mnozice spremenljivk
plot.mat(ccRes$xcoef, mar = c(2, 6, 2, 2))

# pogledamo regresijske koeficiente za kanonicno spremenljivko druge mnozice spremenljivk
plot.mat(ccRes$ycoef, mar = c(2, 6, 2, 2), main = "")

# pogledamo korelacije med vrednostmi kanonicnih spremenljivk (prve mnozice spremenljivk) 
# in spremenljivk prve mnozice spremenljivk
plot.mat(ccRes$scores$corr.X.xscores, mar = c(2, 6, 2, 2), main = "")

# pogledamo korelacije med vrednostmi kanonicnih spremenljivk (druge mnozice spremenljivk) 
# in spremenljivk druge mnozice spremenljivk
plot.mat(ccRes$scores$corr.Y.yscores, mar = c(2, 6, 2, 2), main = "")

# PRVA KANONICNA RESITEV
#  (ne)varnost  <-----> nehedonizem (nepomembna varnost, nepomemben hedonzem in pomembna vznemerljivost)
# DRUGA KANONICNA RESITEV
#  (ne)podrejenost <-----> vznemerljivost (nepoembna podrejenost, pomembna drugacnost)
# ....
plotCCA(ccRes, 
        yTitle = "konservativnost", 
        xTitle = "odprtost", 
        inColors = TRUE, 
        scaleLabels = 1/2,
        nDigits = 2,
        what = "cor")

# ZANIMIVOSTI
# povprecen delez pojasnjene variabilnosti sklopa vsebinskih spremenljivk 
# s kanonicnimi spremenljivkami
# (s kanonicnimi spremenljivkami pojasnimo Xx variabilnosti originalnih spremenljivk)
Xx <- cbind(as.matrix(round(apply(ccRes$scores$corr.X.xscores^2, 2, mean), 3)), 
      cumsum(round(apply(ccRes$scores$corr.X.xscores^2, 2, mean), 3)))
rownames(Xx) <- paste0("Kanonicna spremenljivka ", 1:nrow(Xx), ":")
colnames(Xx) <- c("R2", "cum R2")
Xx

Yy <- cbind(as.matrix(round(apply(ccRes$scores$corr.Y.yscores^2, 2, mean), 3)), 
            cumsum(round(apply(ccRes$scores$corr.Y.yscores^2, 2, mean), 3)))
rownames(Yy) <- paste0("Kanonicna spremenljivka ", 1:nrow(Yy), ":")
colnames(Yy) <- c("R2", "cum R2")
Yy
# v primeru, ko imata sklopa vsebinskih spremenljivk razlicno stevilo spremenljivk, 
# kanonicne spremenljivke sklopa, ki ima manj spremenljivk,
# pojasnijo vso variabilnost vsebinskih spremenljivk tega sklopa

# povprecen delez pojasnjene variabilnosti sklopa 
# vsebinskih spremenljivk s kanonicnimi spremenljivkami iz drugega sklopa
Yx <- cbind(as.matrix(round(apply(ccRes$scores$corr.Y.xscores^2, 2, mean), 3)), 
            cumsum(round(apply(ccRes$scores$corr.Y.xscores^2, 2, mean), 3)))
rownames(Yx) <- paste0("Kanonicna spremenljivka ", 1:nrow(Yx), ":")
colnames(Yx) <- c("R2", "cum R2")
Yx

Xy <- cbind(as.matrix(round(apply(ccRes$scores$corr.X.yscores^2, 2, mean), 3)), 
            cumsum(round(apply(ccRes$scores$corr.X.yscores^2, 2, mean), 3)))
rownames(Xy) <- paste0("Kanonicna spremenljivka ", 1:nrow(Xy), ":")
colnames(Xy) <- c("R2", "cum R2")
Xy
# vsaka nadaljna kanonicna spremenljivka pojasni manj varibilnosti "vsebinskih"
# spremenljivk drugega sklopa

