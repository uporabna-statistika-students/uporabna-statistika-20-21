library("GDAtools")
library("foreign")
library("FactoMineR")
library("multiUS")
library("blockmodeling")

add_modified_rates <- function(mca){
  Q <- length(mca$call$quali)
  lambda_pseudo <- ((Q/(Q-1)) * (mca$eig[,"eigenvalue"] - (1/Q)))**2
  pos_lambda <- mca$eig[,"eigenvalue"] > (1/Q)
  S <- sum(lambda_pseudo[pos_lambda])
  modified_rate <- lambda_pseudo*pos_lambda/S*100
  cum_modified_rate <- cumsum(modified_rate)
  return(data.frame(mca.rez$eig, "modified_rate" = modified_rate, "cum_modified_rate" = cum_modified_rate))
}

# POLITICNE STRANKE -------------------------------------------------------

# uvoz podatkov SPSS
podatki <- readRDS("../Vaje 1/ESS_SLO_rekodirane")

# uporabljali bomo samo enote, ki imajo vrednosti pri vseh spremenljivkah
com <- complete.cases(podatki[, c("prtvtfsi", "imsmetn")])
data <- podatki[com, c("prtvtfsi", "imsmetn")]

# Koga ste volili na zadnjih parlamentarnih volitvah?
freqTab(data$prtvtfsi)
data <- data[!data$prtvtfsi %in% "PS - Pozitivna Slovenija",]
data$prtvtfsi <- as.character(data$prtvtfsi)
freqTab(data$prtvtfsi)

# Kaj menite, v kolikšnem obsegu naj Slovenija dovoli priseljevanje ljudi s
# podobnim narodnostnim izvorom, kot ga ima veèina prebivalcev Slovenije? 
freqTab(data$imsmetn)
data$imsmetn <- car::recode(data$imsmetn, "'Allow many to come and live here' = 'Mnogim osebam';
                                            'Allow some' = 'Nekaterim osebam';
                                            'Allow a few' = 'Nekaj osebam';
                                            'Allow none' = 'Nikomur'")
freqTab(data$imsmetn)

# korespondencna analiza
# kontingencna tabela
kont <- table(data)
dim(kont)
# izvedemo korespondencno analizo
mca.rez <- CA(kont)
# pogledamo koliko skupne inercije pojasnimo s 
# posameznimi dimenzijami 
mca.rez$eig

# PRIMER TO SEM JAZ -------------------------------------------------------

# nalozimo podatke
tsj <- read.table("tsj_vaje.txt", header = TRUE)
# pogledamo zapis podatkov
head(tsj)
# pogledamo posamezne porazdelitve
barplot(table(tsj$st_znakov_rec))
barplot(table(tsj$spol))
barplot(table(tsj$starost))
par(oma = c(10, 0, 0, 0))
barplot(sort(table(tsj$krovna_kat), decreasing = T), las = 2)

# naredimo Burtovo tabelo
(burtova <- burt(data = tsj))
plotMat(burtova, mar = c(5, 20, 20, 1), main = "")
# izvedemo korespondencno analizo
par(mfrow = c(2, 2))

# slika, ki jo vrne spodnji ukaz prikazuje korelacijo med 
# spremenljivkami ter dimenzijmai MCA
mca.rez <- MCA(tsj, ncp = 16, method = "Indicator")

# pogledamo, koliko pojasnimo s posamezno dimenzijo in se odlocimo za primerno stevilo dimenzij
# eigenvalue - lastne vrednosti
# percentage of variance - delez pojasnjene variabilnosti
# cumulative percentage of variance - kumulativen delez pojasnjene variabilnosti
# modified_rate - delez pojasnjene variabilnosti, po tem, ko upostevamo 
# samo tiste dimenzije, ki katerih inercije so visje od 1/st_spremenljivk inercije
round(add_modified_rates(mca.rez), 2)

# narisemo rezultat; invisible = "ind" pomeni "naredi individualne vrednosti nevidne"
plot(mca.rez, invisible = "ind")

# dodatno branje
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/
