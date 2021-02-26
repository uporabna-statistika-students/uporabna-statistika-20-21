# Nalozite paketke, ce jih nimate namescrnih, jih namestite:
# Tools -> Install Packages
library("car")
library("blockmodeling")
library("foreign")
library("psych")
# Pred prvo uporabo namestite paketek multiUS iz repozitorija R-forge.
# Verjetno boste morali prej namestiti se paketke stats, CCA, MASS,e1071 in gplots (poglejte izpis).
# install.packages("multiUS", repos="http://R-Forge.R-project.org")
library("multiUS")
#######################################################################################
#                            PRIPRAVA PODATKOV
#######################################################################################
# uvoz podatkov SPSS
?read.spss
podatkiVseDrzave <- read.spss("../ESS9e03.sav",
                     use.value.labels = TRUE,
                     to.data.frame = TRUE,
                     use.missings = TRUE,
                     reencode = TRUE)

podatki <- podatkiVseDrzave[podatkiVseDrzave$cntry == "Slovenia",]
rm(podatkiVseDrzave)

# pogledamo, kako izgledajo podatki
View(podatki)

# pogledamo, kaj vsebuje kodirni list
names(attributes(podatki))
attributes(podatki)$names #imena spremenljivk
attributes(podatki)$variable.labels #opis spremenljivk

# posebej shranimo labels
varLabs <- attributes(podatki)$variable.labels

# izberemo nominalne in/ali ordinalne (kategoricne) spremenljivke
# spol - gndr
# zaposlitveni sektor - tporgwk
# izobrazba - edlvesi
catVarsEN <- c("gndr", "tporgwk", "edlvesi")
catVars <- c("spol", "sektor", "izobrazba")

# izeremo intervalne spremenljivke
# starost - agea
# preimenujemo v bolj clovesko ime
colnames(podatki)[which(colnames(podatki) == "agea")] <- "starost"
# spremenimo v stevilsko spremenljivko
podatki$starost <- as.numeric(as.character(podatki$starost))

# izberemo "vsebinske" spremenljivke
SchEn <- c("impdiff", "impsafe", "impfun",  "ipadvnt", "impfree", "ipgdtim", 
           "ipmodst", "ipstrgv", "imptrad", "ipcrtiv", "ipfrule", "ipbhprp")

# preimenujemo v slovenska imena
Sch <- c("drugaènost", "varnost", "užitek", "vznemerljivost", "svoboda", 
         "zabava", "ponižnost", "obramba", "tradicija", "kreativnost", 
         "ubogljivost", "sprejemljivost")

for (i in 1:length(SchEn)) {
  colnames(podatki)[which(colnames(podatki) %in% SchEn[i])] <- Sch[i]
}

# pogledamo podatke
head(podatki[, Sch])

# vrednosti bomo obravnavali kot stevilske (intervalne)
# zato jih rekoridamo in obrnemo lestvico
# (tako da bo vecja vrednost pomenila vec, manjsa pa manj)
for (i in 1:length(Sch)) {
  # rekodiranje
  podatki[, Sch[i]] <- Recode(podatki[, Sch[i]], "
                                                  'Very much like me' = 6;
                                                  'Like me' = 5;
                                                  'Somewhat like me' = 4;
                                                  'A little like me' = 3;
                                                  'Not like me' = 2;
                                                  'Not like me at all' = 1
                                                  ")
  # pretvorba iz ordinalne v numericno spremenljivko
  podatki[, Sch[i]] <- as.numeric(as.character(podatki[, Sch[i]]))
}

# pogledamo podatke
head(podatki[, Sch])

#######################################################################################
#                            TOCKA 1-2: PREDSTAVITEV PODATKOV IN REKODIRANJE
#######################################################################################
# spol
freqTab(podatki$gndr, cum = FALSE)
podatki$spol <- Recode(podatki$gndr, "'Male' = 'moški'; 'Female' = 'ženski'")
freqTab(podatki$spol, cum = FALSE)

# vrsta podjetja
freqTab(podatki$tporgwk, cum = FALSE)
# rekodiranje 
# Priporocilo po pravilu palca:
# - ce najmanjsa kategorija obsega manj kot 10 % enot, potem jo zdruzimo 
# - vedno upostevamo teorijo - zdruzitve morajo biti smiselne!
# - ne zdruzujemo kategorij ordinalnih spremenljivk
podatki$sektor <- car::Recode(
  podatki$tporgwk,
  "
  c('Central or local government', 'Other public sector (such as education and health)') = 'javni sektor';
  'A state owned enterprise' = 'podjetje v javni lasti';
  'A private firm' = 'podjetje v zasebni lasti';
   c('Self employed', 'Other', NA) = 'ostalo';
  "
)
freqTab(podatki$sektor, cum = FALSE)

# stopnja izobrazbe
freqTab(podatki$edlvesi)
# definiramo kot faktor in shranimo v novo spremenljivko
podatki$izobrazba <- factor(podatki$edlvesi)
# preverimo vrstni red faktorjev
levels(podatki$izobrazba)
# Other upostevano kot manjkajoco vrednost - 1 respondnet
podatki[podatki$izobrazba %in% "Other", "izobrazba"] <- NA
freqTab(podatki$izobrazba)

# graficne predstavitve kategoricnih spremenljivk
# spravimo v for zanko za vse kategoricne spremenljivke
par(mfrow = c(1, 3), mar = c(25,5,5,0))
for (i in 1:length(catVars)){
  barplot(table(podatki[, catVars[i]]),
          ylab = "frekvenca",
          las = 2)
}

# pogledamo se porazdelive stevilskih spremenljivk

# starost
povzetek <- as.data.frame(psych::describe(podatki[, "starost"]))
rownames(povzetek) <- c("starost")
round(povzetek[, c(2, 3, 4, 5, 8, 9, 10)], 2)

# histogram za starost
hist(podatki$starost,
     main = "PORAZDELITEV STAROSTI",
     ylab = "Frekvenca",
     xlab = "starost",
     freq = TRUE)

# vsebinske spremenljivke
# Skewness (asimetrija) in kurtosis (splscenost) po pravilu palca:
# - ce je absolutna vrednost manjsa od 1, potem spremenljivka bistveno ne odstopa od normalne.
# odstopa od normalne porazdelitve
# Koeficient sploscenosti je definiran tako, da ima std. normalna porazdelitev
# vrednost 0 (in ne 3).
povzetek <- as.data.frame(psych::describe(podatki[, Sch]))
round(povzetek[, c(2, 3, 4, 5, 8, 9, 10, 11, 12)], 2)

# histogram za vsebinske spremenljivke
par(mfrow = c(4, 3))
for (i in 1:length(Sch)) {
  histNorm(podatki[, Sch[i]],
    breaks = 0:6 + 0.5,
    main = Sch[i],
    xlab = "strinjanje", 
    ylab = "Frekvenca",
    ylim = c(1, 800))
}

#######################################################################################
#                            TOCKA 3: ANALIZA POVEZANOSTI
#######################################################################################
# povezanost spremenljivk o vrednotah

# izracunamo korelacije med spremenljivkami
# uporabimo funckcijo s privzetimi vrednostmi
cor(podatki[, Sch])
# upostevamo samo enote, ki imajo vrednosti pri vseh spremenljivkah
# pogledamo, koliksen deleu enot bomo upostevali na tak nacin
mean(complete.cases(podatki[, Sch]))
# izracunamo pearsonov koeficient korelacije
(R.p <- cor(podatki[, Sch], use = "complete.obs", method = "pearson"))

# narisemo korelacije
plot.mat(R.p, main = "Pearsonov koeficient korelacije")

# poskusite spremeniti vrstni red spremenljivk !
vrstni.red <- c(1,3,4,6,10,5,2,7,8,9,11,12)
plot.mat(R.p[vrstni.red, vrstni.red], main = "Pearsonov koeficient korelacije")

# izracunamo povprecja strinjanja za prvo spremenljviko o opravicljivosti po spolu
# lahko uporabimo funkcijo describeBy
# (v domaco nalogo vkljucite samo grafe)
describeBy(podatki[, Sch], podatki[, "spol"],  mat = TRUE)[,-c(1, 3, 8, 9)]

# povezanost spremenljivk o upraviciljivosti s kategoricnimi spremenljivkami lahko predstavimo graficno
par(mfrow = c(1, 2), oma = c(5, 2, 1, 1))
plotMeans(x = podatki[, Sch[vrstni.red]], by = podatki$spol, xlab = "", ylim = c(1, 6))
plotMeans(x = podatki[, Sch[vrstni.red]], by = podatki$sektor, xlab = "", ylim = c(1, 6), xleg = "bottomright")

korelacije <- data.frame("izobrazba (Spearman)" = cor(podatki[, Sch], as.numeric(podatki[, "izobrazba"]), method = "spearman", use = "com"),
                         "starost (Pearson)" = cor(podatki[, Sch], podatki[, "starost"], method = "pearson", use = "com"), check.names = FALSE)
rownames(korelacije) <- Sch
colnames(korelacije) <- c("izobrazba (Spearman)", "starost (Pearson)")
round(korelacije, 2)
#######################################################################################
#                            TOCKA 4: LIKARTOVE LESTVICE
#######################################################################################
# zdruzimo spremenljivke, ki naj bi merile isto stvar
# navadno to naredimo na podlagi teorije in nato preverimo s faktorsko analizo
# vcasih je dovolj pogledati ze korelacijsko matriko
odprtost <- c("drugaènost", "užitek", "vznemerljivost", "zabava", "kreativnost", "svoboda")
konservativnost  <- c("varnost", "ponižnost", "obramba", "tradicija", "ubogljivost", "sprejemljivost")

# izracunamo povprecja odgovorov po posameznih sklopih spremenljivk
# temu recemo spremenljivka Likertove lestvice
# (upostevamo razpolozljive vrednosti)
podatki$odprtost <- rowMeans(x = podatki[, odprtost], na.rm = TRUE)
podatki$konservativnost <- rowMeans(x = podatki[, konservativnost], na.rm = TRUE)

# pogledamo porazdelitev teh dveh spremenljivk
par(mfrow = c(1, 2))
histNorm(podatki$odprtost, xlim = c(1, 6), main = "odprtost", xlab = "strinjanje", ylab = "Frekvenca")
histNorm(podatki$konservativnost, xlim = c(1, 6), main = "konservativnost", xlab = "strinjanje", ylab = "Frekvenca")

# pogledamo porazdelitev teh dveh spremenljivk tabelaricno
povzetek <- data.frame(describe(podatki[, c("odprtost", "konservativnost")]))
rownames(povzetek) <- c("odprtost", "konservativnost")
round(povzetek[, c(2, 3, 4, 5, 8, 9, 10, 11, 12)], 2)

# preverimo domnevo o enakosti povprecij
t.test(x = podatki$konservativnost, y = podatki$odprtost, paired = TRUE)

# pogledamo korerelacijo med spremenljivkama Lik. lestvice
# ce imate vec kot 2 spremenljivki, lahko uporabite funkcijo pairs
# vrednosti prej "raztresemo"
tmp <- apply(podatki[, c("odprtost", "konservativnost")], 2, jitter, amount = 0.4)
plot(tmp[,1], tmp[,2], ylim = c(1, 6), xlim = c(1, 6), xlab = "odprtost", ylab = "konservativnost")

# preverimo odmnevo o povezanosti
cor.test(x = podatki$odprtost, y = podatki$konservativnost)

# pogledamo korelacijo med Lik spremenljivkama in starostjo 
plot(podatki$starost, jitter(podatki$odprtost, amount = 0.05), xlab = "starost", ylab = "odprtost")
cor.test(podatki$starost, podatki$odprtost, use = "compl")

plot(podatki$starost, jitter(podatki$konservativnost, amount = 0.05), xlab = "starost", ylab = "konservativnost")
cor.test(podatki$starost, podatki$konservativnost, use = "compl")

# pogledamo, kako se razlikujejo ocene po kategoricnih spremenljivkah
par(mfrow = c(1,2), mar = c(11, 5, 2, 2))
plotmeans(odprtost ~ spol, data = podatki, ylab = "povprecje", 
          xlab = "", main = "odprtost", las = 2, cex.axis = 0.7, ylim = c(1, 6))
plotmeans(konservativnost ~ spol, data = podatki, ylab = "povprecje", 
          xlab = "", main = "konservativnost", las = 2, cex.axis = 0.7, ylim = c(1, 6))

par(mfrow = c(1,2), mar = c(11, 5, 2, 2))
plotmeans(odprtost ~ izobrazba, data = podatki, ylab = "povprecje", xlab = "", main = "odprtost", las = 2, cex.axis = 0.7, ylim = c(1, 5))
plotmeans(konservativnost ~ izobrazba, data = podatki, ylab = "povprecje", xlab = "", main = "konservativnost", las = 2, cex.axis = 0.7, ylim = c(1, 5))

par(mfrow = c(1,2), mar = c(11, 5, 2, 2))
plotmeans(odprtost ~ sektor, data = podatki, ylab = "povprecje", xlab = "", main = "odprtost", las = 2, cex.axis = 0.7, ylim = c(1, 6))
plotmeans(konservativnost ~ sektor, data = podatki, ylab = "povprecje", xlab = "", main = "konservativnost", las = 2, cex.axis = 0.7, ylim = c(1, 6))

# ali so razlike statisticno znacilne?
t.test(podatki$odprtost ~ podatki$spol)
leveneTest(y = podatki$odprtost, group = podatki$sektor)
oneway.test(podatki$odprtost ~ podatki$sektor, var.equal = FALSE)
cor.test(podatki$odprtost, as.numeric(podatki$izob), method = "spearman", use = "com")

t.test(podatki$konservativnost ~ podatki$spol)
leveneTest(y = podatki$konservativnost, group = podatki$sektor)
oneway.test(podatki$konservativnost ~ podatki$sektor, var.equal = TRUE)
cor.test(podatki$konservativnost, as.numeric(podatki$izob), method = "spearman", use = "com")
#######################################################################################
#                            TOCKA 5: GRAF
#######################################################################################
dev.off()

# izberemo spremenljivko
var <- "sektor"
imena_spremenljivk <- c("odprtost", "konservativnost")
# izracunamo centroide po spolu
(agg <- aggregate(podatki[, imena_spremenljivk], by = list(podatki[, var]), FUN=mean, na.rm = TRUE))
tmp <- apply(podatki[, imena_spremenljivk], 2, jitter, amount = 0.07)
joint <- rbind(tmp, agg[, -1])
# narisemo v 2D prostoru, kot smo se naucili - s funkcijo jitter
# v primeru, da imamo tri spremenljivke, "plot" nadomestimo s "pairs"
plot(joint,
     xlab = "odprtost",
     ylab = "konservativnost",
     pch = 16,
     ylim = c(1, 6), xlim = c(1, 6),
     cex = c(rep(0.5, nrow(tmp)), rep(2, nrow(agg))),
     col = c(podatki[, var], agg[,1]))
# dodamo legendo
par(xpd=TRUE)
legend("topleft", legend = agg[,1], pch = 16, col = 1:nrow(agg))
#######################################################################################
#                            TOCKA 5: SHRANIMO PODATKE
#######################################################################################
# shranimo podatkovno datoteko
#saveRDS(podatki, file = "ESS_SLO_rekodirane")
#podatki <- readRDS("ESS_SLO_rekodirane")
###############################################
