library("lavaan")
library("semTools")
library("semPlot")
library("foreign") 
library("car")
library("psych")
library("blockmodeling")
library("multiUS") # preverite, ce uporabljate najnovejso verzijo
# install.packages("multiUS", repos="http://R-Forge.R-project.org")

# Nekaj dodatnega gradiva o uporabi paketka lavaan lahko najdete na spodnjih povezavah.
# http://lavaan.ugent.be/tutorial/index.html
# https://www.jstatsoft.org/article/view/v048i02
# https://stats.idre.ucla.edu/r/seminars/rcfa/

#######################################################################################
#                            PRIPRAVA PODATKOV
#######################################################################################

# uvoz podatkov SPSS
podatkiVseDrzave <- read.spss("../ESS9e03.sav",
                              use.value.labels = TRUE,
                              to.data.frame = TRUE,
                              use.missings = TRUE,
                              reencode = TRUE)

# IZBOR DRZAVE ------------------------------------------------------------

dve.drzavi <- podatkiVseDrzave[podatkiVseDrzave$cntry %in% c("Slovenia", "Estonia"),]
table(dve.drzavi$cntry)
rm(podatkiVseDrzave)

# IZBOR SPREMENLJIVK ------------------------------------------------------

dve.drzavi <- renameVar(dve.drzavi, renames = list(
  "gndr" = "spol",
  "cntry" = "drzava",
  "eduyrs" = "solanje",
  "impdiff" = "drugacnost", 
  "impsafe"= "varnost", 
  "impfun" = "uzitek", 
  "ipadvnt" = "vznemerljivost", 
  "impfree" = "svoboda", 
  "ipgdtim" = "zabava", 
  "ipmodst" = "poniznost", 
  "ipstrgv" = "obramba", 
  "imptrad" = "tradicija", 
  "ipcrtiv" = "kreativnost", 
  "ipfrule" = "ubogljivost", 
  "ipbhprp" = "sprejemljivost",
  "imbgeco" = "gospodarstvo", 
  "imueclt" = "kultura", 
  "imwbcnt" = "bivanje"
))

Sch <- c("drugacnost", "varnost", "uzitek", "vznemerljivost", "svoboda", "zabava", "poniznost", "obramba", "tradicija", "kreativnost", "ubogljivost", "sprejemljivost")
priseljenci <- c("gospodarstvo", "kultura", "bivanje")

# izberemo podatke, ki vsebujejo samo potrebne spremenljivke
data_all <- dve.drzavi[, c(Sch, priseljenci, "spol", "solanje", "drzava")]
dataSLOEST <- data_all

# REKODIRANJE SPREMENLJIVK ------------------------------------------------

# priseljenci
dataSLOEST$gospodarstvo <- Recode(dataSLOEST$gospodarstvo, "'Good for the economy' = 10; 'Bad for the economy' = 0;")
dataSLOEST$kultura <- Recode(dataSLOEST$kultura, "'Cultural life enriched' = 10;'Cultural life undermined' = 0;")
dataSLOEST$bivanje <- Recode(dataSLOEST$bivanje, "'Better place to live' = 10; 'Worse place to live' = 0;")

dataSLOEST$gospodarstvo <- as.numeric(as.character(dataSLOEST$gospodarstvo))
dataSLOEST$kultura <- as.numeric(as.character(dataSLOEST$kultura))
dataSLOEST$bivanje <- as.numeric(as.character(dataSLOEST$bivanje))

# vrednote
for (i in 1:length(Sch)) {
  dataSLOEST[, Sch[i]] <- Recode(dataSLOEST[, Sch[i]], "
                                                  'Very much like me' = 6;
                                                  'Like me' = 5;
                                                  'Somewhat like me' = 4;
                                                  'A little like me' = 3;
                                                  'Not like me' = 2;
                                                  'Not like me at all' = 1
                                                  ")
  # pretvorba iz ordinalne v numericno spremenljivko
  dataSLOEST[, Sch[i]] <- as.numeric(as.character(dataSLOEST[, Sch[i]]))
}

# spol
dataSLOEST$spol <- car::recode(dataSLOEST$spol, "'Male' = 1; 'Female' = 0")
dataSLOEST$spol <- as.numeric(as.character(dataSLOEST$spol))
table(dataSLOEST$spol)

# solanje
dataSLOEST$solanje <- as.numeric(as.character(dataSLOEST$solanje))

# izberemo podatke samo za Slovenijo
dataSLO <- dataSLOEST[dataSLOEST$drzava == "Slovenia",]
# options(scipen=999)

# PREDSTAVITEV DODATNIH SPREMENLJIVK --------------------------------------

par(mfrow = c(1, 3))
for (i in 1:length(priseljenci)){
  histNorm(dataSLO[, priseljenci[i]], 
           breaks = -1:10+0.5, 
           main = priseljenci[i], 
           xlab = "strinjanje", 
           ylab = "frekvenca")
}
round(data.frame(describe(dataSLO[, priseljenci]))[, c(2, 3, 4, 5, 8, 9, 10, 11, 12)], 2)

dev.off(); plotMat(cor(dataSLO[, priseljenci], use = "com"), main = "")

hist(dataSLO$solanje)

View(t(dve.drzavi[as.numeric(as.character(dve.drzavi$solanje)) %in% 41, ]))
dataSLO$solanje[dataSLO$solanje %in% 41] <- NA
hist(dataSLO$solanje, main = "", xlab = "leta šolanja", ylab = "frekvenca")
round(data.frame(describe(dataSLO[, "solanje"]))[, c(2, 3, 4, 5, 8, 9, 10, 11, 12)], 2)

# KOMENTAR: Preverjanje primernosti podatkov
# - da so vsaj nekatere izmed spremenljivk med sabo korelirane
# - veèrazsežna normalna porazdelitev
# - ustrezne merske lestvice
# - velikost vzorca

#######################################################################################
#                            VAJA 1: POTRJEVALNA FAKTORSKA ANALIZA
#######################################################################################
# za definiranje faktorjev uporabljamo oznako =~ ki pomeni "se izraza skozi"
# na levo stran napisemo ime faktorja, na desno pa imena manifestnih spremenljivk
modelCfa <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba
priseljenci =~ gospodarstvo + kultura + bivanje
"

# da bi zagotovili identifikabilnost modela, moramo
#---a) nastaviti eno fiksno utez za vsak faktor (privzeto)
#---b) standardizirati variance odklonov latentnih spremenljivk (std.lv = FALSE)
# dodatni paramteri so:
#---a) mimic = "Mplus"; za zeljeno obliko izpisa
#---b) orthogonal; FALSE za nepravokotne faktorje, TRUE za pravokotne
#---c) estimator; pogosto DWLS, ko imamo ordinalne spremenljivke
fitCfa <- cfa(modelCfa, 
              data = dataSLO, 
              orthogonal = FALSE, 
              estimator = "DWLS", 
              ordered = c(Sch, priseljenci)) 

# mere prileganja lahko izpisemo z uporabo funkcije fitMeasures
# RMSEA = v kaksni meri se model dobro prilega vrednostim na populaciji (<0.06)
# SRMR = razlike med ocenjenimi in "modelskimi" korelacijami (<0.08)
# CFI = primerja ocenjen model z nultim modelom (brez korelacij) (>0.95)
# TLI = podobno kot CFI, le da kontrolira za kompleksnost modela (>0.95)
fitMeasures(fitCfa, fit.measures = c("rmsea", "srmr", "cfi", "tli"))

# ce se model slabo prilega podatkom, potem lahko razmislimo o dodajanju nekaterih sprostitev
# razmislimo o tistih sprostitvah, pri katerih je vrednost mi (modification index) najvecja
# mi je pricakovano zmanjsanje statistike hi2, ce sprostimo izbran parameter
# EPC (expected parameter change) je pricakovana sprememba parametra
# zadnji trije stolpci so razlicne standardizirane vrednosti EPC
# (sepc.nox = standardizirane vrednosti vseh, razen eksogenih spremenljivk)
modInd <- modificationindices(fitCfa)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)

modelCfa2 <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + bivanje
"
# naredimo nov model in pogledamo indekse prileganja
fitCfa2 <- cfa(modelCfa2, data = dataSLO, orthogonal = FALSE, estimator = "DWLS", ordered = c(Sch, priseljenci))
fitmeasures(fitCfa2, fit.measures = c("srmr","rmsea","tli","cfi"))

#modification indices
modInd <- modificationindices(fitCfa2)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)

# pogledamo rezultate modelov
#---a) fit.measures; kako dobro se model prilega
#---b) standardized; vrne standardizirane vrednosti koeficientov
summary(fitCfa2, fit.measures = TRUE, standardized = TRUE)

# KOMENTAR: prileganje modela
# Model Test User Model: Ho: kovariancna matrika, ocenjena na podlagi modela je enaka populacijski.
# Model Test Baseline Model: Ho: kovariancna matrika nultega modela je enaka populacijski kovariancni matriki.

# KOMENTAR: standardizirane vrednosti
# Std.lv = standardizirane vrednosti latentnih spremenljivk
# Std.all = standardizirane vrednosti vseh spremenljivk
# standardizirane vrednosti kovarianc so korelacije

# narisemo resitev
# what; kaj narisati, glej ?semPaths
# whatLabels; kaj napisati na povezave, obicajno "est" ali "std"
# intercepts; ali naj izpise tudi presecisca?
semPaths(fitCfa2, what = 'path', whatLabels = 'est')
semPaths(fitCfa2, what = 'path', whatLabels = 'std')
semPaths(fitCfa2, what = 'std', whatLabels = 'std', layout = "circle", thresholds = FALSE, intercepts = FALSE)

# preverimo, ali se bolj kompleksen model bolje prilega podatkom
# Ho: modela se enako dobro prilegata podatkom
anova(fitCfa2, fitCfa)

# pogledamo moznost za omejitev varianc latentnih spremenljvk
fitCfa.var <- cfa(modelCfa2, data = dataSLO, estimator = "DWLS", std.lv = TRUE, ordered = c(Sch, priseljenci))
summary(fitCfa.var, fit.measures = TRUE, standardized = TRUE)
#######################################################################################
#                            VAJA 2: MIMIC MODEL
#######################################################################################
modelMimic <- "
# latentne spremenljivke
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba
# regresija (oznaka ~ oznacuje linearno regresijo, is regressed on')
konservativnost ~ solanje + spol
"

fitMimic <- sem(modelMimic, data = dataSLO, estimator = "DWLS", ordered = Sch)
fitmeasures(fitMimic, fit.measures = c("srmr","rmsea","tli","cfi"))
# ce je prileganje modela ustrezno, potem ne dodajamo sprostitev
modInd <- modificationindices(fitMimic)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)
summary(fitMimic, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fitMimic, what = 'std', whatLabels = 'std', intercepts = FALSE, thresholds = FALSE)
#######################################################################################
#                            VAJA 3: POLNI SEM
#######################################################################################
modelFullSem <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + bivanje

# regresija
priseljenci ~ odprtost + konservativnost
priseljenci ~ solanje + spol
odprtost ~ solanje + spol
konservativnost ~ solanje + spol
"

fitFullSem <- sem(modelFullSem, data = dataSLO, estimator = "DWLS", ordered = c(Sch, priseljenci)) 
fitmeasures(fitFullSem,fit.measures = c("srmr","rmsea","tli","cfi"))
modInd <- modificationindices(fitFullSem)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)

modelFullSemDopolnjen <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + bivanje

# regresija
priseljenci ~ odprtost + konservativnost
priseljenci ~ solanje + spol
odprtost ~ solanje + spol
konservativnost ~ solanje + spol

# dodatne sprostitve (~~ oznacuje korelacije napak)
odprtost ~~ konservativnost
"

fitFullSemDopolnjen <- sem(modelFullSemDopolnjen, data = dataSLO, estimator = "DWLS", ordered = c(Sch, priseljenci)) 
fitmeasures(fitFullSemDopolnjen,fit.measures = c("srmr","rmsea","tli","cfi"))
modInd <- modificationindices(fitFullSemDopolnjen)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)

summary(fitFullSemDopolnjen, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
# KOMENTAR
# covariances med latentnimi spremenljivkami so sedaj korelacije napak (parcialne korelacije)
# torej del korelacij, ki ni pojasnjen z ostalimi spremenljivkami
# variances napak latentnih spremenljivk je del variance, ki ni pojasnjen z modelom
# torej  1 - (std. variances napak) = R2

modelFullSemPosr <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + poniznost + tradicija + ubogljivost + sprejemljivost + obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + bivanje

# regresija
priseljenci ~ odprtost + b*konservativnost
priseljenci ~ neposredni*solanje + spol
odprtost ~ solanje + spol
konservativnost ~ a*solanje + spol

# dodante sprostitve
odprtost ~~ konservativnost

# posredni in skupni ucinki (:= lahko beremo kot 'definira' ali 'je enako')
posredni := a*b
skupni := neposredni + posredni
"

fitFullSemPosr <- sem(modelFullSemPosr, data = dataSLO, estimator = "DWLS", ordered = c(Sch, priseljenci)) 
fitmeasures(fitFullSemPosr, fit.measures = c("srmr","rmsea","tli","cfi"))
modInd <- modificationindices(fitFullSemPosr)
subset(modInd[order(modInd$mi, decreasing = TRUE),], mi > 10)

summary(fitFullSemPosr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

semPaths(fitFullSemPosr, 
         what = 'std', 
         whatLabels = 'std', 
         intercepts = FALSE, 
         layout = "circle",
         thresholds = FALSE)

#######################################################################################
#                            VAJA 4: MERSKA ENAKOVREDNOST
#######################################################################################
dve_skupini <- cfa(modelCfa2, 
                          data = dataSLOEST, 
                          orthogonal = FALSE, 
                          estimator = "DWLS", 
                          std.lv = TRUE, 
                          group = "drzava") 
par(mfrow = c(1, 2)); semPaths(dve_skupini, 
                               what = 'path', 
                               whatLabels = 'est', 
                               intercepts = FALSE, 
                               thresholds = FALSE,
                               layout = "circle")

# funkcija measurmentInvariance preverja prileganje modelov
# z razlicnimi omejitvami vrednosti parametrov 
# (tj. razlicnimi vrstami enakovrednosti)
# dolociti moramo spremenljivko, ki doloca skupine
measurementInvariance(model=modelCfa2, 
                      data = dataSLOEST, 
                      group = "drzava",
                      estimator = "DWLS",
                      orthogonal = FALSE, 
                      fit.measures = c("srmr","rmsea","tli","cfi"), 
                      ordered = c(Sch, priseljenci))

# KOMENTAR
# CONFIGURAL / konfiguralna = enake manifestne spremenljivke
# LOADINGS / metricna = lahko primerjamo regresijske koeficiente, korelacije, kovariance
# MEANS / skalarna = lahko primerjamo povprecja

# Delno metricno enakovrednost lahko dosezemo tako, da fiksiramo
# vrednosti parametrov za vsaj dve manifestni spremenljivki
# pri vsaki latentni spremenljivki.

# V nasem primeru imamo kar nekaj utezi, ki bi lahko bile
# enake pri obeh faktorskih modelih, zato namesto, da bi napisali
# katere naj bodo enake, bomo napisali, katere naj bodo razlicne, vse 
# ostale pa bomo fiksirali v funkciji cfa s parametrom group.equal.
delnaMetricnaEnakovrednost <- "
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + c(a, b)*poniznost + c(c, d)*tradicija + c(e, f)*ubogljivost + sprejemljivost + c(g, h)*obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + c(i, j)*bivanje
"

delniMetricniModel <- cfa(delnaMetricnaEnakovrednost, 
                          group.equal = "loadings",
                          data = dataSLOEST, 
                          orthogonal = FALSE, 
                          estimator = "DWLS", 
                          std.lv = TRUE, 
                          group = "drzava",
                          ordered = c(Sch, priseljenci)) 
par(mfrow = c(1, 2)); semPaths(delniMetricniModel, 
                               what = 'path', 
                               whatLabels = 'est', 
                               intercepts = FALSE, 
                               thresholds = FALSE,
                               layout = "circle")

#######################################################################################
#                            VAJA 5: POLNI SEM MODEL ZA VEC SKUPIN
#######################################################################################
# Predpostavljamo delno mestricno enakovrednost.
modelFullSemPosrVecSkupin <- "
# latentne spremenljivke
odprtost =~ vznemerljivost + drugacnost + uzitek + zabava + kreativnost + svoboda 
konservativnost  =~ varnost + c(a, b)*poniznost + c(c, d)*tradicija + c(e, f)*ubogljivost + sprejemljivost + c(g, h)*obramba + vznemerljivost
priseljenci =~ gospodarstvo + kultura + c(i, j)*bivanje

# regresija
priseljenci ~ odprtost + b*konservativnost
priseljenci ~ solanje + neposredni*spol
odprtost ~ solanje + spol
konservativnost ~ solanje + a*spol

# dodatne sprostitve
odprtost ~~ konservativnost

# posredni in skupni ucinki
posredni := a*b
skupni := neposredni + posredni
"

modelFullSemVecSkupin <- sem(modelFullSemPosrVecSkupin, 
                             data = dataSLOEST, 
                             estimator = "DWLS", 
                             group = "drzava", 
                             group.equal = "loadings",
                             ordered = c(Sch, priseljenci)) 
fitmeasures(modelFullSemVecSkupin, fit.measures = c("srmr","rmsea","tli","cfi"))

summary(modelFullSemVecSkupin, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

par(mfrow = c(1, 2)); semPaths(modelFullSemVecSkupin, 
                               what = 'path',
                               whatLabels = 'est', 
                               intercepts = FALSE, 
                               thresholds = FALSE,
                               layout = "circle") 
