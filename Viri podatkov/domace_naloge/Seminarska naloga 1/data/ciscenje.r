library(tidyverse)
library(lubridate)
library(tidyverse)
library(scales)
library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(RDS)



pn2018 <- read.csv("pn2018.csv", sep=";")
pn2019 <- read.csv("pn2019.csv", sep=";")
pn2020 <- read.csv("pn2020.csv", sep=";")

podatki_pn = rbind(pn2018, pn2019, pn2020)

podatki_pn <- podatki_pn %>% filter(Povzrocitelj == "POVZROČITELJ")


####### TEGA SPODI POL MISLIM DA NISEM UPORABILA

# podatki_pn <- podatki_pn %>% select(ZaporednaStevilkaPN, KlasifikacijaNesrece, UpravnaEnotaStoritve,
#                   DatumPN, UraPN, VNaselju, VrstaCesteNaselja, OpisKraja, VzrokNesrece,
#                   TipNesrece, VremenskeOkoliscine, GeoKoordinataX, GeoKoordinataY, Povzrocitelj,
#                   Starost, Spol, PoskodbaUdelezenca, UporabaVarnostnegaPasu, VozniskiStazVLetih, 
#                   VrednostAlkotesta)

str(podatki_pn)

# urejanje objektov

podatki_pn$KlasifikacijaNesrece <- factor(podatki_pn$KlasifikacijaNesrece, 
                                      levels = c("Z MATERIALNO ŠKODO", 
                                                 "Z LAŽJO TELESNO POŠKODBO",
                                                 "S HUDO TELESNO POŠKODBO",
                                                 "S SMRTNIM IZIDOM"))

podatki_pn$UpravnaEnotaStoritve <- factor(podatki_pn$UpravnaEnotaStoritve)

podatki_pn$DatumPN <- dmy(podatki_pn$DatumPN)

podatki_pn$VNaselju <- factor(podatki_pn$VNaselju)
podatki_pn$VrstaCesteNaselja <- factor(podatki_pn$VrstaCesteNaselja)
podatki_pn$OpisKraja <- factor(podatki_pn$OpisKraja)
podatki_pn$VzrokNesrece <- factor(podatki_pn$VzrokNesrece)
podatki_pn$TipNesrece <- factor(podatki_pn$TipNesrece)
podatki_pn$VremenskeOkoliscine <- factor(podatki_pn$VremenskeOkoliscine)
podatki_pn$Spol <- factor(podatki_pn$Spol, levels = c("ŽENSKI", "MOŠKI"))
podatki_pn$PoskodbaUdelezenca <- factor(podatki_pn$PoskodbaUdelezenca, 
                                    levels = c("BREZ POŠKODBE","BREZ POŠKODBE-UZ",
                                               "LAŽJA TELESNA PO?KODBA",
                                               "HUDA TELESNA PO?KODBA",
                                               "SMRT"))
podatki_pn$UporabaVarnostnegaPasu <- factor(podatki_pn$UporabaVarnostnegaPasu)
podatki_pn$VrednostAlkotesta <- as.numeric(gsub(",", ".", gsub("\\.", "", podatki_pn$VrednostAlkotesta)))

podatki_pn$DanvTednu <- factor(wday(podatki_pn$DatumPN, label = TRUE), 
                           levels = c("pon\\.", "tor\\.", "sre\\.", "čet\\.", "pet\\.", "sob\\.", "ned\\."),
                           labels = c("PONEDELJEK", "TOREK", "SREDA", "ČETRTEK", "PETEK", "SOBOTA", "NEDELJA"))
podatki_pn$Mesec <- factor(month(podatki_pn$DatumPN), 
                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("JANUAR", "FEBRUAR", "MAREC", "APRIL","MAJ","JUNIJ","JULIJ", "AVGUST","SEPTEMBER","OKTOBER","NOVEMBER","DECEMBER"))

podatki_pn$Leto <- factor(year(podatki_pn$DatumPN))


saveRDS(podatki_pn, "podatki_pn_ocisceno")

##

# data for map
Slovenia2<-getData("GADM", country="SI", level=2)

Slovenia2_UTM<-spTransform(Slovenia2, CRS("+init=EPSG:4883"))

# changing all letters to capital letters 
NAME_2<-Slovenia2_UTM@data$NAME_2
Slovenia2_UTM@data$id <- rownames(Slovenia2_UTM@data)
Slovenia2_UTM@data = data.frame(lapply(Slovenia2_UTM@data, function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


# changing Č in C
Slovenia2_UTM@data[30, 7] = "ČRNOMELJ"
Slovenia2_UTM@data[32, 7] = "KOČEVJE"

# grouping data of "občine" in "upravne enote"
Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BLED"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BOHINJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RADOVLJICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "NAKLO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "JEZERSKO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CERKLJE NA GORENJSKEM"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PREDDVOR"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠENČUR"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KRANJ"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽELEZNIKI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽIRI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GORENJA VAS-POLJANE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠKOFJA LOKA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KRANJSKA GORA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽIROVNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "JESENICE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOBARID"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BOVEC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TOLMIN"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CERKNO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "IDRIJA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BRDA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KANAL"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MIREN-KOSTANJEVICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠEMPETER-VRTOJBA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "NOVA GORICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VIPAVA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "AJDOVŠČINA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOLENJSKE TOPLICE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠENTJERNEJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MIRNA PEC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽUŽEMBERK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠKOCJAN"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "NOVO MESTO"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOSTEL"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "OSILNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOČEVJE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SODRAŽICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LOŠKI POTOK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RIBNICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SEMIC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ČRNOMELJ"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ČRNA NA KOROŠKEM"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MEŽICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PREVALJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RAVNE NA KOROŠKEM"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MISLINJA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SLOVENJ GRADEC"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MUTA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PODVELKA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RIBNICA NA POHORJU"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VUZENICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RADLJE OB DRAVI"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BLOKE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LOŠKA DOLINA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CERKNICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PIVKA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "POSTOJNA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOMEN"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DIVACA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "HRPELJE-KOZINA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SEŽANA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BOROVNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VRHNIKA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BREZOVICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOL PRI LJUBLJANI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOBROVA-POLHOV GRADEC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "HORJUL"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "IG"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠKOFLJICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MEDVODE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VELIKE LAŠČE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VODICE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LJUBLJANA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOBREPOLJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "IVANCNA GORICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GROSUPLJE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOMENDA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KAMNIK"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MORAVCE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LUKOVICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MENGEŠ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TRZIN"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOMŽALE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BENEDIKT"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SVETA ANA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CERKVENJAK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LENART"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DESTRNIK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DORNAVA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "HAJDINA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "JURŠINCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KIDRICEVO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MAJŠPERK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MARKOVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽETALE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ZAVRC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VIDEM"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TRNOVSKA VAS"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SVETI ANDRAŽ V SLOVENSKIH GORICAH"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PODLEHNIK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GORIŠNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PTUJ"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RACE-FRAM"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "STARŠE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MIKLAVŽ NA DRAVSKEM POLJU"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "HOCE-SLIVNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DUPLEK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MARIBOR"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠENTILJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KUNGOTA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PESNICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LOVRENC NA POHORJU"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SELNICA OB DRAVI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RUŠE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "OPLOTNICA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SLOVENSKA BISTRICA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BELTINCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CANKOVA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GORNJI PETROVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "HODOŠ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GRAD"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KUZMA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MORAVSKE TOPLICE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TIŠINA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ROGAŠOVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠALOVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PUCONCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MURSKA SOBOTA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ČRENŠOVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOBROVNIK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOBILJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VELIKA POLANA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ODRANCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TURNIŠČE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LENDAVA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KRIŽEVCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VERŽEJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RAZKRIŽJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LJUTOMER"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RADENCI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SVETI JURIJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GORNJA RADGONA"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BISTRICA OB SOTLI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ROGATEC"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "KOZJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ROGAŠKA SLATINA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PODCETRTEK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠMARJE PRI JELŠAH"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOBJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠENTJUR PRI CELJU"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "BRASLOVCE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "POLZELA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "PREBOLD"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "TABOR"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VRANSKO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŽALEC"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "DOBRNA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠTORE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VOJNIK"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "CELJE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "GORNJI GRAD"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LJUBNO"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LUCE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "NAZARJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SOLCAVA"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "MOZIRJE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "RADECE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "LAŠKO"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠMARTNO OB PAKI"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ŠOŠTANJ"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VELENJE"), 7]

Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "ZRECE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "VITANJE"), 7] =
  Slovenia2_UTM@data[which(Slovenia2_UTM@data$NAME_2 == "SLOVENSKE KONJICE"), 7]

# saving new RDS file
saveRDS(Slovenia2_UTM, "Slovenia2_UTM")
