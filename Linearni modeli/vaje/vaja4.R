# SKRIPTNA DATOTEKA IZ GRADIVA PREDPOSTAVKE NISO IZPOLNJENE
#######################################

#Primer: postaje, 1.del
###############################################
postaje<-read.table("POSTAJE.txt", header=TRUE, sep="\t")
rownames(postaje)<-postaje$Postaja
postaje<-subset(postaje, subset=postaje$Postaja!="Kredarica")
# Kredarico izločimo iz analize (glej primer pri posebnih točkah)
# koordinate geografske dolžine in geografske širine izrazimo v km
postaje$x<-postaje$x.gdol/1000
postaje$y<-postaje$y.gsir/1000
# izločimo dve postaji, ki nimata podatka za geografsko dolžino
postaje<-na.omit(postaje)
library(car)
scatterplot(padavine~x, regLine=F, smooth=FALSE,
            boxplots='xy',  xlab=c("Geografska dolžina (km)"),
            ylab=c("Padavine (mm)"), data=postaje, id=list(n=3), pch=16)
model.1 <- lm(padavine~x, data=postaje)
par(mfrow = c(2, 2), oma=c(0,0,3,0))
plot(model.1)
par(mfrow = c(1, 1))
symbox(~padavine, xlab= "Lambda", ylab="Transformirane vrednosti za padavine",
       data=postaje)
summary(powerTransform(model.1))
boxCox(model.1)
# transformacija 1/y
model.2 <- lm(1/padavine~x, data=postaje)
par(mfrow = c(2, 2), oma=c(0,0,3,0))
plot(model.2)
#############################

# Primer: kovine
kovine0<-read.table("KOVINE.txt", header=TRUE, sep="\t")
kovine0$razdalja<-kovine0$razdalja.m/1000
# izločimo vzorčne točke z oddaljenostjo več kot 10 km
kovine<-kovine0[kovine0$razdalja<10,]
dim(kovine)
summary(kovine[,c("Pb","razdalja")])
scatterplot(Pb~razdalja, regLine=F, xlab="Razdalja (km)",
            ylab="Pb (mg/kg)", smooth=FALSE, spread=FALSE,
            boxplots='xy', span=0.5, data=kovine, pch=16)
scatterplot(log(Pb)~razdalja, regLine=F, smooth=FALSE,
            xlab="Razdalja (km)", ylab="log(Pb)", spread=FALSE,
            boxplots='xy', span=0.5, data=kovine, pch=16)
model.Pb <- lm(log(Pb)~razdalja, data=kovine)
par(mfrow = c(2, 2),  oma = c(0, 0, 3, 0))
plot(model.Pb)
par(mfrow = c(1, 1))
qqPlot(model.Pb, id=TRUE)
influencePlot(model.Pb, id=T)
outlierTest(model.Pb)
(b <- coefficients(model.Pb))
summary(model.Pb)$r.squared
confint(model.Pb)
library(effects)
plot(effect(c("razdalja"), model.Pb, transformation=list(link=log, inverse=exp)),
     axes=list(y=list(lab="Pb")), main="")
library(ggplot2)
ggplot(data = kovine, aes(x = razdalja, y = log(Pb))) +
    geom_point() + stat_smooth(method = "lm") +
    xlab("Razdalja (km)") + ylab("log(Pb)")
ggplot(data = kovine, aes(x = razdalja, y = Pb)) +
    geom_point() + xlab("Razdalja (km)") + ylab("Pb (mg/kg)") +
    stat_function(fun=function(razdalja) exp(b[1]+b[2]*razdalja) )
########################################

# Primer: trees
############################################
names(trees)
k1<-0.30480   ## feet -> m
k2<-0.0254    ## inches -> m
H<-trees$Height*k1
D <-trees$Girth*k2
Vol <-trees$Volume*(k1^3)
drevesa<-data.frame(cbind(Vol, H, D))
summary(drevesa)
pairs(log(drevesa))
model.d<- lm(log(Vol)~log(D)+log(H), data=drevesa)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
plot(model.d)
summary(model.d)$r.squared
confint(model.d)

#####################################

# Primer: postaje 2. del
####################################
par(mfrow=c(1,2))
with(postaje, plot(x, padavine,  pch=16))
with(postaje, plot(z.nv, padavine,  pch=16))
model.m1<-lm(padavine~ x + z.nv, data=postaje)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))
plot(model.m1)
avPlots(model.m1, ylim=c(-1000,1500), id=list(location="avoid"))
library(effects)
graf1 <- plot(Effect(c("z.nv", "x"), model.m1, partial.residuals=TRUE),
              ci.style="none", lattice=list(layout=c(4, 1)))
graf2 <- plot(Effect(c("x","z.nv"), model.m1, partial.residuals=TRUE),
              ci.style="none",lattice=list(layout=c(4, 1)))
library(gridExtra)
grid.arrange(graf1, graf2)
model.m2<-lm(padavine~ z.nv + x + z.nv:x , data=postaje)
# model.m2<-lm(padavine~ z.nv * x  , data=postaje) # krajši zapis
X.m2<-model.matrix(model.m2)
head(X.m2, n=3)
model.m2$coeff
graf1 <- plot(Effect(c("z.nv", "x"), model.m2, partial.residuals=TRUE),
              ci.style="none", lattice=list(layout=c(4, 1)))
graf2 <- plot(Effect(c("x","z.nv"), model.m2, partial.residuals=TRUE),
              ci.style="none",lattice=list(layout=c(4, 1)))
library(gridExtra)
grid.arrange(graf1, graf2)
par(mfrow=c(2,2),  oma = c(0, 0, 2, 0))
plot(model.m2)
avPlots (model.m2, ylim=c(-1000,600), id=list(location="avoid"))
outlierTest(model.m2)
summary(model.m2)$coeff
summary(model.m2)$r.squared
plot(predictorEffects(model.m2, ~.,
                      xlevels=list(x=4, z.nv=c(250, 500, 750, 1500))),
     rows=2, cols=1, main="", layout=c(4,1))

#########################
# ilustracija pomena parametrov, če je interakcijski člen v modelu
#########################################
z<-seq(from=100, to=500, by=100)
razlika<-rep(NA,5)
for (i in 1:5) razlika[i]<-model.m2$coeff[3]*50+model.m2$coeff[4]*50*z[i]
round(data.frame(z,razlika),1)

anova(model.m2) # sekvenčni F-test

#####################################

# Primer: mammals
#######################################
library(MASS)
head(mammals)
scatterplot(brain~body, regLine=FALSE, smooth=FALSE,
            boxplots="xy", data=mammals, id=list(n=3),pch=16)
par(mfrow=c(1,1))
symbox(~brain, xlab= "Lambda", ylab="Transformirane vrednosti za brain",
       data=mammals)
scatterplot(log10(brain) ~ body, regLine=FALSE, smooth=FALSE,
            boxplots="xy", data=mammals, id=list(n=3), pch=16)
scatterplot(log10(brain) ~ log10(body), regLine=FALSE,
            smooth=FALSE, spread=FALSE, boxplots="xy",
            data=mammals, id=list(n=3), pch=16)
model.m1<- lm(log10(brain)~log10(body), data=mammals)
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(model.m1)
par(mfrow=c(1,1))
qqPlot(model.m1, id=list(n=3))
outlierTest(model.m1)
influencePlot(model.m1,id=list(n=1), xlab="Vzvodi",ylab="Studentizirani ostanki")
summary(model.m1)$coeff
summary(model.m1)$r.squared
confint(model.m1)
ggplot(mammals, aes(x=log10(body),y=log10(brain)))+
    geom_point() + stat_smooth(method="lm")
# scatterplot(log10(brain)~log10(body), reg.line=lm, smooth=FALSE,
# boxplots="xy", data=mammals, id=T, pch=16)

###################################

# Primer: ANDY
######################################

andy<-read.table("ANDY.txt", header=T)
str(andy)
andy$height<-andy$height/3.2808 # višino dreves izrazimo v metrih
andy$buckets<-factor(andy$buckets) # buckets naj bo opisna spremenljivka, ker je za obravnavanje kot številsko spremenljivko premalo vrednosti
summary(andy)
library(ggplot2)
ggplot(data=andy, aes(x=age, y=height, col=buckets)) +
    geom_point() + xlab("Starost (leta)") + ylab("Višina dreves (m)")
mod.OLS<-lm(height ~ age * buckets, data=andy)
anova(mod.OLS)
par(mfrow=c(2,2))
plot(mod.OLS)
library(dplyr)
andy <- as_tibble(andy)
utez <- andy %>%
    group_by(age) %>%
    summarise(w=1/var(height))
andy <- merge(andy, utez, by="age")
head(andy)
par(mfrow=c(1,1))
plot(andy$age, andy$w, pch=16, type=c("b"))
mod.WLS<-lm(height ~ age * buckets, weights = w, data=andy)
par(mfrow=c(2,2))
plot(mod.WLS)
plot(fitted(mod.WLS), rstandard(mod.WLS),
     xlab="Prilagojene vrednosti",ylab="Standardizirani ostanki")
abline(h=0)
anova(mod.WLS)
summary(mod.WLS)
library(effects)
p1 <- plot(Effect(c("buckets"), mod.WLS), main="", ylim=c(0,16))
p2 <- plot(Effect(c("buckets","age"), mod.WLS), main="", multiline=TRUE,
           ci.style="band", ylim=c(0,16))
library(gridExtra)
grid.arrange(p1, p2, nrow=1, ncol=2)
