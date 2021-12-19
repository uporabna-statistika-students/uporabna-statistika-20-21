library(ggplot2)



#Pomoc za porazdelitve v R:help(Distributions). Za vzorec velikosti 1000
#iz porazdelitve N(120, 30^2) narisite
#
# a.) verjetnostno porazdelitev
#       kaj je gostota porazdelitve? Kaj dobimo, ce narisemo rezultate funkcije density?
#       Na histogram dodajte še pričakovano vrednost za to spremenljivko ()abline, parameter v; ali geom_vline)
#       na histogram dodajte še gostoto nrmalne porazdelitve(uporabite funkciji dnorm in curve
#       parameter add=T; ali stat_function)

n <- 1000
mu <- 120
sd <- 30

set.seed(8)

norm_vect <- rnorm(n,mean=mu,sd=sd)

ggplot(data.frame(x=norm_vect), aes(x=x)) +
    geom_histogram(aes(y=..density..), bins=40) + # dens hist
    geom_vline(xintercept=mean(norm_vect)) + # vzorcno povprecje
    geom_vline(xintercept=120, col="#1b98e0") + # populacijsko povprecje
    geom_density() + # dens plot
    stat_function(fun = dnorm, # normlna porazdelitev cez
                  args = list(mean = mu,
                              sd = sd),
                  col = "#1b98e0",
                  size = 1)

# nikoli ne bomo točno zadeli teoretičnih vrednosti bomo pa blizu. Skačemo okrog modre vrednosti

# b.) Porazdelitveno funkcijo (ecdf) ponovite isto za vzorec velikosti 10. Komentirajte
# opazanja

ggplot(data.frame(x=norm_vect), aes(x=x)) +
    stat_ecdf(geom = "step") +
    stat_function(fun=pnorm, col="#1b98e0",
                  args=list(mean=mu, sd=sd))

n <- 10

norm_vect1 <- rnorm(n, mean=mu, sd=sd)
ggplot(data.frame(x=norm_vect1), aes(x=x)) +
    stat_ecdf(geom="step") +
    stat_function(fun=pnorm, col="#1b98e0",
                  args=list(mean=mu, sd=sd))


# 2. Povprečna porodna teža novorojenčka v Sloveniji je 3.300 gramov, večina
# donošenih novorojenčkov ob rojstvu tehta med 2.500 in 4.100 grami.
#   a.) Če velja, da je teža novorojenčkov v populaciji normalno porazdeljena in da sta spodnja in zgornja
#   meja 5 in 95. percentil, z R izračunajte, kolikšn je standardni odklon teže
#   novorojenčkov.

# sd = (X - \mu)/z_{0.05}

qnorm(0.05) # -1.644854

# sd = -800/-1.65 = 486.3655

sd <- (2500-3300)/qnorm(0.05)

#   b.) Izračunajte interkvartilni razmik za težo.

c(qnorm(0.25, mean=3300, sd=sd) - qnorm(0.75, mean=3300, sd=sd), qnorm(0.25, mean=3300, sd=sd), qnorm(0.75, mean=3300, sd=sd))

#   c.) V katerem percentilu se nahaja novorojenček, ki je težak 2900 g?

pnorm(2900, mean=3300, sd=sd)

#   d.) Iz teoretične porazdelitve izberite vzorec 5, 50, 500 enot in na vzorcu
#   izračunajte percentil za novorojenčka z 2900 grami. Primerjajte in komentirajte rezultata.

dojencki5 <- rnorm(5, mean=3300, sd=sd)
dojencki50 <- rnorm(50, mean=3300, sd=sd)
dojencki500 <- rnorm(500, mean=3300, sd=sd)

mean(ifelse(dojencki5 < 2900, 1, 0))
mean(ifelse(dojencki50 < 2900, 1, 0))
mean(ifelse(dojencki500 < 2900, 1, 0))
#   e.) Za izbran vzorec s 500 enotami izračunajte 90% interval zaupanja za povprečje.
#   Primerjajte ga z intervalom, ki so ga poročali na začetku naloge. Komentirajte.

quantile(dojencki50, probs=c(0.05, 0.95))

se <- sd(dojencki500)/sqrt(500)
mean_dojencki <- mean(dojencki500)
t_crit <- dt(0.95, df=499)

c(mean_dojencki - t_crit*se, mean_dojencki + t_crit*se)

# 3. teoretične nalogice:
#   a.) Pokažite s pomočjo definicije za variance, da drži var(X) = E(X^2) - E(X)^2
#   B.) Pokažite s pomočjo vsote varianc, da drži var(2X)=4var(X)
#   c.) Pokažite, da varianco končne populacije (velikosti N) lahko zapišemo tudi kot
#
#   var(X) = 1/2N^2 sum(od i=1 do N)sum(od j=1 do N)(x_i - x_j)^2

# rešene na listu




# 4. Verjetnostna porazdelitev končne populacije, ki vas zanima, je predstavljena v spodnji
# tabeli pod nalogo. V njej je 1500 enot.
#   a.) Izračunajte pričakovano vrednost populacije v R.
#       - po definiciji

x <- c(1,2,3,4,5)
px <- c(1/15,1/5,4/15,2/5,1/15)

ex <- sum(x*px)

sum((x-ex)^2*px) # varianca


#       - tako da zapišete populacijo v vektor (funkcija rep)
N <- 1500

populacija <- rep(x, N*px)

table(populacija)

ex2 <- mean(populacija)

#   b.) Izračunajte varianco populacije v R. Zakaj izračun s funkcijo var da drugačen rezultat?

var(populacija)*((N-1)/N)


#   c.) Iz populacije bi radi izbrali vzorec velikosti 15 s ponavljanjem. Zapišite kodo v R. (sample)
N <- 15
sample(x,N,replace=T,prob=px)

#   d.) Iz populacije bi radi izbrali vzorec velikosti 15 brez ponavljanja. Zapišite kodo v R.

sample(populacija,N,replace=F)

#   x        1      2       3       4       5
#   p(X)     1/15   1/5     4/15    2/5     1/15




# 5. Naj bo spremenljivka X podazdeljna po Bernoulli(\pi=0.85)
#   a.) Generirajte 100 opazovanj. (uporabite npr. runif ali sample)

vzorec <- as.numeric(runif(100) < 0.85)

#   b.) Izračunajte po definiciji E(x) in Var(X)
# na listu

#   c.)Vemo, da je vsota neodvisnih, enako porazdeljenih Bernoullijevih spremenljivk porazdeljena
#   po binomski porazdelitvi (Y = \sum_{i=1}^n X_i \sim Bin(n, \pi)) Pokažite s to simulacijo
#   velikega vzorca.


eny <- function(n,p) {
    vzorec <- sample(0:1, size=n, replace=T, prob=c(1-p,p))
    return(sum(vzorec))
}

ponovi <- 100000
nn <- 100
pp <- 0.85

vzorecy <- replicate(ponovi, eny(n=nn, p=pp))
hist(vzorecy, freq=F, breaks=max(vzorecy)-min(vzorecy))
teory <- dbinom(0:nn, size=nn, prob=pp)
points(0:nn-0.5, teory, col="red")

#   d.) Izpeljite, da velja Var(Y)=n*\pi(1-\pi)






# 6. Radi bi primerjali povprečni teži dveh velikih skupin morskih prašičkov.
# Prva skupina so tisti, ki so hišni ljubljenčki v Evropi, druga tist iz ZDA. V ta namen zberemo
# enako velika vzorca.
#   a.) Kateri statistični test bi uporabili? Kaj lahko poveste o kovarianci teh dveh skupin?

# t test za neodvisna vzorca. Najverjetneje bo 0, ker sta neodvisna vzorca

#   b.) Simulirajte populacijsko kovarianco v R (funkcija cov) za naslednji spremenljivki
#       predpostavite lahko, da sta teži v populaciji pribl. normalno porazdeljeni):
#       - X_EU in X_ZDA (koliko je populacijska kovarianca?)
#       - X_Eu in X_EU - X_ZDA (kaj pričakujete?)


n <- 10000
xeu <- rnorm(n=n, mean=500, sd=100)
xzda <- rnorm(n=n, mean=500, sd=200)

cov(xeu, xzda)
cor(xeu, xzda)

coveuzda <- function() {
    xeu <- rnorm(n=n, mean=500, sd=100)
    xzda <- rnorm(n=n, mean=500, sd=200)

    cov(xeu, xzda)

}

kovariance <- replicate(1000, coveuzda())

hist(kovariance)
mean(kovariance)



# xeu in xeu-xzda


coveueuzda <- function() {
    xeu <- rnorm(n=n, mean=500, sd=100)
    xzda <- rnorm(n=n, mean=500, sd=200)

    cov(xeu, xeu-xzda)

}

kovariance <- replicate(1000, coveueuzda())

hist(kovariance)
mean(kovariance)

#   c.) izračunajte kovarianco teoretično

# smo že na listu
#






func <- function(ime)
