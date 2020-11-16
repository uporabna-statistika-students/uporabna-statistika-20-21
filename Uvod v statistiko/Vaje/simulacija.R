####Koda za simulacijo (datoteka simulacija.r)

######Naloga 1

##simuliraj podatke za veliko populacijo iz N(1000,200):
y<-rnorm(1000000,mean=1000,sd=200)

##prikazi porazdelitev spremenljivke v histogramu:
par(mfrow=c(2,1))
hist(y,xlim=c(0,2000))

##zacni s simulacijo

n<-5 ##velikost vzorca
B<-1000 ##stevilo ponovitev simulacije
Y.bar<-rep(NA, B) ##tu se shranijo povprecja v posameznem koraku simulacije
for ( i in 1:B) { ##zacni s simulacijo
  id<-sample(1:length(y),n) ##slucajno izberi enote
  Y<-y[id] ##vrednosti na vzorcu
  Y.bar[i]<-mean(Y) ##povprecje na vzorcu
}
hist(Y.bar,xlim=c(0,2000)) ##narisi porazdelitev Y.bar
mean(Y.bar) ##povprecje porazdelitve
sd(Y.bar) ##razprsenost porazdelitve
 
##simuliraj podatke za veliko populacijo iz U(0,2000):
y<-runif(1000000,min=0,max=2000)
##prikazi porazdelitev spremenljivke v histogramu:
par(mfrow=c(2,1))
hist(y,xlim=c(0,2000))
##zacni s simulacijo

n<-5
B<-1000
Y.bar<-rep(NA, B)
for ( i in 1:B) {
  id<-sample(1:length(y),n)
  Y<-y[id]
  Y.bar[i]<-mean(Y)
}
hist(Y.bar,xlim=c(0,2000))
mean(Y.bar)
sd(Y.bar)

#################

#################


####Naloga 2


##simuliraj podatke za veliko populacijo iz N(1000,200):
y<-rnorm(1000000,mean=1000,sd=200)
##prikazi porazdelitev spremenljivke v histogramu:
par(mfrow=c(2,1))
hist(y,xlim=c(0,2000))

##zacni s simulacijo

n<-5 ##velikost vzorca
B<-10000 ##stevilo ponovitev simulacije
Y.bar<-rep(NA, B) ##tu se shranijo povprecja v posameznem koraku simulacije
z<-rep(NA,B) ##tu se shrani standardizirano vzorcno povprecje (prava SE)
tt<-rep(NA,B) ##tu se sharni standardizirano vzorcno povprecje (ocenjena SE)
for ( i in 1:B) { ##zacni s simulacijo
  id<-sample(1:length(y),n) ##slucajno izberi enote
  Y<-y[id] ##vrednosti na vzorcu
  Y.bar[i]<-mean(Y) ##povprecje na vzorcu
  z[i]<-(mean(Y)-1000)/(200/sqrt(n))
  tt[i]<-(mean(Y)-1000)/(sd(Y)/sqrt(n))
}

par(mfrow=c(1,2)) ##na isto sliko narisi dva histograma

hist(z,freq=FALSE,main="Prava SE",breaks=100)
##narisi histogram, kjer je na y-osi relativna frekvenca

xx<-seq(from=-4,to=4,by=0.01)
##rabimo zato, da izracunamo gostoto v posamezni tocki
lines(xx,dnorm(xx))
## v sliko dodaj gostoto standardne normalne porazdelitve
lines(xx,dt(xx,df=n-1),col="red")
## v sliko dodaj se gostoto t porazdelitve s n-1 stopinjami prostosti
 
hist(tt,freq=FALSE,main="Ocenjena SE",ylim=c(0,0.4),breaks=100,xlim=c(-5,5))
##histogram, kjer uporabljena ocenjena SE
 
lines(xx,dnorm(xx))
## v sliko dodaj gostoto standardne normalne porazdelitve
lines(xx,dt(xx,df=n-1),col="red")
## v sliko dodaj se gostoto t porazdelitve s n-1 stopinjami prostost

