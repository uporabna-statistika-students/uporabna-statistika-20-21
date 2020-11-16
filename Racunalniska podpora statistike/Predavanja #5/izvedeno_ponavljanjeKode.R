# CLI - centralni limitni izrek
vzorec = replicate(1000,mean(rnorm(30,mean=10,sd=2)))
hist(vzorec,freq=FALSE,breaks=50)
curve(dnorm(x,mean=10,sd=2/sqrt(30)),col="red",add=TRUE)

# 3 slike, za razlicne velikosti osnovnih vzorcev
# ponavljanje kode - NI PRIMERNO!!!
op=par(mfrow=c(1,3))
# 1. slika (n=30)
n = 30
vzorec = replicate(1000,mean(rnorm(n,mean=10,sd=2)))
hist(vzorec,freq=FALSE,breaks=20)
curve(dnorm(x,mean=10,sd=2/sqrt(n)),col="red",add=TRUE)
abline(v=mean(vzorec),col="blue")
# 2. slika (n=10)
n = 10
vzorec = replicate(1000,mean(rnorm(n,mean=10,sd=2)))
hist(vzorec,freq=FALSE,breaks=20)
curve(dnorm(x,mean=10,sd=2/sqrt(n)),col="red",add=TRUE)
abline(v=mean(vzorec),col="blue")
# 3. slika (n=100)
n = 100
vzorec = replicate(1000,mean(rnorm(n,mean=10,sd=2)))
hist(vzorec,freq=FALSE,breaks=20)
curve(dnorm(x,mean=10,sd=2/sqrt(n)),col="red",add=TRUE)
abline(v=mean(vzorec),col="blue")
# -----------

# boljsi nacin - BREZ PONAVLJANJA KODE 1
op=par(mfrow=c(1,3))
nji = c(30,10,100)
for(i in 1:3){
  n = nji[i] # izberemo velikost osnovnega vzorca
  vzorec = replicate(1000,mean(rnorm(n,mean=10,sd=2)))
  hist(vzorec,freq=FALSE,breaks=20)
  curve(dnorm(x,mean=10,sd=2/sqrt(n)),col="red",add=TRUE)
  abline(v=mean(vzorec),col="blue")
}

# sapply - BREZ PONAVLJANJA KODE 2
# deklaracija funkcije za simulacijo in risanje CLI
CLI = function(n){
  vzorec = replicate(1000,mean(rnorm(n,mean=10,sd=2)))
  hist(vzorec,freq=FALSE,breaks=20)
  curve(dnorm(x,mean=10,sd=2/sqrt(n)),col="red",add=TRUE)
  abline(v=mean(vzorec),col="blue")
}
op=par(mfrow=c(1,3))
sapply(nji,FUN = CLI)

