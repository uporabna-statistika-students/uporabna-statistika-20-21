### NALOGA 5.1 - simulacije
# funkcija za izracun vrednosti p

vrednostP = function(vzorec){
  n = length(vzorec)
  # izracun testne statistike
  testna = mean(vzorec)
  if(testna < 13){
    # ce testna  < 13
    vzorecP = pnorm(testna,mean=13,sd=3.56/sqrt(n))*2
  }else{
    vzorecP = pnorm(testna,mean=13,sd=3.56/sqrt(n),lower.tail=FALSE)*2
  }
  return(vzorecP)
}

# i. 1 vzorec iz H0, kaksna je vrednost p
n = 100
vzorec = rnorm(n,mean=13,sd=3.56)
vrednostP(vzorec)

# iii. 1000 vzorcev iz H0, kaksne so vrednosti p
rezultatH0 = NULL
for(i in 1:1000){
  vzorec = rnorm(n,mean=13,sd=3.56)
  rezultatH0 = c(rezultatH0, vrednostP(vzorec))
}
hist(rezultatH0)
mean(rezultatH0 < 0.05)
# ecdf = empirical cumulative distribution function
plot(ecdf(rezultatH0))

# ii. 1 vzorec pod HA: mu = 14
n = 100
vzorec = rnorm(n,mean=14,sd=3.56)
vrednostP(vzorec)

# iv. 1000 vzorcev iz HA: mu = 14, kaksne so vrednosti p
rezultatHA = NULL
for(i in 1:1000){
  vzorec = rnorm(n,mean=14,sd=3.56)
  rezultatHA = c(rezultatHA, vrednostP(vzorec))
}
hist(rezultatHA)
# delez zavrnjenih H0 (tj. delez vrednosti p < 0.05)
mean(rezultatHA < 0.05)
plot(ecdf(rezultatHA))

### NALOGA 5.2
# izris gostot porazdelitve pod H0 za nalogo 1 in 2
curve(dgamma(x,shape=39,rate=3),from=5,to=20)
curve(dnorm(x,mean=13,sd=3.56),add=TRUE,col="red")
abline(v=13)

# meji zavrnitve
qnorm(0.025,mean = 13, sd=sqrt(39/(9*100)))
qnorm(0.025,mean = 13, sd=sqrt(39/(9*100)),lower.tail=FALSE)

# kaj se dogaja, ko je n=4, 
# glede na to, da predpostavka o CLI ni izpolnjena
## kaksna je velikost testa za n = 4

vrednostP1 = function(vzorec,mu0,sd0){
  n = length(vzorec)
  # izracun testne statistike
  testna = mean(vzorec)
  if(testna < mu0){
    # ce testna  < mu0
    vzorecP = pnorm(testna,mean=mu0,sd=sd0/sqrt(n))*2
  }else{
    vzorecP = pnorm(testna,mean=mu0,sd=sd0/sqrt(n),lower.tail=FALSE)*2
  }
  return(vzorecP)
}

# simulacija deležev zavrnitev pod H0
n = 4
mu0 = 13
sd0 = sqrt(39/9)

sim1000 = function(){
  rezultatH0 = NULL
  for(i in 1:1000){
    vzorec = rgamma(n,shape=39,rate=3) # vzorec iz gamma porazd!
    rezultatH0 = c(rezultatH0, vrednostP1(vzorec,mu0,sd0))
  }
  #hist(rezultatH0)
  return(mean(rezultatH0 < 0.05))
}

delezZavrnitevH0 = replicate(1000,sim1000())
hist(delezZavrnitevH0,breaks=20)
mean(delezZavrnitevH0)

# izracun mej zavrnitve za n=4 s pomocjo simulacij
# generiramo veliko vzorcev, izracunamo vrednosti testnih statistik
# dolocimo 2.5 percentil in 97.5 percentil za porazdelitev testnih statistik

n = 4
testnaStat = NULL
for(i in 1:10000){
  vzorec = rgamma(n,shape=39,rate=3) 
  testnaStat = c(testnaStat, mean(vzorec))
}
quantile(testnaStat,probs = c(0.025,0.975))

## ko je testna statistika vsota opazovanj, je eksaktna testna statistika
# porazdeljena po Gamma(n*alpha,lambda)
# simulacije
alfa0 = 39
lambda = 3

# izracun natancne p-vrednosti v tem primeru NI 2-kratnik manjse ploscine pod krivuljo 
# testne statistike, saj je porazdelitev testne statistike ASIMETRICNA
# vec o tem https://stats.stackexchange.com/questions/140107/p-value-in-a-two-tail-test-with-asymmetric-null-distribution
# vendar pa to presega vsebino predmeta OTS
# zato se bomo omejili samo na prestevanje zavrnitev H0

# pod H0
# izracunamo meji zavrnitve (pri tem moramo vedeti tudi velikost vzorca)
n = 30
m1 = qgamma(0.025,shape=n*alfa0,rate=lambda)
m2 = qgamma(0.975,shape=n*alfa0,rate=lambda)

zavrniH0 = function(vzorec,alfa0,lambda){
  n = length(vzorec)
  testna = sum(vzorec)
  m1 = qgamma(0.025,shape=n*alfa0,rate=lambda)
  m2 = qgamma(0.975,shape=n*alfa0,rate=lambda)
  if((testna > m1) & (testna < m2)){
    return(FALSE) # ne zavrnemo H0
  }else{
    return(TRUE) # zavrnemo H0
  }
}

rezultatH0 = NULL
for(i in 1:100000){ # veliko vzorcev bolj natancno oceni delez zavrnitev
  vzorec = rgamma(n,shape=alfa0,rate=lambda) # vzorec iz H0
  rezultatH0 = c(rezultatH0, zavrniH0(vzorec,alfa0,lambda))
}
mean(rezultatH0) # delez zavrnitev

# pod HA
alfaA = 42
rezultatHA = NULL
for(i in 1:100000){
  vzorec = rgamma(n,shape=alfaA,rate=lambda) # vzorec iz HA (spremenjena alfa)
  rezultatHA = c(rezultatHA, zavrniH0(vzorec,alfa0,lambda))
}
mean(rezultatHA) # delez zavrnitev


# simulacija deleza zavrnitev za primer 
# testne statistike T2 (povprecna vrednost, CLI, normalna porazdelitev)
mu0 = 13
sd0 = sqrt(39/9)
rezultatHA = NULL
for(i in 1:100000){
  vzorec = rgamma(n,shape=alfaA,rate=lambda) # vzorec iz HA (spremenjena alfa)
  rezultatHA = c(rezultatHA, vrednostP1(vzorec,mu0,sd0))
}
mean(rezultatHA< 0.05) # delez zavrnitev

## KOMENTAR: delez zavrnitev je za obe verziji testnih statistik zelo podoben



### NALOGA 5.4
pod = read.csv("data/data_depression.csv")
dim(pod)

# meja zavrnitve
qnorm(0.05,mean=90,sd=14/sqrt(30))
# vrednost p za nas vzorec
testna = mean(pod$x)
pnorm(testna,mean=90,sd=14/sqrt(30))
