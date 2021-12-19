## 2. naloga (teza novorojenčkov)
z0.05 = qnorm(0.05)
sigma = (2500-3300)/z0.05

# poiscimo se sredino intervala med 2500 in 4100
# da se prepricamo, da je 3300 res populacijsko povprecje
(4100+2500)/2

# b. izracun IQR, za populacijo
qnorm(c(0.25,0.75),mean=3300,sd=sigma)
# c. percentil za 2900g
pnorm(2900,mean=3300,sd=sigma)
# d. percentil za 2900 na podlagi vzorca
n = 500
vzorec = rnorm(n,mean=3300,sd=sigma)
# izracun vzorcnega percentila
sum(vzorec <= 2900)/n # precej nenatancne ocene
# drugace
#mean(vzorec <=2900)

# e. 90% IZ za povprecje
# povpr +/- kriticna * SE
povp = mean(vzorec)
SEpop = sigma/sqrt(n)
SEvzorec = sd(vzorec)/sqrt(n)
# IZ na podlagi SEpop
kritPop = qnorm(0.05)# percentil std. norm. porazdelitve (NEGATIVEN)
kritPop = abs(kritPop)
c(povp - kritPop*SEpop, povp + kritPop*SEpop)
# IZ na podlagi SEvzorec
kritVzorec = qt(0.95,df=n-1) #(POZITIVEN)
c(povp - kritVzorec*SEvzorec, povp + kritVzorec*SEvzorec)

# 4. naloga
N = 1500
x = 1:5
px = c(1/15,1/5,4/15,2/5,1/15)
# pricakovana vrednost po definiciji
Ex = sum(x*px)
# zapisi populacijo
frek = N*px
populacija = NULL
for(i in 1:5){
  populacija = c(populacija,rep(x[i],frek[i]))
}
table(populacija)
# tmp = rep(x,frek) # krajse
# pricakovana vrednost iz "populacije"
ExPop = mean(populacija)

# varianca po definiciji
#E((X - E(X))^2)
# E(f(X)) = sum_i f(x_i) p(x_i)
# f(X) = (X - E(X))^2

fx = (x-Ex)^2
varX = sum(fx*px)
# varianca iz populacije (spr. "populacija")
varXPop = var(populacija)*(N-1)/N

# izberi vzorec s ponavljanjem iz "populacije"
sample(populacija,15,replace = TRUE)
sample(x,15,replace=TRUE,prob=px)

# 5. naloga
# X ~ Bernoulli(pi = 0.85)
vzorec = as.numeric(runif(100) < 0.85)
table(vzorec)
# tocka b doma
# tocka c
# generirajmo veliko vzorcev (n=100) iz Bernoullijeve porazd. -> sum
vzorecBin = replicate(10000,sum(as.numeric(runif(100) < 0.85)))
# tako dobimo opazovanje iz Bin(n=100,pi)
# narisemo opazovanja in 
hist(vzorecBin,breaks=20,freq=FALSE)
#cez teoreticno porazd. Bin(n=100,pi=0.85)
verjBin = dbinom(0:100,size = 100,prob=0.85)
points(0:100,verjBin,col="red",pch=16)
# poglejte projekt o biserih pri RPS za lepsi izris grafa

# 6. naloga
n = 1000
xEU = rnorm(n, mean=500,sd=100)
xZDA = rnorm(n, mean=500,sd=200)
cov(xEU,xZDA)
cor(xEU,xZDA)
# simulacija za boljso oceno kovariance
kovariance = NULL
for(i in 1:1000){
  xEU = rnorm(n, mean=500,sd=100)
  xZDA = rnorm(n, mean=500,sd=200)
  kovariance = c(kovariance,cov(xEU,xZDA))
}
hist(kovariance)
mean(kovariance)

# oziroma iz 1 samega vzorca EU in ZDA, ki je ZELO velik
# pokazite s simulacijami, da je kovarianca
# cov(xEU, xEU-xZDA) = var(xEU)
