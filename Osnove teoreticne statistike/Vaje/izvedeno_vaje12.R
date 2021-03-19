### NALOGA 6.8 (velikost vzorca, k, ni)

set.seed(1)

k = 10 #st. sportnikov
ni = 50 #st. meritev za vsakega sportnika
si = rep(5,k) # std.odklon vsakega sportnika

ponovi = 1000
rezultat = numeric(ponovi)
for(it in 1:ponovi){
  mi = rnorm(k,148,7) # povprecje vsakega sportnika
  sportnik = data.frame(mi=mi,si=si)
  meritve = as.data.frame(apply(sportnik,1,FUN=function(x){rnorm(ni,x[1],x[2])}))
  miHat = apply(meritve,2,mean)# za vsak stolpec
  siHat = apply(meritve,2,function(x){sqrt(1/ni*sum((x-mean(x))^2))})
  xMinusMi = apply(meritve,2,function(x){sum((x-mean(x))^2)})
  si0Hat = sqrt(sum(xMinusMi)/(ni*k))
  logL = 2*ni*sum(log(si0Hat) - log(siHat))
  rezultat[it] = logL
}

hist(pchisq(rezultat,k-1,lower.tail=FALSE))


#### NALOGA 7.1
curve(dexp(x,rate=2),from=0,to=3)
x = rexp(10^4,rate=2)
hist(x)
# za opazovanja x izracunati F(x) = P(X <= x)
Fx = pexp(x,rate=2)
hist(Fx)

#### NALOGA 7.2
# inverz F-1(x)
a = 2
b =0.5
invF = function(x){
  a - b*log(-log(x))
}
# generiramo vzorec iz Unif(0,1)
vzorec = runif(1000)
# uporabimo invF
vzorec2 = invF(vzorec)
# vzorec2 naj bi bil porazdeljen po Gumbel(2,0.5)
hist(vzorec2,freq = FALSE,breaks=50)
# library(ordinal)
curve(dgumbel(x,a,b),add=TRUE,col="red")

#### NALOGA 7.5
p1 = 12/18
p2 = 11/22
p = 23/40
n1 = 18
n2 = 22
testna = (p1-p2)/sqrt(p*(1-p)*(1/n1 + 1/n2))
testna
pvrednost = pnorm(abs(testna),lower.tail=FALSE)*2

# še z uporabo Fisherjevega eksaktnega testa
kontT = matrix(c(11,12,11,6),ncol=2)
fisher.test(kontT)

# funkcija za izračun vrednosti p za test za primerjavo povprecij
testnaS = function(p1,p2,n1,n2){
  p = 0.8 # na podlagi prejsnjih raziskav (brez intervencij)
  testna = (p1-p2)/sqrt(p*(1-p)*(1/n1 + 1/n2))
  return(pnorm(abs(testna),lower.tail=FALSE)*2)
}

# moc testa za razliko 5%
# n1 in n2 sta enaka kot prej

meja1 = qnorm(0.025)
meja2 = qnorm(0.025,lower.tail = FALSE)
# teoreticno vrednost moci testa:
# P(T <= meja1 | Ha (povprecje -0.05)) + P(T >= meja2 | Ha (povprecje -0.05))
p0 = 0.5 # na podlagi prejsnjih raziskav (brez intervencij)
SE = sqrt(p0*(1-p0)*(1/n1+1/n2))
pnorm(meja1,mean = -0.05/SE, sd= 1) + pnorm(meja2,mean = -0.05/SE, sd= 1, lower.tail=FALSE)

# s simulacijami
vrednostiP = NULL
for(i in 1:10^6){
  # generiramo vzorca sk1 in sk2 (pod alternativno domnevo)
  v1 = rbinom(1,size = n1, prob = 0.8) # pri moskih - ostanejo na istem glede na prejsnje raziskave
  v2 = rbinom(1,size = n2, prob = 0.85)
  vrednostiP[i] = testnaS(v1/n1,v2/n2,n1,n2)
}
hist(vrednostiP)
mean(vrednostiP < 0.05)

# simulacije za H0
n1 = 100
n2 = 100
testnaS = function(p1,p2,n1,n2){
  p = 0.5 # na podlagi prejsnjih raziskav (brez intervencij)
  testna = (p1-p2)/sqrt(p*(1-p)*(1/n1 + 1/n2))
  return(pnorm(abs(testna),lower.tail=FALSE)*2)
}
vrednostiP = NULL
for(i in 1:10^5){
  # generiramo vzorca sk1 in sk2 (pod alternativno domnevo)
  v1 = rbinom(1,size = n1, prob = 0.5) 
  v2 = rbinom(1,size = n2, prob = 0.5)
  vrednostiP[i] = testnaS(v1/n1,v2/n2,n1,n2)
}
hist(vrednostiP,breaks=100) # "zanimiv vzorec" zaradi diskretnosti porazdelitve
mean(vrednostiP < 0.05)


