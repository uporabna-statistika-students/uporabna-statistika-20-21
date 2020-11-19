
#binomska porazdelitev X ~Bin(n=20,pi=1/6)
n = 20
verjetnost = 1/6
# pade 6 šestic
# P(X=6 | n=20, pi=1/6)
dbinom(6,size=n,prob=verjetnost)
# kumulativna verjetnost: da pade največ 6 šestic v 20 metih
# P(X <= 6 | n=20, pi=1/6)
pbinom(6,size=n,prob=verjetnost)
sum(dbinom(0:6,size=n,prob=verjetnost))

# ------------------------------------------------------

# binomska porazdelitev, št. petic v 10 metih, simulacije
# for zanka
vzorec = NULL
for(i in 1:1000){
  # generiraj vzorec (st. petic v 10 metih)
  # 10 metov (ali pade petica?) - sestevek! 
  # to je ena statisticna enota
  vzorec = append(vzorec,sum(sample(0:1,size=10,prob=c(5/6,1/6),replace=TRUE)))
}

# replicate
fun1 = function(){
  # 10 metov - sestevek!
  tmp = sum(sample(0:1,size=10,prob=c(5/6,1/6),replace=TRUE))
  return(tmp)
}
vzorec2 = replicate(1000,fun1())

# rbinom
vzorec3 = rbinom(1000,size=10,prob=1/6)
# frekvencna porazdelitev / frekvencna tabela
opazFrek =table(vzorec2)
barplot(opazFrek)

# teoreticne verjetnosti: P(X = k)
teorVerjetnosti =dbinom(0:10,size=10,prob=1/6)
pricFrek = 1000* teorVerjetnosti # X ~ Bin(n,pi) E(X) = n*pi

opazDelez = opazFrek/1000
#tabelaF = rbind(opazDelez,teorVerjetnosti) WARNING!!!

# ista dolzina vektorjev!
vzorecK = factor(vzorec2,levels=0:10)
opazFrek = table(vzorecK)
opazDelez = opazFrek/1000
tabelaF = rbind(opazDelez,teorVerjetnosti)
tabelaF = rbind(tabelaF,opazFrek,pricFrek)
dim(tabelaF)
kable(tabelaF,digits=c(3,3,3,3,4,4,4,4,7,7,7))
# obrnimo tabelo
kable(t(tabelaF),digits=c(3,3,0,1))

# dorisimo na graf frekvencni poligon pricakovanih frekvenc
barplot(opazFrek)
points(0:10,pricFrek)
lines(0:10,pricFrek) # ne da zadovoljivega rezultata

# ggplot
library(ggplot2)
df = data.frame(vzorec=vzorec2)
ggplot(df,aes(x=vzorec)) + geom_bar() #+ xlim(0,10)
df2 = data.frame(pricF=pricFrek,kateg=0:10)
ggplot(df,aes(x=vzorec)) + geom_bar() +
  geom_point(data=df2,aes(x=kateg,y=pricF)) +
  geom_line(data=df2,aes(x=kateg,y=pricF))

# ------------------------------------------------------

# simulacija hi-kvadrat porazdelitev
# generiram vzorec in N(0,1)
vzorec = rnorm(10000)
x = vzorec^2
hist(x,freq=FALSE) # porazd. hi-kvadrat(df=1)
curve(dchisq(x,df=1),add=TRUE,col="red",from=1,to=10)
# X ~ N(0,1) ---> X^2 ~ Chi^2(df=1)
# X_1 ~ N(0,1), X_2 ~ N(0,1), X_k ~ N(0,1) ---> Y = X_1^2 + X_2^2 + ... + X_k^2
# Y ~ Chi^2(df=k), X_1, ... X_k neodvisne!

k=26
vzorecK = replicate(k,rnorm(10000))
vzorecK2 = vzorecK^2
vsotaVrstic = apply(vzorecK2,1,FUN=sum)
dim(vsotaVrstic) # NULL
length(vsotaVrstic)

hist(vsotaVrstic,freq=FALSE,breaks=100) # porazd. hi-kvadrat(df=k)
curve(dchisq(x,df=k),add=TRUE,col="red")

# ------------------------------------------------------

# centralni limitni izrek (n -> infinity)
# vzorcna povprecja so porazdeljena po N(mu,sigma/sqrt(n))

vzorec = replicate(1000,mean(rnorm(30,mean=10,sd=2)))
hist(vzorec,freq=FALSE,breaks=50)
curve(dnorm(x,mean=10,sd=2/sqrt(30)),col="red",add=TRUE)

vzorec = replicate(1000,mean(rchisq(100,df=1)))
hist(vzorec,freq=FALSE)
curve(dnorm(x,mean=1,sd=2/sqrt(100)),col="red",add=TRUE)
# Wikipedia - za pričakovano vrednost, varianco hi-kvadrat porazdelitve z df=1

# ------------------------------------------------------

# primerjava vzorčne variance (1/(n-1)) in "populacijske" (1/n)

# vzorcna varianca ---> var(x) (1/(n-1))
varianca2 = function(x){
  n = length(x)
  tmp = var(x)*(n-1)/n
  return(tmp)
}

# velik vzorec obeh varianc 
vzorecV1 = replicate(10000,var(rnorm(10,mean=10,sd=2)))
vzorecV2 = replicate(10000,varianca2(rnorm(10,mean=10,sd=2)))
plot(density(vzorecV1)) # crne barve
lines(density(vzorecV2),col="red")
# populacijska varianca = 2^2
mean(vzorecV1) # nepristranska cenilka variance (gl. OTS)
mean(vzorecV2)