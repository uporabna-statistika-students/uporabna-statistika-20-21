# vzorec iz N(120,30)

n = 1000
vzorec = rnorm(n,mean=120,sd=30)
# narisi histogram brez absolutnih frekvenc (y-os)
# VERJETNOSTNA PORAZDELITEV
hist(vzorec,freq=FALSE)
?density
lines(density(vzorec))
curve(dnorm(x,mean=120,sd=30),add=TRUE,col="red")

library(ggplot2)
df = data.frame(vzorec=vzorec)
ggplot(data=df,aes(x=vzorec)) + 
  geom_histogram(aes(y=..density..)) +
  geom_density() +
  stat_function(fun=dnorm,args=list(mean=120,sd=30),color="red")

# KUMULATIVNO VERJETNOSTNO PORAZDELITEV
# PORAZDELITVENA FUNKCIJA
# ecdf (emprical cumulative density function)

plot(ecdf(vzorec))
# teoreticna porazdelitvena funkcija 
curve(pnorm(x,mean=120,sd=30),add=TRUE,col="red")

# vzorec velikosti 10
vzorec = rnorm(10,mean=120,sd=30)
plot(ecdf(vzorec))
# teoreticna porazdelitvena funkcija 
curve(pnorm(x,mean=120,sd=30),add=TRUE,col="red")

## 2. naloga (teza novorojenčkov)
z0.5 = qnorm(0.05)
sigma = (2500-3300)/z0.5

# poiscimo se sredino intervala med 2500 in 4100
# da se prepricamo, da je 3300 res populacijsko povprecje
(4100+2500)/2

