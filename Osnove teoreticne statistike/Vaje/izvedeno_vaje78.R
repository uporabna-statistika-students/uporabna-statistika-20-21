# naloga 4.1

vzorec  = c(-1, 1,-1, 1, 2, 2, 0, 0,-1, 0, 1, 1, 1)

n = length(vzorec)
# 1. in 2. moment
mu1 = sum(vzorec)/n # izracunan na vzorcu
mu2 = sum(vzorec^2)/n

a = (mu2 - 1)/2
b = (mu1+1)/2 - mu2 + 1


# naloga 4.8
library(VGAM)
curve(drayleigh(x,3),from=0.01,to=10)
curve(drayleigh(x,1),from=0.01,to=10,add=TRUE,col="red")

set.seed(42)
n=100
vzorec=rrayleigh(n,3)
# ocena za theta:
thetaHat = sqrt(sum(vzorec^2)/(2*n))

#verjetje
log(prod(drayleigh(vzorec,3)))

# simulacija logaritmiranih verjetij
logVerjetje = function(theta,n){
  vzorec=rrayleigh(n,theta)
  return(log(prod(drayleigh(vzorec,theta))))
}

logL =replicate(1000,logVerjetje(3,100))
hist(logL)
plot(density(logL))

# verjetja za drug parameter theta (3.1)
logL1 =replicate(1000,logVerjetje(thetaHat,100))
lines(density(logL1),col="red")


# 
set.seed(42)
n=100
vzorec=rrayleigh(n,3)
thetas = seq(2,4,0.01)
# izracunamo logaritmirano verjetje za vzorec pri razlicnih thetah
logLVzorec = 
  sapply(thetas,FUN = function(x)log(prod(drayleigh(vzorec,x))))
plot(thetas,logLVzorec,type="l", ylab="log verjetja za vzorec") 
# narisemo log. verjetja
abline(v=thetaHat,col="red")
logLVHat = logLVzorec[max(which(thetas<thetaHat))] 
# pribl. pokazemo, da je thetaHat ekstrem
abline(h=logLVHat,col="red")

# izracun variance cenilke
varTheta = thetaHat^2/(4*n)

c(thetaHat - qnorm(0.975)*sqrt(varTheta), 
  thetaHat + qnorm(0.975)*sqrt(varTheta))
