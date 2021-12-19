# naloga 3.2
# narisi graf velikosti vzorca v odvisnosti od sd za neskoncno pop.
funkcijaNesk = function(sigma,alfa=0.05){
  z = qnorm(p=1-alfa/2)
  return((sigma*z/2)^2)
}

curve(funkcijaNesk(x,alfa=0.05), from=0.5, to=15)

# dodaj graf velikosti vzorca v odvisnosti od sd za koncno pop.
funkcijaKoncna = function(sigma,alfa=0.05,N=175){
  z = qnorm(p=1-alfa/2)
  return((sigma*z)^2*N/(4*(N-1)+(sigma*z)^2))
}

curve(funkcijaKoncna(x,alfa=0.05,N=175), from=0.5, to=30,
      add=TRUE,col="red")
abline(h=175,lty=2)

# naloga 3.4(d)  variabilnost deleza
n = 140
p=0.7
p*(1-p)/(n-1) #nepristranska cenilka za varianco deleza

funkcijaVar = function(p){
  return(p*(1-p)/(n-1))
}
curve(funkcijaVar(x),from=0,to=1)
