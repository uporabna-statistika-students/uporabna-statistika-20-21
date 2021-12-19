## 6. naloga
# nepristranskost cenilke
n = 100
lambda = 4
# naredimo veliko vzorcev iz populacije
# izracunamo ocene (za cenilko h) vzorcev
# pogledamo, okrog katere vrednosti ocene variirajo (povprecje)
# nepristranska bi bila cenilka v primeru, ce bi ocene 
#variirale okrog 4 (lambda iz populacije)

ocenaVzorca  = function(){
  vzorec = rexp(n,rate=lambda)
  ocena = 1/mean(vzorec) # funkcija h
  return(ocena)
}
ocene = replicate(10000,ocenaVzorca())
mean(ocene)
hist(ocene)

###################### koda direktno iz PDF z vajami
#funkcija za izracun cenilke
cenilka = function(x){
  return(length(x)/sum(x))}

# definiramo funkcijo za simulacijo N vzorcev in izracun N ocen
simN = function(N,n){
  ocene = NULL # inicializiramo vektor simuliranih ocen
  for(i in 1:N){
    vzorec = rexp(n,rate=4)
    ocene = c(ocene,cenilka(vzorec))} # izracunamo cenilko
  return(ocene) #vrnemo vektor ocen
}

#doslednost
velikostVzorca = seq(10,1000,by = 10)
povpr = NULL
for(i in velikostVzorca){
  povpr = c(povpr,mean(simN(N=3000,n=i)))}
plot(velikostVzorca,povpr,xlab='velikost vzorca',
     ylab="povprecje 3000 simulacij",main="",type="b")
abline(h=4,col="green") # populacijska vrednost

## 8. naloga
# pristranska varianca (podcenjuje)
varianca = function(x){
  return(1/length(x) * sum((x-mean(x))^2))
}

# tocka a. izracunaj varianco za vzorec 10^4 N(120,30)
vzorec = rnorm(10^4,mean = 120, sd = 30)
# velik vzorec, prevlada doslednost nad pristranskostjo
# popul. varianca: 30^2 = 900
varianca(vzorec)
# da bi videli, okrog katere vrednosti se varianca giblje, naredimo simulacijo

simulacija = function(){
  vzorec = rnorm(10^4,mean = 120, sd = 30)
  return(varianca(vzorec))
}

ocene = replicate(1000, simulacija())
mean(ocene)
hist(ocene)

# tocka b. 
# "vzorec" razdeli na podvzorce velikosti 10 in zanje izracunaj varianco 
# potem pa izracunaj povprecno varianco vseh podvzorcev
zacIndeksi = seq(1,length(vzorec),by=10)

variance = NULL
for(i in zacIndeksi){
  variance = c(variance, varianca(vzorec[i:(i+9)]))
}
mean(variance)
