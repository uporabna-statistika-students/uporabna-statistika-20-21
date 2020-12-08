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
simN = function(N){
  ocene = NULL # inicializiramo vektor simuliranih ocen
  for(i in 1:N){
    vzorec = rexp(20,rate=4)
    ocene = c(ocene,cenilka(vzorec))} # izracunamo cenilko
  return(ocene) #vrnemo vektor ocen
}

#(ne)pristranskost
stVzorcev = seq(50,5000,by = 50)
povpr = NULL
for(i in stVzorcev){
  povpr = c(povpr,mean(simN(i)))}
plot(stVzorcev,povpr,xlab='st. simulacij',
     ylab="povprecje simulacij",main="",type="b",ylim=c(4,4.4))
abline(h=4,col="green") # populacijska vrednost

