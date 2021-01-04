# 6. naloga

populacija = 1:10
vzorec = sample(populacija,size=5,replace=FALSE)
vzorec2 = sample(populacija,size=5,replace=FALSE)

#  spr1  spr2 
#   x1    y1
#   x2    y2

# cor(spr1, spr2)

# stat. tabela - simulacije

# spr1 (i-ta vrednost v vzorcu)   spr2 (j-ta vrednost v vzorcu)
#   xi(1. vzorec)                 xj(1.vzorec)
#   xi(2. vzorec)                 xj(2.vzorec)

populacija = 1:10
ocenaKor = function(){
  spr1 = NULL
  spr2 = NULL
  i = 1
  j = 3
  for(k in 1:1000){
    vzorec = sample(populacija,size=5,replace=FALSE)
    # v spr1 damo i-to vrednost
    spr1 = c(spr1, vzorec[i])
    # v spr2 damo j-to vrednost iz vzorca
    spr2 = c(spr2, vzorec[j])
  }
  return(cor(spr1,spr2))
}
ocenaKor()
simulacije = replicate(1000,ocenaKor())
hist(simulacije)
mean(simulacije)

# 14. naloga
qbinom(0.99,size=105,prob=1/15)
# tocna ploscina pod krivuljo od 0 do 14:
pbinom(14,size=105,prob=1/15)
# S TABLE: 0.99 < P(Y <= a-1)
# nas a je torej 15!

n=105; p = 1/15
qnorm(0.99,mean=n*p,sd=sqrt(n*p*(1-p)))
# a2 je torej 13
