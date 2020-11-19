
# ------------------------------------------------
# while zanka
funkcija = function(x){
  rezultat = x^3 - x - 2
  return(rezultat)
}
x = -5:5
# for zanka
y = NULL
for(i in x){
  y = append(y,funkcija(i))
}
# namesto zgornje for zanke
y = funkcija(x)
plot(x=x,y=y)
lines(x=x,y=y)

plot(x=x,y=y,type="l")
abline(h=0,col="red")

# program
# interval
a = 1
b = 2
fSredina = 1
# izvajaj
while((abs(fSredina)>10^-5)&(abs(a-b)> 10^-5)){
  # telo zanke
  sredina = (a+b)/2
  fSredina = funkcija(sredina)
  if(fSredina >= 0){
    b = sredina
  }else{
    a = sredina
  }
}

# funkcija
bisekcija = function(a,b,FUN){
  fSredina = 1
  # izvajaj
  while((abs(fSredina)>10^-5)&(abs(a-b)> 10^-5)){
    # telo zanke
    sredina = (a+b)/2
    fSredina = FUN(sredina)
    if(fSredina >= 0){
      b = sredina
    }else{
      a = sredina
    }
  }
  return(list(nicla = sredina,interval = c(a,b)))
}
rezultat = bisekcija(a=1,b=2,FUN=funkcija)

# bolj splošna funkckija
bisekcija = function(a,b,FUN){
  fSredina = 1
  # izvajaj
  while((abs(fSredina)>10^-5)&(abs(a-b)> 10^-5)){
    # telo zanke
    sredina = (a+b)/2
    fSredina = FUN(sredina)
    fA = FUN(a)
    if(sign(fSredina) != sign(fA)){
      b = sredina
    }else{
      a = sredina
    }
  }
  return(list(nicla = sredina,interval = c(a,b)))
}
rezultat = bisekcija(a=1,b=2,FUN=funkcija)


# ------------------------------------------------
# ggplot
podatki = readRDS("data/podatki.Rda")
podatki$skupinaN = factor(podatki$skupina,levels = 1:3,labels=paste(rep("group",3),1:3))
podatki$terapijaN = factor(podatki$terapija,levels=0:1,labels=c("placebo","therapy"))


graf1 = ggplot(data=podatki,mapping=aes(y=BMI)) + geom_boxplot(col="lightblue")
# boxplot s facet_grid
graf1 + facet_grid(.~skupinaN)
# boxplot z aes(x=skupinaN)
ggplot(data=podatki,mapping=aes(y=BMI,x=skupinaN)) + geom_boxplot(col="lightblue") + labs(x="")

# stat_summary
ggplot(podatki)+stat_summary(mapping = aes(x = terapijaN, y = BMI),  fun.ymin = min, fun.ymax = max, fun.y = median)+ facet_wrap(skupinaN~.)

kvartil1 = function(x){
  return(quantile(x,probs=0.25))
}
kvartil3 = function(x){
  return(quantile(x,probs=0.75))
}

ggplot(podatki)+stat_summary(mapping = aes(x = terapijaN, y = BMI),  fun.ymin = kvartil1, fun.ymax = kvartil3, fun.y = median)+ facet_wrap(skupinaN~.)


