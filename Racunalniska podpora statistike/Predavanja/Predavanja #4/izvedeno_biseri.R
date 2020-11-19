######## tocka 1

# parametri
n=80
p=0.2
N = 350
# porazd. št neuspehov
x = 150:500
# neuspehi so bolj pogosti (verj 0.8)
y = dnbinom(x,size=n,prob=p)
op=par(mfrow=c(2,1))
plot(x=x,y=y,xlab = "st.neuspehov",ylab ="teor.verjetnost")
# dodali normalno aproksimacijo
pricV = n*(1-p)/p
varianca = n*(1-p)/p^2
curve(dnorm(x,mean=pricV,sd=sqrt(varianca)),col="red",
      add=TRUE,lwd=2)

# porazd. st potopov
x2 = x + n # st. neuspehov + st. uspehov
plot(x=x2,y=y,xlab = "st.potopov",ylab ="teor.verjetnost")
abline(v = N)
par(op)

# verjetnost za <= 350 potopov, pri tem, da dobimo 80 biserov
pnbinom(N-n,size=n,prob=p) # 10.3%
# z normalno aproksimacijo, verjetnost za <= 350 potopov pri tem, da dobimo 80 biserov
# oz. verjetnost za <= 350-80 neuspelih poskusov
pnorm(N-n,mean=pricV,sd=sqrt(varianca)) #10.6%

# ------------------------------------------------------
######## tocka 2

x = 40:110
# binomska porazdelitev
y = dbinom(x,size=N,prob=p)
plot(x=x,y=y,xlab = "st.uspehov",ylab ="teor.verjetnost")
abline(v=n)
pricV = N*p
pbinom(n,size=N,prob=p,lower.tail = FALSE) # P(X > 80)
# preverimo s funkcijo dbinom (P(X=x), vse od x=80 naprej bomo sesteli)
sum(dbinom(80:350,size=N,prob=p))
pbinom(n-1,size=N,prob=p,lower.tail = FALSE) # P(x >=80)

# simuliramo 1000 vzorcev Bin(N,p)
vzorec = rbinom(1000,size=N,prob=p)
hist(vzorec,xlab="st.uspehov",ylab="frekvenca",main="",freq=FALSE)
points(x=x,y=y)
# za vsako vrednost (celostevilska!) na x-osi bi imeli svoj stolpec
minV = min(vzorec)
maxV = max(vzorec)
intervali = seq(minV-0.5,maxV+0.5,by=1)
hist(vzorec,xlab="st.uspehov",ylab="frekvenca",main="",freq=FALSE,
     breaks=intervali)
points(x=x,y=y,pch=16)
pricV = N*p
varianca = N*p*(1-p)
curve(dnorm(x,mean=pricV,sd=sqrt(varianca)),col="red",
      add=TRUE,lwd=2)

# kako to narediti z ggplot?

# ------------------------------------------------------
######## tocka 3

# st. potopov do vkljucno 1. bisera
# generiramo eno opazovanje iz porazd. Geom(p)
set.seed(42)
rgeom(1,prob=p) + 1
# st potopov za 80 biserov (z geom. porazd.)
# ponovi 120 vrstico 80x
vzorec = rgeom(80,prob=p) + 1 #+ rep(1,80)
# za preverjanje naredimo frekvencno tabelo
table(vzorec)
stPotopov = sum(vzorec)

# simulacija 1000x stevilo potopov
simul = replicate(1000, sum(rgeom(80,prob=p) + 1))
hist(simul,xlab="st.potopov",ylab="frekvenca",main="")

minV = min(simul)
maxV = max(simul)
intervali = seq(minV-0.5,maxV+0.5,by=1)
hist(simul,xlab="st.potopov",ylab="frekvenca",main="",
     breaks=intervali)
# doloci x, y  za teoreticno porazdelitev (NB(n,p)+80)
x = (minV:maxV) - n # st.neuspehov
y = dnbinom(x,size=n,prob=p) * 1000 # 1000 simulacij, dobimo pricakovane frekvence
x2 = x + n
points(x=x2,y=y,pch=16,col="red")

# za bolj zglajeno sliko uporabite vecje st.simulacij

# izracun deleza realizacij z 350 ali manj potopi
sum(simul<=350) # st. realizacij s 350 ali manj potopi
sum(simul<=350)/1000 # delez

