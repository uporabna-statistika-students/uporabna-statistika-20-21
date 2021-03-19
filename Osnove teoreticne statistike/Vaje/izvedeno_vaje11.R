### NALOGA 6.1

x = read.csv("../data/data_semafor.csv")
class(x)
dim(x)

# testna statistika T2 ~ Pois(300)
meja = qpois(0.95,lambda = 300)
ppois(meja,lambda=300) #meja je ze v območju zavrnitve
# P(T2 <= meja | H0 drzi)

# moc testa (T2 alternativna Pois(350))
ppois(meja,lambda=350,lower.tail=FALSE) 
# ni povsem pravilno izracunana zaradi diskretnosti porazdelitve
#

# P(T2 >= meja | Ha drzi) --- > to je moč testa
# 1 - P(T2 < meja | Ha drzi)
# 1 - P(T2 <= (meja-1) | Ha drzi)
1 - ppois((meja-1),lambda=350)

# testna statistika za nase podatke in vrednost p
t2Vzorec = sum(x$x)
1 - ppois((t2Vzorec-1),lambda=300)

### NALOGA 6.5
studenti = read.csv("../data/data_studenti.csv")
m0 = lm(teza ~ 1 ,data=studenti)
mS = lm(teza ~ studij, data=studenti)
?logLik
logLik(m0)
logLik(mS)

testna = as.numeric(-2*(logLik(m0) - logLik(mS)))
# vrednost p na podlagi testne statistike 
#(vemo, da je porazd. po hi-kvadrat porazd. z 2 st.prostosti)
pchisq(testna,df=2,lower.tail = FALSE)

# NALOGA 6.3
curve(x^2,from=0,to=1)
curve(x^3,add=TRUE,col="red")

