# naloga 8.1

xA = 971
xB = 1037
xn = 199
n = xA + xB + xn

p0 = (xA+xB)/(2*n)
pA = xA/n
pB = xB/n
# prek posplošenega testa razmerja verjetij
testna = -2*((xA + xB)*log(p0) + xn*log(1-2*p0) - xA*log(pA) - xB*log(pB) - xn*log(1-pA-pB))
#vrednostP
pchisq(testna,df=1,lower.tail = FALSE)

# prek testa hi-kvadrat
oi = c(xA,xB,xn)
ei = c(1004,1004,199)
testna2 = sum((oi-ei)^2/ei)
#vrednostP
pchisq(testna2,df=1,lower.tail = FALSE)

# naloga 4
meja = qpois(0.95,lambda = 15)
podatki = c(0,0,0,1,1,1,1,1,2,2,3,3,3,3,5)
testna = sum(podatki)
# vrednostP
ppois(testna-1,lambda = 15,lower.tail=FALSE)

# verjetnost napake 1. vrste za primer lambdaA = 0.8
ppois(meja-1,lambda=15*0.8,lower.tail=FALSE)
# verjetnost napake 2. vrste za primer lambdaA = 1.4
ppois(meja,lambda=15*1.4)


lambda0 = 1
moc = NULL
leta = 100:250
for(n in leta){
  meja = qpois(0.95,n*lambda0)
  moc = c(moc,ppois(meja-1, n*1.2,lower.tail=FALSE))
}
leta[129]

# ničelna domneva, podatki prihajajo iz Pois porazdelitve
temp = factor(podatki,levels=0:max(podatki))
oi = table(temp)
lambdaMLE = mean(podatki)
verjetnostiPoisson = dpois(0:max(podatki),lambda=lambdaMLE) # P(X = x | lambda)
pricakovaneFrekvence = verjetnostiPoisson * length(podatki)

verjDo5 = dpois(0:(max(podatki)-1),lambda=lambdaMLE) 
verjOd5naprej = 1 - sum(verjDo5)
verjetnosti = c(verjDo5,verjOd5naprej)
ei = verjetnosti * length(podatki)

testna2 = sum((oi-ei)^2/ei)
#vrednostP, df = (st.ocenj.v splosnem) - (st.ocenj. pod H0)
# df =  (6-1) - 1(H0) = 4
pchisq(testna2,df=4,lower.tail = FALSE)
