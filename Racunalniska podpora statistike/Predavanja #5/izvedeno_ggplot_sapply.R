# ------------------------------------------------
# datumi
format(Sys.time(), "%a %b %d %X %Y %Z")
x <- c("01jan.1960", "2jan.1960", "31mar.1960", "30jul.1960") # slovenska verzija - pri imenu meseca je se pika
z <- as.Date(x, "%d%b%Y")
class(z)
str(z)
as.numeric(z)
as.Date(0,origin="1970-01-01") # how much time from origin date

# ------------------------------------------------
# ggplot
studenti = read.table("data/studenti2012.txt",sep="\t",header=TRUE)
# popravimo napacne vnose in dolocimo faktorje
studenti[studenti$masa == 700,"masa"] = 70
studenti = studenti[-which(studenti$starost == 59),]
studenti$mesec[studenti$mesec==0] = NA
studenti$mesec = factor(studenti$mesec,levels=1:12,
                        labels=c("jan","feb","mar","apr","maj","jun","jul","avg","sep","okt","nov","dec"))
studenti$majica = factor(studenti$majica,levels=c("XS","S","M","L","XL"),ordered=TRUE)


# histogram + density
ggplot(studenti,aes(x = visina)) + geom_histogram(aes(y=..density..)) + facet_grid(spol~.) +
  geom_density(fill="yellow",alpha=0.2)
# boxplot + tocke
ggplot(studenti,aes(x= spol, y = visina)) + geom_boxplot() + geom_jitter(col="red",alpha=0.2)
# barplot (delezi)
ggplot(studenti,aes(x = majica)) + geom_bar(aes(fill=spol),position="fill") +
  labs(y="delez")
ggplot(studenti,aes(x = majica)) + geom_bar(stat="count")
# barplot (iz frekvencne tabele)
df1 = data.frame(uporaba = c(4.3,6,8,3.5,1,1.4),naprava = c("racunalnik","TV","telefon","radio" ,"Ipod","spletna kamera"))
  # osnovno risanje
barplot(df1$uporaba,names.arg = df1$naprava,las=2)
  # ggplot
ggplot(df1,aes(y=uporaba,x=naprava)) + geom_bar(stat="identity")
df1$naprava1 = factor(df1$naprava,levels=c("racunalnik","TV","telefon","radio" ,"Ipod","spletna kamera"),ordered=TRUE)
ggplot(df1,aes(y=uporaba,x=naprava1)) + geom_bar(stat="identity")

### Prikažite histograme višin študentov glede ena številko majice. 
### Na histograme narišite še navpične črte, ki bodo označevale povprečno višino za posamezno številko majice.
ggplot(studenti,aes(x=visina)) + geom_histogram(bins=5) + facet_wrap(majica~.)

  # nov podatkovni okvir za povprecne visine (glede na st. majice)
unique(studenti$majica)
velikosti = levels(studenti$majica)
  ### zanka
povpr = NULL
for(v in velikosti){
  visine = studenti$visina[studenti$majica == v] # dobim visine za st. majice iz "v"
  povpr = append(povpr,mean(visine,na.rm=TRUE)) # izracunam povprecje
}
  ### izracun povprecij s pomocjo funkcije SAPPLY
funPovpr = function(x){
  # x ... velikost majice
  visine = studenti$visina[studenti$majica == x]
  tmp = mean(visine,na.rm=TRUE)
  return(tmp)
  ## edina vrstica funkcije bi bila taka
  ## return(mean(studenti$visina[studenti$majica == x],na.rm=TRUE))
}

povpr2 = sapply(velikosti,FUN=funPovpr)

  # dobim vektor povprecnih visin "povpr"
dfPovpr = data.frame(majica=velikosti,povprecje=povpr2)

  # IZRIS
ggplot(studenti,aes(x=visina)) + geom_histogram(bins=5) + facet_wrap(majica~.) +
  geom_vline(data=dfPovpr,aes(xintercept=povprecje),col="red")
# dopisi se tekst (povprecje = ...) ... z "annotate"


# ------------------------------------------------
# porazdelitve

# met 1000 kock, vsi izidi (multinomska porazdelitev)
set.seed(42)
vzorec = sample(x=1:6,size=1000,replace=TRUE)
frekvT = table(vzorec)
barplot(frekvT,ylim=c(0,180))
# delez 6:
frekvT[6]/1000

# met 1000 kock, le šestice
set.seed(42)
vzorec = sample(x=c(TRUE,FALSE),size=1000,replace=TRUE,prob=c(1/6,5/6))
frekvT = table(vzorec)
barplot(frekvT)

# verjetnost in kvantili
p1 = pnorm(7.5,mean=povp,sd=stdo) # P(X <= 7.5 | mean=5, sd=2) = 0.89
#zgornji del
p2 = pnorm(7.5,mean=povp,sd=stdo,lower.tail=FALSE) # P(X > 7.5 | mean=5, sd=2) 
p1 + p2

qnorm(p1,mean=povp,sd=stdo) # inverz od pnorm

# ------------------------------------------------
# sapply na vektorju
fun2 = function(x){
  return(x +100)
}
sapply(1:10,FUN=fun2)
