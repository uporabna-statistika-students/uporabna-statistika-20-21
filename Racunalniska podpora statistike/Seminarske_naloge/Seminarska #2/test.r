# alen <- read.table("test_data/slovenia-traffic-accidents-2016-persons.txt",
#                    header = TRUE,
#                    fill = TRUE,
#                    sep = "\t",
#                    encoding = "UTF-8")
#
# alen2 <- read.table("test_data/slovenia-traffic-accidents-2016-events.txt",
#                    header = TRUE,
#                    fill = TRUE,
#                    sep = "\t",
#                    encoding = "UTF-8")
#
#

set.seed(1232)
n = c(50,500,10^3,10^4,10^5)
m = 10^3
sim = function(){
    ocena = NULL
    napaka = NULL
    levi = NULL
    desni = NULL
    for(i in n){
        sim1 = replicate(m,ocenaA(i))
        ocena1 = round(mean(sim1),4)
        napaka1 = sd(sim1)
        interval1 = qt(0.975,df=m-1)*napaka1/sqrt(m)
        levi1 = ocena1 - interval1
        desni1 = ocena1 + interval1
        ocena[i] = ocena1
        napaka[i] = napaka1
        levi[i] = levi1
        desni[i] = desni1
        hist(sim1, main = paste("n =",i), xlab = "ocena e", ylab = "frekvenca")
        }
    tabela = cbind(ocena,napaka,levi,desni)
    return(tabela)
}
tabela = na.omit(sim())
steviloN = as.character(n)
steviloN = replace(steviloN,steviloN=="1e+05","100000")
ocenae1 = round(tabela[,1],4)
napaka1 = round(tabela[,2],4)
levi1 = round(tabela[,3],4) desni1 = round(tabela[,4],4) interval1 = paste("[",levi1," ; ",desni1,"]",sep = "")  tabela = t(rbind(steviloN,ocenae1,napaka1,interval1))
