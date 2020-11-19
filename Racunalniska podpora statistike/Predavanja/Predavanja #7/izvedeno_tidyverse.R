studenti %>%
  filter(lasje=="S" & spol=="F")

studenti %>%
  filter(mati < 160) %>%
  arrange(desc(oce))

x = studenti %>%
  group_by(majica) %>%
  summarise(st.studentov = n(),
            povp.visina = mean(visina),
            mediana.visine = median(visina),
            povp.roke = mean(roke,na.rm=TRUE),
            st.visjih.od.165 = length(which(visina>165)),
            st.visjih.2 = sum(visina>165))


studenti %>%
  mutate(razmerje=oce/mati) %>%
  ggplot(aes(x=razmerje)) + geom_histogram() +
  facet_grid(spol~.)
# z okvirjem z rocaji
studenti %>%
  mutate(razmerje=oce/mati) %>%
  ggplot(aes(x=spol,y=razmerje)) + geom_boxplot()

# generiranje podatkov v statističnih tabelah
n = 10 # velikost vzorca
st = tibble(spr1 = rnorm(n,mean=10,sd=5),spr2 = rnorm(10)) %>%
  mutate(spr3 = rbinom(n,size=1,prob=0.5)) %>%
  mutate(spr3 = as.factor(spr3))
st

#### ANOVA
n = 300
stSkupin = 3
sigma = 5
# generiraj tabelo, st. spr: 2, tip spremenljivk: 1 stevilska (mu_i + napaka),
#1  opisna (skupina)
st2 = tibble(skupina=sample(1:3,n,replace=TRUE,prob=c(1/3,1/3,1/3))) %>%
  mutate(napaka = rnorm(n,mean=0,sd=sigma)) %>%
  group_by(skupina) %>%
  mutate(povprecje = rnorm(1,mean=2,sd=2)) %>%
  ungroup()

# preverimo generiranje podatkov
# zapisimo povprecje za spr. "povprecje" in std. odklon zanjo za vsako skupino posebej

unique(st2$povprecje) # izpis vseh razlicnih vrednosti v spr. "povprecje"
st2 %>% 
  group_by(skupina) %>%
  summarise(povp = mean(povprecje), stdo = sd(povprecje))

# dokoncamo tabelo st2
st2 = st2 %>% 
  mutate(stevilska = povprecje + napaka,
         skupina = as.factor(skupina)) %>%
  select(stevilska, skupina)

# izvedi na podatkih analizo variance
st2 = st2 %>%
  mutate(napovedi = aov(stevilska ~ skupina)$fitted.values)

# brez tidyverse-a
summary(aov(stevilska ~ skupina,data=st2))

# graficni prikaz podatkov
library(ggplot2)
st2 %>% ggplot(aes(x=skupina,y=stevilska)) + geom_boxplot()

#### dolgi in široki format tabele podatkov

library(nlme)
#View(Orthodont) # long format
orti = Orthodont[1:16,1:3] 
orti

ortiWider = orti %>%
  pivot_wider(id_cols=Subject,names_from=age,values_from=distance,names_prefix="starost")

ortiLonger = ortiWider %>%
  pivot_longer(cols=paste("starost",c(8,10,12,14),sep=""),names_to = "starost",values_to="razlika")

