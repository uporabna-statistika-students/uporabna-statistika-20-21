library(car)
library(ggfortify)
library(multcomp)
library(tidyverse)

pacienti <- as_tibble(read.table("PACIENTI1.TXT", header = TRUE))


## 1.2
# 1
cor(pacienti, method="spearman")
cor(pacienti, method="pearson")
cor(pacienti, method="kendall")

pairs(pacienti)

# 2

mod_1 <- lm(SKT~zap + starost + masa + PT + utrip + stres, data=pacienti)
vif(mod_1)
summary(mod_1)

autoplot(mod_1, which=1:6)

mod_2 <- lm(SKT~starost + PT + utrip + stres, data=pacienti)
vif(mod_2)
summary(mod_2)

autoplot(mod_2, which=1:6)


summary(glht(mod_2))

# premal podatkov

## 1.3

spanje <- as_tibble(read.table("SLEEP.txt", header = TRUE, stringsAsFactors = TRUE))

spanje_no_na <- na.omit(spanje)

cor(spanje_no_na[c("BodyWt", "BrainWt", "TotalSleep", "LifeSpan", "Gestation")], method="spearman")

ggplot(spanje_no_na, aes(x=BodyWt, y=log(TotalSleep), col=Danger3)) +
    geom_point()

ggplot(spanje_no_na, aes(x=BrainWt, y=log(TotalSleep), col=Danger3)) +
    geom_point()

ggplot(spanje_no_na, aes(x=LifeSpan, y=log(TotalSleep), col=Danger3)) +
    geom_point()

ggplot(spanje_no_na, aes(x=Gestation, y=log(TotalSleep), col=Danger3)) +
    geom_point()

spanje_no_na$log_BodyWt <- log(spanje_no_na$BodyWt)
spanje_no_na$log_BrainWt <- log(spanje_no_na$BrainWt)

cor(spanje_no_na[c("BodyWt", "BrainWt", "BodyWt", "BrainWt", "TotalSleep", "LifeSpan", "Gestation")], method="spearman")

pairs(spanje_no_na[c("log_BodyWt", "log_BrainWt", "TotalSleep", "LifeSpan", "Gestation")])

mod_2 <- lm(TotalSleep~log_BodyWt + log_BrainWt + LifeSpan + Gestation, data=spanje_no_na)

autoplot(mod_2, which=1:6)

mod_2_l <- lm(log(TotalSleep)~log_BodyWt + log_BrainWt + LifeSpan + Gestation, data=spanje_no_na)

autoplot(mod_2_l, which=1:6)
