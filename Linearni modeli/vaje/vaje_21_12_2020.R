data(Wage, package="ISLR")
library(dplyr)
library(ggplot2)
library(ggfortify)
library(splines)

wage <- as_tibble(Wage)

#
ggplot(wage, aes(x=age, y=wage)) +
    geom_point() +
    geom_smooth(method="loess", se=FALSE)


mod_0 <- lm(data=wage, wage~age)
mod_1 <- lm(data=wage, wage~poly(age, degree=2))
mod_2 <- lm(data=wage, wage~poly(age, degree=3))
mod_3 <- lm(data=wage, wage~poly(age, degree=4))
mod_4 <- lm(data=wage, wage~poly(age, degree=5))

anova(mod_0, mod_1)
anova(mod_1, mod_2)
anova(mod_2, mod_3)
anova(mod_3, mod_4)

autoplot(mod_3, which=1:4)

summary(mod_3)

# creating models
mod_0r <- lm(data=wage, wage~age, raw=TRUE)
mod_1r <- lm(data=wage, wage~poly(age, degree=2, raw=TRUE))
mod_2r <- lm(data=wage, wage~poly(age, degree=3, raw=TRUE))
mod_3r <- lm(data=wage, wage~poly(age, degree=4, raw=TRUE))
mod_4r <- lm(data=wage, wage~poly(age, degree=5, raw=TRUE))

anova(mod_0r, mod_1r)
anova(mod_1r, mod_2r)
anova(mod_2r, mod_3r)
anova(mod_3r, mod_4r)

autoplot(mod_3r, which=1:4)

summary(mod_3r)


mod_ns1 <- lm(data=wage, wage~ns(age, df=2))
mod_ns2 <- lm(data=wage, wage~ns(age, df=3))
mod_ns3 <- lm(data=wage, wage~ns(age, df=4))
mod_ns4 <- lm(data=wage, wage~ns(age, df=5))
mod_ns5 <- lm(data=wage, wage~ns(age, df=6))
mod_ns6 <- lm(data=wage, wage~ns(age, df=7))
mod_ns7 <- lm(data=wage, wage~ns(age, df=8))

anova(mod_ns1, mod_ns2, mod_ns3, mod_ns4, mod_ns5, mod_ns6, mod_ns7)
