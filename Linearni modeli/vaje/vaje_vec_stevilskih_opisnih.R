library(ISLR)
library(tidyverse)
library(effects)
library(car)



data("Carseats")

Carseats <- as_tibble(Carseats)


# 1.1 - 1.

ggplot(Carseats, aes(x=Price, y=Sales, col=ShelveLoc)) +
    geom_point()

ggplot(Carseats, aes(x=Advertising, y=Sales, col=ShelveLoc)) +
    geom_point()

ggplot(Carseats, aes(y=Sales, x=ShelveLoc)) +
    geom_boxplot()

Carseats$ShelveLoc <- factor(Carseats$ShelveLoc, levels=c("Bad", "Medium", "Good"))

ggplot(Carseats, aes(y=Sales, x=ShelveLoc)) +
    geom_boxplot()

# 1.1 - 2.

# Ali grafični prikaz narekuje obstoj interakcije med ShelveLoc in Price in/ali
# obstoj interakcije med ShelveLoc in Advertising? Kako se to vidi?
#


ggplot(Carseats, aes(x=Price, y=Sales, col=ShelveLoc)) +
    geom_point() +
    geom_smooth(se=FALSE)

ggplot(Carseats, aes(x=Advertising, y=Sales, col=ShelveLoc)) +
    geom_point() +
    geom_smooth(se=FALSE)

ggplot(Carseats, aes(x=Price, y=Sales)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    facet_wrap(vars(ShelveLoc))

ggplot(Carseats, aes(x=Advertising, y=Sales, col=ShelveLoc)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    facet_wrap(vars(ShelveLoc))


mod_0 <- lm(Sales ~ ShelveLoc + Price + Advertising, data=Carseats)
mod_1 <- lm(Sales ~ ShelveLoc + Price*ShelveLoc + Advertising*ShelveLoc, data=Carseats)

anova(mod_0, mod_1)

plot(Effect(c("Advertising", "Price", "ShelveLoc"),
            mod_0,
            partial.residuals=TRUE),
     ci.style="none")

mod_2 <- lm(Sales ~ ShelveLoc + Price * Advertising, data=Carseats)

anova(mod_0, mod_2)

par(mfrow=c(2, 2), oma=c(0, 0, 2, 0))
plot(mod_0)

avPlots(mod_0)

Carseats$w.M <- model.matrix(mod_0)[, "ShelveLocMedium"]
Carseats$w.G <- model.matrix(mod_0)[, "ShelveLocGood"]

e.y <- residuals(lm(Sales~ Price + Advertising + w.G, data=Carseats))
e.w.M <- residuals(lm(w.M~ Price + Advertising + w.G, data=Carseats))

mod_e.M <- lm(e.y~e.w.M)

(b.e.M <- coef(summary(mod_e.M))[2, 1])
(s.b.e.M <- coef(summary(mod_e.M))[2, 2])

