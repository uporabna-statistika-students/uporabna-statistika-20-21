trees <- read.table("trees.dat", header=TRUE)
attach(trees)

plot(H, V)
plot(D, V); lines(lowess(D, V)) # Krümmung (falsche Skala?)

# Modell: V ~ H + D
mod <- lm(V ~ H + D)

plot(lm.influence(mod)$hat, ylab = "leverages") # 2 Hebelpunkte

h.crit <- 2*mod$rank/length(V)
lev.points <- (lm.influence(mod)$hat > h.crit)
abline(h.crit, 0); abline(v = c(20, 31))

plot(D, residuals(mod), ylab="residuals"); abline(0, 0)
lines(lowess(D, residuals(mod))) # Senke in der Mitte


# Modell: V(lambda) ~ H + D
library(MASS, help=T)
bc <- boxcox(V ~ D + H, lambda = seq(0.0, 0.6,len = 100))
max(bc$y)  # max = -76.08  near lambda = 1/3
boxcox(V ~ D + H, lambda = seq(0.0, 0.6,len = 18)) # plot it

plot(D, V**(1/3))
lines(lowess(D, V**(1/3))) # Krümmung beseitigt

mod1 <- lm(V**(1/3) ~ H + D)
mu <- fitted(mod1)
s2 <- deviance(mod1)/mod1$df.residual
plot(mu**3, V); abline(0,1)  # Median Modell
plot(mu**3*(1+3*s2/mu**2), mu**3, xlab="mean", ylab="median") # Median/Mean


# Modell: log(V) ~ log(H) + log(D)
plot(log(D),log(V))
lm(log(V) ~ log(D) + log(H)) # ? log(V) or V ?
bc <- boxcox(V ~ log(H) + log(D), lambda = seq(-0.35, 0.25, len = 100))
max(bc$y) # max = -75.05  near lambda = -0.07
boxcox(V ~ log(H) + log(D), lambda = seq(-0.35, 0.25, len = 100))

bc1 <- boxcox(V ~ H + D, lambda = 1/3, plotit=FALSE)
bc1$x  # 1/3
bc1$y  # -76.12

bc2 <- boxcox(V ~ log(H) + log(D), lambda = 0, plotit=FALSE)
bc2$x  # 0
bc2$y  # -75.34
# both models results in similar maxima !
# LRT:
qchisq(0.95,3)


# Modell: log(V) ~ log(H) + log(D) (interpretation of parameters)
summary(lm(log(V) ~ log(D) + log(H)))
# log(D) slope close to 2
# log(H) slope close to 1

# if volume is described by a cylinder: V = D^2*H * pi/4
# if volume is described by a cone:     V = D^2*H * pi/12
# Thus, both models are of the form: log(V) = 2*log(D) + 1*log(H) + const
# with
# (cylinder): c = log(pi/4)
# (cone):     c = log(pi/12)

# in order to interpret the intercept, we have to convert the D scale
# from inches to feet
lm(log(V) ~ log(D/12) + log(H))

mod3 <- lm(log(V) ~ 1 + offset(2*log(D/12) + log(H)))
summary(mod3)

log(pi/4)

log(pi/12)

#========================================================================
# GLM: normal responses, various link functions:
#
# general call of "family = quasi()"
attach(trees)
glm(V ~ D + H, family = quasi(link=power(1/3), variance="constant"))

# setting control variables
glm(V ~ D + H, family = quasi(link=power(1/3), variance="constant"), trace=T, epsilon=1e-8, maxit=15)

# alternative specification
glm(V ~ log(D) + log(H), family = quasi(link=log, variance="constant"))
# or in short
cherry.model <- glm(V ~ log(D/12) + log(H), family = gaussian(link=log))
summary(cherry.model)
