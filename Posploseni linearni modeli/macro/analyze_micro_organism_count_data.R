t <- c(0,1,2,6,12)
count <- c(31,26,19,15,20)

plot(t, count, type="b", ylim=c(0, 40))
plot(t, log(count), type="b", ylim=c(2, 4))

mo.lm <- lm(count ~ t + I(t^2))
summary(mo.lm)
qqnorm(residuals(mo.lm), ylab="residuals", xlim=c(-3,2), ylim=c(-3,2))
qqline(residuals(mo.lm))

mo.P0 <- glm(count ~ t + I(t^2), family=poisson)
summary(mo.P0)

f <- fitted(mo.P0)
r <- residuals(mo.P0, type="pearson")
sum(r^2)
plot(f, r, ylab="residuals", xlab="fitted", ylim=c(-1,1)); abline(0,0)

plot(t, count, ylim=c(0,40))
t.new <- seq(0, 12, .5)
lines(t.new, predict(mo.P0, data.frame(t=t.new), type="response"))

mo.P1 <- glm(count ~ t, family=poisson)
anova(mo.P1, mo.P0, test="Chisq")


c <- d <- 1:100
for (i in 1:100) {
    c[i] <- i/200
    d[i] <-  deviance(glm(count ~ 1 + log(t+c[i]), family=poisson))
}
plot(c, d, type="l", ylab="deviance")
c[d==min(d)]
min(d)

tc <- t + 0.105
mo.P2 <- glm(count ~ 1+log(tc)+I(log(tc)^2), family=poisson)
mo.P3 <- glm(count ~ 1+log(tc), family=poisson); summary(mo.P3)
anova(mo.P3, mo.P2, test="Chisq")
plot(fitted(mo.P3), residuals(mo.P3), ylim=c(-1,1)); abline(0,0)


tc.new <- data.frame(tc = seq(0,12,.005) + 0.105)

# pointwise (1-alpha) confidence intervalls for mu
# either directly (Delta-Method)
mo.pred <- predict.glm(mo.P3, newdata=tc.new, type="response", se.fit=T)
attributes(mo.pred)
mo.pred$residual.scale

fit   <- mo.pred$fit
upper <- fit + qnorm(0.975)*mo.pred$se.fit
lower <- fit - qnorm(0.975)*mo.pred$se.fit

plot(t, count, type="p", ylim=c(0,50), xlab="t", ylab="micro-organisms")
lines(tc.new[,1], upper)
lines(tc.new[,1], fit)
lines(tc.new[,1], lower)

# or by means of the linearen predictor "link"
mo.peta <- predict.glm(mo.P3, newdata=tc.new, type="link", se.fit=T)
upper <- exp(mo.peta$fit + qnorm(0.975)*mo.peta$se.fit)
lower <- exp(mo.peta$fit - qnorm(0.975)*mo.peta$se.fit)
fit <- exp(mo.peta$fit)
plot(t, count, type="p", ylim=c(0,50), xlab="t", ylab="micro-organisms")
lines(tc.new[,1], upper); lines(tc.new[,1], fit); lines(tc.new[,1], lower)





