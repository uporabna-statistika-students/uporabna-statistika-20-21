#==========================================================
# Introduction to Regression analysis through MC Simulation
#==========================================================
# Consider a SLR for n=11 fixed x 0,1,...,10
x <- seq(0, 10)    # or in short x <- 0:10
n <- length(x)     # here n=11 with means
beta0 <- 3
beta1 <- 0.7
mu <- beta0 + beta1*x
sigma <- rep(2, n) # constant standard deviation

# Plot mean model with 2-sigma bounds
plot(x ,mu, type="l", ylim=c(-5,20),col="red",pch="*",ylab="mu+-2sigma")
lines(x, mu+2*sigma, col="red");  lines(x, mu-2*sigma, col="red")

# Simulate a dataset (with n elements) under these assumptions
set.seed(29)
y <- rnorm(n, mean=mu, sd=sigma)
points(x, y)       # also plot these data into the same graph


# Determine the LS etsimates b_0, b_1 and the MSE
lm(y ~ 1 + x)      # or in short: lm(y ~ x)
slr <- lm(y ~ x)   # save the fit-object and name it "slr"

# more information (standard errors, ...) through "summary( )"
summary(slr)

# what's all in "slr" and in "summary(slr)" ?
attributes(slr);  attributes(summary(slr))
abline(slr)        # plot the estimated regression line

# a friend also collect data under the same model and gets
y2 <- rnorm(n, mean=mu, sd=sigma)
points(x, y2, col="blue")       # plot the friend's data
abline(lm(y2 ~ x), col="blue")  # and the resulting estimated model

# repeating this experiment several times (S times) we get
plot(x ,mu, type="l", ylim=c(-5,20),col="red",pch="*",ylab="mu+-2sigma")
lines(x, mu+2*sigma, col="red");  lines(x, mu-2*sigma, col="red")

S <- 1000
b0 <- b1 <- MSE <- rep(NA, S)  # initialize vectors
for(s in 1:S)  {
    y.sim <- rnorm(n, mean=mu, sd=sigma)
    slr.sim <- lm(y.sim ~ x)
    b0[s] <- slr.sim$coefficients[1] # or slr.sim$coef["(Intercept)"]
    b1[s] <- slr.sim$coefficients[2] # or slr.sim$coef["x"]
    MSE[s] <- summary(slr.sim)$sigma^2
    abline(slr.sim)
}

# Both estimates b0, b1 seem to be normally distributed
hist(b0, freq=FALSE)
summary(b0)
sd(b0)
hist(b1, freq=FALSE)
summary(b1)
sd(b1)

# The estimate MSE for sigma^2 seems to be a chi^2 variable
hist(MSE, freq=FALSE)
summary(MSE)
sd(MSE)

# the original sample gave
summary(slr)

# the theoretical results (usually unknown to us) would be
Sxx <- sum((x-mean(x))^2); Sxx
var.b0 <- sigma[1]^2*sum(x^2)/(n*Sxx)  # var(b_0)
sqrt(var.b0)                           # std.err(b_0)
var.b1 <- sigma[1]^2/Sxx               # var(b_1)
sqrt(var.b1)                           # std.err(b_1)

# SSE/sigma^2 ~ chi^2_(n-2), where SSE = MSE*(n-2)
# with   Var(SSE/sigma^2) = 2*(n-2)
# =>     Var(SSE) = 2*(n-2)*sigma^4
# or     Var(MSE) = Var(SSE/(n-2)) = 2*sigma^4/(n-2)
var.MSE <- 2*sigma[1]^4/(n-2)  # var(MSE)
sqrt(var.MSE)                  # std.err(MSE)


#=========================================================
# Confidence intervalls for unknown parameters:
#=========================================================
# (1-alpha)*100% percent CIV(beta_1)
# ==> b1 +/- t(1-alpha/2; n-2)*sqrt(MSE/Sxx)

alpha <- 0.05
t <- qt(1-alpha/2, n-2)  # t=2.262

u <- o <- rep(NA, S)  # initialize vectors
for (s in 1:S){
    u[s] <- b1[s] - t*sqrt(MSE[s]/Sxx)
    o[s] <- b1[s] + t*sqrt(MSE[s]/Sxx)
}
wrong.o <- sum(o < beta1);  wrong.o
wrong.u <- sum(beta1 < u);  wrong.u
alpha.mc <- (wrong.o + wrong.u)/S;  alpha.mc

R <- 200
plot(seq(1:R), b1[1:R],
     ylim=c(min(u[1:R]), max(o[1:R])),
     xlab="repetition", ylab=expression(paste("CIV(", beta[1], ")", sep="")))

for (s in 1:R)
{
    col <- 1*(u[s]<beta1 & o[s]>beta1) + 2*(o[s]<beta1) + 3*(u[s]>beta1)
    l <- 1*(col == 1) + 4*(col > 1)  # line width
    lines(c(s, s), c(u[s], o[s]), col=col, lwd=l)
}
abline(h=beta1, lwd=2)


#=========================================================
# Hypotheses Tests for unknown parameters:
#=========================================================
# Test H0: beta1 <= 0 versus H1: beta1 > 0
# using the test statistic t* = (b1-0)/(sqrt(MSE/Sxx))

tstar <- p.value <- rep(NA, S)
for (s in 1:S){
    tstar[s] <- b1[s]/sqrt(MSE[s]/Sxx)
    p.value[s] <- pt(tstar[s], n-2, lower.tail=FALSE)
}
hist(p.value)

R <- S
plot(seq(1:R), tstar[1:R], xlab="repetition", ylab="t*")

t <- qt(1-alpha, n-2)  # t=1.833
abline(h=t, col="red", lwd=2)

# Compare the hypothetical density of t* (given H0) with the simulated one
plot((-30:100)/10, dt((-30:100)/10, n-2), type="l", lwd=2, xlab="t*", ylab="density")
lines(density(tstar), col="blue", lwd=2)
abline(v=t, col="red", lwd=2)

# the correct conclusion is drawn with (MC-estimated) probability
sum(tstar > t)/S


#=========================================================
# Now we test H0: beta1 = 0.7 versus H1: beta1 /= 0.7
tstar1 <- rep(NA, S)
for (s in 1:S){
    tstar1[s] <- (b1[s] - 0.7)/sqrt(MSE[s]/Sxx)
    p.value[s] <- 2*pt(abs(tstar1[s]), n-2, lower.tail=FALSE)
}

hist(p.value)

plot(seq(1:R), tstar1[1:R], xlab="repetition", ylab="t*")

t <- qt(1-alpha/2, n-2)
abline(h=t, col="red", lwd=2); abline(h=-t, col="red", lwd=2)

# Compare the hypothetical density of t* (given H0) with the simulated one
plot((-40:40)/10,dt((-40:40)/10,n-2),type="l",lwd=2,xlab="t*",ylab="density")
lines(density(tstar1), col="blue", lwd=2)
abline(v=t, col="red", lwd=2); abline(v=-t, col="red", lwd=2)

# the correct conclusion is drawn with (MC-) probability
sum(abs(tstar1) < t)/S
#=========================================================
