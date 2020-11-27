#slučajna razporeditev
n <- 50
x <- sample(seq(0,10,0.1), n)
beta_0 <- 1
beta_1 <- 5
y <- beta_0 + beta_1*x + rnorm(n, 0, 10)
plot(x,y)

mod <- lm(y~x)
par(mfrow=c(2,2))
plot(mod)

# nelinearnost
n <- 50
x <- sample(seq(0,10,0.1), n)
beta_0 <- 1
beta_1 <- 5
beta_2 <- 2
x.kvad <-x^2
y <- beta_0 + beta_1*x + beta_2*x.kvad+rnorm(n, 0, 10)
par(mfrow=c(1,1))
plot(x,y)

mod <- lm(y~x)
par(mfrow=c(2,2))
plot(mod)

# heteroskedastičnost, nekonstantna varianca
n <- 50
x <- sample(seq(0,10,0.1), n)
beta_0 <- 1
beta_1 <- 5
y <- beta_0 + beta_1*x + rnorm(n, 0, 3*x) # varianca y je sorazmerna z x
par(mfrow=c(1,1))
plot(x,y)

mod <- lm(y~x)
par(mfrow=c(2,2))
plot(mod)

