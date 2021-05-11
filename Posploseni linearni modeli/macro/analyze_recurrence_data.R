rez <- c(21,7,9,13,18,6,5,5,4,16,9,10)
total <- c(124,21,16,13,58,12,7,5,14,19,12,12)
LK <- c(0,1,2,3,0,1,2,3,0,1,2,3)
GZ <- c(0,0,0,0,1,1,1,1,2,2,2,2)


SF <- cbind(rez, nonrez=total-rez)
rez.glm <- glm(SF ~ LK + GZ, family = binomial); summary(rez.glm)


L <- factor(LK); G <- factor(GZ);    rez.1 <- glm(SF~1, family=binomial)
rez.L <- glm(SF~L, family=binomial); rez.G <- glm(SF~G, family=binomial)
rez.LG<-glm(SF~L+G,family=binomial); rez.sat<-glm(SF~L*G,family=binomial)

anova(rez.1, rez.L, rez.LG, rez.sat, test="Chisq")
summary(rez.LG)$coefficients


plot(as.numeric(L), rez.LG$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="L+G")
lines(as.numeric(L)[G==0], fitted(rez.LG)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.LG)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.LG)[G==2], type="l")

plot(as.numeric(L), rez.sat$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="L*G")
lines(as.numeric(L)[G==0], fitted(rez.sat)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.sat)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.sat)[G==2], type="l")

plot(as.numeric(L), rez.L$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="L")
lines(as.numeric(L)[G==0], fitted(rez.L)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.L)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.L)[G==2], type="l")

plot(as.numeric(L), rez.G$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="G")
lines(as.numeric(L)[G==0], fitted(rez.G)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.G)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.G)[G==2], type="l")

plot(as.numeric(L), rez.1$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="1")
lines(as.numeric(L)[G==0], fitted(rez.1)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.1)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.1)[G==2], type="l")


rez.LK <- glm(SF ~ LK, family = binomial)
rez.LKG <- glm(SF ~ LK + G, family = binomial)
rez.LKGi <- glm(SF ~ LK * G, family = binomial)

anova(rez.1,rez.LK,rez.LKG,rez.LKGi, test="Chisq")

summary(rez.LKG)$coefficients

plot(as.numeric(L), rez.LKGi$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="L*G")
lines(as.numeric(L)[G==0], fitted(rez.LKGi)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.LKGi)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.LKGi)[G==2], type="l")

plot(as.numeric(L), rez.LKG$y, type="p", xlab="L", ylab="prob",
     ylim=c(0,1), main="L+G")
lines(as.numeric(L)[G==0], fitted(rez.LKG)[G==0], type="l")
lines(as.numeric(L)[G==1], fitted(rez.LKG)[G==1], type="l")
lines(as.numeric(L)[G==2], fitted(rez.LKG)[G==2], type="l")



# =======================================================
# Interpretation as a simple (2 level) multinomial model:
# =======================================================
y <- c(rez, total-rez)
R <- c(rep(1, length(rez)), rep(0, length(rez)))

rezidiveMN <- data.frame(y, R, c(LK,LK), c(GZ,GZ))
dimnames(rezidiveMN)[[2]] <- c("y", "R", "LK.MN", "GZ.MN")

#detach("rezidive")
attach(rezidiveMN)
L <- factor(LK.MN)
G <- factor(GZ.MN)
R <- factor(R)
MN.glm <- glm(y ~ R*(L+G) + L*G, family=poisson)
MN.sum <- summary(MN.glm)

coefficients(MN.sum)
coefficients(rez.LG)

deviance(MN.sum)
deviance(rez.LG)

grid <- list(R=levels(R), L=levels(L), G=levels(G))
MN.p <- predict(MN.glm, expand.grid(grid), type="response")
pred <- t(matrix(MN.p, 8))
pred


MN.glm1 <- glm(y ~ R + L*G, family=poisson)
MN.sum1 <- summary(MN.glm1)
deviance(MN.sum1)
MN.sum1$df

MN.p1 <- predict(MN.glm1, expand.grid(grid), type="response")
pred <- t(matrix(MN.p1, 8))
pred
pred[1,2]/pred[1,1]
pred[3,4]/pred[3,3]


# =========================================================
# Direct estimation by means of "multinom" in library(nnet)
# =========================================================
library(nnet)

MN.direct <- multinom(R ~ L+G, weights=y)
summary(MN.direct)
# Here the deviance compares the model with the model predicting each
# single subject (out of 313) correctly.
# (and not with the saturated model of the 12 entries in the 3 x 4 table)

MN.direct.sat <- multinom(R ~ L*G, weights=y)
summary(MN.direct.sat)
anova(MN.direct, MN.direct.sat) # resulting deviance difference

# the multinomial deviance can be derived by
MN.direct$deviance - MN.direct.sat$deviance
# this value exactly compares with the Binomial logistic deviance


# =========================================================
# Check independence
# =========================================================
y <- c(153, 23, 12, 2, 43, 29, 23, 28)
L <- factor(c(seq(0,3), seq(0,3)))
R <- factor(c(rep(0,4), rep(1,4)))

summary(LK.mod <- glm(y ~ L + R, family=poisson)) # (1,1)=reference cell

LK.mod <- glm(y ~ C(L, base=4) + R, family=poisson) # (4,1)=reference cell
summary(LK.mod)

options(contrasts=c("contr.sum","contr.poly"))
LK.mod <- glm(y ~ L + R, family=poisson) # contrasts
summary(LK.mod)
options(contrasts=c("contr.treatment","contr.poly"))

# Pearson Statistic
r <- residuals(LK.mod, type="pearson")
sum(r^2)

# alternative calculation
N <- matrix(y, 2, 4, byrow=TRUE)
N
chisq.test(N) # only available for I x J tables

