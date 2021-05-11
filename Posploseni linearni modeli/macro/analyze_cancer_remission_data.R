li <- c(seq(8,28,2),32,34,38)
total <- c(2,2,3,3,3,1,3,2,1,1,1,1,1,3)
back <- c(0,0,0,0,0,1,2,1,0,1,1,0,1,2)

SF <- cbind(back, nonback=total-back)
carcinoma <- glm(SF ~ li, family=binomial); summary(carcinoma)

beta <- carcinoma$coeff

-beta[1]/beta[2]

N <- sum(total)

mean.li <- sum(total*li)/N

eta <- beta[1] + beta[2]*mean.li

exp(eta)/(1+exp(eta))

sum(back)

sum(back)/N


anova(carcinoma, test="Chisq")
