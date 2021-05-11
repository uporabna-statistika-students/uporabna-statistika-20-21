ldose <- rep(0:5, 2); ldose
dead <- c(1,4,9,13,18,20,0,2,6,10,12,16); dead
sex <- factor(rep(c("M", "F"), c(6, 6))); sex


# response information as 2 col matrix, with #(successes) and #(failures)
SF <- cbind(dead, alive = 20-dead); SF
budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
summary(budworm.lg)

# alternatively, we can code the response information as:
summary(glm(dead/20 ~ sex*ldose, family = binomial, weights=rep(20,12)))


# plot the estimated model together with the data
plot(c(1,32), c(0,1), type="n", xlab="dose", ylab="prob", log="x")
text(2^ldose, dead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
                                           sex=factor(rep("M", length(ld)), levels=levels(sex))),type="response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
                                           sex=factor(rep("F", length(ld)), levels=levels(sex))),type="response"))
# sex doesn't seem to be necessry!!

# if we are interested in comparing the sex effect at ldose=3, then
budworm.lg8 <- update(budworm.lg, . ~ sex*I(ldose-3))
summary(budworm.lg8)

anova(budworm.lg, test = "Chisq")

# quadratic effect?
# Analysis of deviance based on Chisq
# no unknown dispersion => F would result in the same p-values
anova(update(budworm.lg, . ~ . + sex*I(ldose^2)), test = "Chisq")
# no! also the sex:ldose interaction can be neglected

# separat parameters for each gender:
budworm.lg0 <- glm(SF ~ sex + ldose - 1, family = binomial)
summary(budworm.lg0)

library(MASS)
dose.p(budworm.lg0, cf = c(1,3), p = 1:3/4)

dose.p(budworm.lg0, cf = c(2,3), p = 1:3/4)

dose.p(update(budworm.lg0, family=binomial(link=probit)),
       cf = c(1,3), p = 1:3/4)
