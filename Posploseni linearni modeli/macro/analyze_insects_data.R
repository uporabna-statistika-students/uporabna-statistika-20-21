insects <- read.table("insects.dat"); attach(insects)

block     <- factor(rep(1:10, len=140)); block
plot      <- factor(rep(rep(1:2, each=10), times=7)); plot
treatment <- factor(rep(1:7, each=20)); treatment

insects <- data.frame(insects, data.frame(treatment, plot, block))
colnames(insects) <- c("counts", "treatment", "plot", "block")
attach(insects)

# Normal linear model with (treatment x block) interaction
i.lm <- lm(counts ~ treatment*block)
r <- residuals(i.lm)
f <- fitted(i.lm)
hist(r)
qqnorm(r); qqline(r)
plot(f, r, xlab="fitted means"); abline(0,0)

cell.mean <- tapply(counts, list(treatment, block), mean); cell.mean
cell.sd   <- tapply(counts, list(treatment, block), sd);   cell.sd
plot(cell.mean, cell.sd); abline(mean(cell.sd), 0)
abline(lsfit(as.vector(cell.mean), as.vector(cell.sd)))

trt.mean <- tapply(counts, treatment, mean)
trt.sd   <- tapply(counts, treatment, sd)
plot(trt.mean, trt.sd); abline(lsfit(trt.mean, trt.sd))

# Gamma reciprocal model with (treatment x block) interaction
i.glmmax <- glm(counts ~ treatment * block, family = Gamma)
summary(i.glmmax)
r <- residuals(i.glmmax, type = "pearson") # Pearson Residuals
s.glmmax <- summary(i.glmmax) # generate summary object
(df.max <- s.glmmax$df[2]) # [1]=#(parameters), [2]=df
(scale <- sum(r^2) / df.max) # mean Pearson statistic
(dev.max <- s.glmmax$deviance / scale) # scaled Deviance
plot(f, r, xlab = "fitted means"); abline(0, 0)

# Gamma reciprocal model with (treatment, block) main effects
i.glmmain <- glm(counts ~ treatment + block, family = Gamma)
s.glmmain <- summary(i.glmmain)
(df.main <- s.glmmain$df[2]) # 124
(dev.main <- s.glmmain$deviance / scale) # 153.5404
1-pchisq(dev.main-dev.max, df.main-df.max) # 0.207146

# Gamma reciprocal model with treatment main effect only
i.glmt <- glm(counts ~ treatment, family=Gamma)
s.glmt <- summary(i.glmt)
(df.t <- s.glmt$df[2]) # 133
(dev.t <- s.glmt$deviance / scale) #  235.8278

# Gamma reciprocal model with black main effect only
i.glmb <- glm(counts ~ block, family = Gamma)
s.glmb <- summary(i.glmb)
(df.b <- s.glmb$df[2]) # 130
(dev.b <- s.glmb$deviance / scale) # 248.4376
s.glmmain$coefficients


# alternatively, simply use
anova(glm(counts ~ treatment * block, family = Gamma), test="Chisq")



# predict new responses given the main effects gamma model
new.i <- data.frame(treatment=factor(1:7), block=factor(rep(1,7)))
i.pred <- predict.glm(i.glmmain, newdata = new.i, type = "response", se.fit = T); i.pred

fit   <- i.pred$fit
upper <- fit + qt(0.975, df.main)*i.pred$se.fit
lower <- fit - qt(0.975, df.main)*i.pred$se.fit

# alternatively, there is a function confint in the library MASS
library(MASS)
confint()


plot(new.i$treatment, upper, style="box", xlab="Treatment",
     ylab="mean count", ylim=c(0.0,40.0), main="Block 1")
par(new=TRUE); plot(new.i$treatment, fit,   style="box", ylim=c(0.0,40.0))
par(new=TRUE); plot(new.i$treatment, lower, style="box", ylim=c(0.0,40.0))
