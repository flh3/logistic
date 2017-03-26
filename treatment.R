#binary

.2/.8 #orig
.16/.84 #target
log(.25) #orig lodds, -1.38
(.25-.19)/.25 #change in lodds
.25*.76

ns <- 300
br <- -1.39
change <- -.05

reps <- 1000
ps <- bs <- matrix(1, nrow = reps, ncol =2)

for (i in 1:reps){
  x <- rnorm(ns)
  predlo <- br + (x*change)
  y <- rbinom(ns, 1, pp(predlo))
  a1 <- lm(y ~ x )
  ps[i, 1] <- summary(a1)$coef[2,4]
  bs[i, 1] <- summary(a1)$coef[2,1]
  a2 <- glm(y ~ x, family = binomial)
  ps[i, 2] <- summary(a2)$coef[2,4]
  bs[i, 2] <- summary(a2)$coef[2,1]
  print(i)
}


#library(lattice)
sum(ps[,1] < .05)
sum(ps[,2] < .05)




range(predict(a1))
range(predict(a2, type = 'response'))
pols <- predict(a1)
plog <- predict(a2, type = 'response')
cor(pols, plog)
plot(pols, plog)

df2 <- data.frame(pols, plog)
colMeans(df2)
apply(df2, 2, range)
###

summary(a1)$r.squared
1/exp(coef(a2))

summary(a1)
summary(a2)
library(lmtest)
bptest(a1)
plot(a1)
summary(a1)
library(car)
ncvTest(a1)
exp(-2.5)/exp(-2.3)

###
exp(coef(a2))
summary(a2)
summary(a1)
exp(-.3)
