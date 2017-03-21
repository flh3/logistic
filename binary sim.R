

library(MASS)
library(nlme)
library(lme4)
library(lmerTest)

### logits to pp
pp <- function(x){
  exp(x)/(1+exp(x))
  
}

pp(-2.332)

J <- 5
nJ <- 20

Nt <- J * nJ
x <- rep(c(0,1), each = Nt / 2)
grp <- rep(seq(1:J), each = nJ) 

pi/sqrt(3)
sqrt(3)/pi

pi^2/3

l2v <- .6
l2v/(3.29 + l2v)

test <- rnorm(J, 0, sqrt(.6))
test2 <- rep(test, each = nJ)

reps <- 1000

for (i in 1:reps){
  if (i == 1) {pv <- mp <- mp2 <- icc <- numeric(reps) }
  
  
  test <- rnorm(J, 0, sqrt(.6))
  test2 <- rep(test, each = nJ)
  
  predlo <- -2.332 + x*1.663 + test2
  test <- rbinom(Nt, 1, pp(predlo))
  mean(test)
  
  # tapply(test, x, mean)
  
  
  ml0 <- lmer(test ~ (1|grp))
  vc <- as.data.frame(VarCorr(ml0))
  
  icc[i] <- vc[1,4] / (vc[1,4] + vc[2,4] ) 
  
  res <- (glm(test ~ x, family = binomial))
  
  #summary(res)$coef
  pv[i] <- summary(res)$coef[2,4]
  
  ml1 <- lmer(test ~ x + (1|grp))
  mp[i] <- summary(ml1)$coef[2,5]
  
  ml2 <- glmmPQL(test ~ x, ~1|grp, family = 'binomial')
  mp2[i] <- summary(ml2)$tTable[2,5]
  
  
  print(i)
  
}

mean(icc)



ml0 <- lmer(test ~ (1|grp))
ml0
str(VarCorr(ml0))
ml0$grp[2]




sum(pv < .05) / reps
sum(mp < .05) / reps
sum(mp2 < .05) / reps

df2 <- data.frame(test,x,grp)


ml1 <- lmer(test ~ x + (1|grp))
summary(ml1)

library(nlme)

str(ml2)
summary(ml2)
summary(ml2)$tTable
oo <- summary(ml2)
str(oo)


##################



ns <- 300
br <- -3
change <- -.22

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
