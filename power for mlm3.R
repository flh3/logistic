
library(sandwich)
library(MASS)
library(nlme)
library(lme4)
library(lmerTest)
library(lmtest)

### logits to pp
pp <- function(x){
   exp(x)/(1+exp(x))
   
}


##################



ns <- 500
b0 <- -1.09
b0 <- -.2
#b1 <- -.22
b1 <- -.69

test <- (b0 + b1)
pp(b0)
pp(test)

reps <- 1000
ps <- bs <- matrix(1, nrow = reps, ncol = 3)
inter <- hetero <- numeric(reps)

for (i in 1:reps){
	#x <- rnorm(ns)
   
  x <- rbinom(ns, 1, .5)
	predlo <- b0 + (x*b1)
	y <- rbinom(ns, 1, pp(predlo))
	a1 <- lm(y ~ x)
	hetero[i] <- bptest(a1)$p.value
	
	#if (hetero[i] < .05) {
	  pad <- coeftest(a1, vcov. = vcovHC)
	  ps[i, 1] <- pad[2,4]
  #} else {
	  ps[i, 2] <- summary(a1)$coef[2,4]
	  
	#}
	
	#ps[i, 1] <- summary(a1)$coef[2,4]
	bs[i, 1] <- summary(a1)$coef[2,1]
	
	a2 <- glm(y ~ x, family = binomial)
	ps[i, 3] <- summary(a2)$coef[2,4]
	bs[i, 2] <- summary(a2)$coef[2,1]
	inter[i] <- summary(a2)$coef[1,1]
	print(i)
}



sum(hetero < .05) 


#library(lattice)
sum(ps[,1] < .05) #corrected
sum(ps[,2] < .05) #not corrected
sum(ps[,3] < .05)

colMeans(bs) #slopes, first is ols, 2nd is log
mean(inter) #intercept

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


range(predict(a1))
pols <- predict(a1)
#plog <- predict(a2, type = 'response')
plog <- fitted(a2)
cor(pols, plog)
plot(pols, plog)

exp(coef(a2))
summary(a2)
summary(a1)
exp(-.3)