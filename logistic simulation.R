
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



ns <- 400
b0 <- -1.09
b1 <- -.69
b2 <- .36


test <- (b0 + b1 + b2)
#pp(b0)
#pp(test)

reps <- 1
bs2 <- ps2 <- ps <- bs <- matrix(1, nrow = reps, ncol = 4)
inter <- hetero <- numeric(reps)

for (i in 1:reps){
	#x <- rnorm(ns)
   
  x1 <- rbinom(ns, 1, .5)
  x2 <- rnorm(ns)
	predlo <- b0 + (x1*b1) + (x2*b2)
	y <- rbinom(ns, 1, pp(predlo))
	
	a1 <- lm(y ~ x1 + x2)
	hetero[i] <- bptest(a1)$p.value
	
	  #corrected
	  pad <- coeftest(a1, vcov. = vcovHC)
	  ps[i, 1] <- pad[2,4]
	  ps2[i, 1] <- pad[3,4] #conti
	  
	  ### stadnard
	  ps[i, 2] <- summary(a1)$coef[2,4]
	  ps2[i, 2] <- summary(a1)$coef[3,4]
	  

	#ps[i, 1] <- summary(a1)$coef[2,4]
	bs[i, 1] <- summary(a1)$coef[2,1] #b1 OLS
	bs[i, 2] <- summary(a1)$coef[3,1] #b2 OLS
	
	a2 <- glm(y ~ x1 + x2, family = binomial)
	ps[i, 4] <- summary(a2)$coef[3,4] #x2
	ps[i, 3] <- summary(a2)$coef[2,4] #x1
	bs[i, 3] <- summary(a2)$coef[2,1] #b1 LOG
	bs[i, 4] <- summary(a2)$coef[3,1] #b2 LOG
	inter[i] <- summary(a2)$coef[1,1]
	
	df1 <- data.frame(y, x1, x2)
	df1$pp <- fitted(a1)
	df1$pp2 <- fitted(a2)
	print(i)
}


sum(hetero < .05) 

plot(df1$x2, df1$pp, col = 'red')
points(df1$x2, df1$pp2)
abline(h = 0)

#library(lattice)
sum(ps[,1] < .05) #corrected
sum(ps[,2] < .05) #not corrected
sum(ps[,3] < .05) #log reg
sum(ps[,4] < .05) #log reg, b2
sum(ps2[,1] < .05) #corrected
sum(ps2[,2] < .05) #not corrected


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