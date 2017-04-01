
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
#b0 <- -1.09
#b0 <- -1.38 #.05
b0 <- -2.944
b1 <- -.69 #binary
b2 <- -.36


reps <- 1000
bs2 <- ps2 <- ps <- bs <- matrix(1, nrow = reps, ncol = 4)
inter <- hetero <- viol <- numeric(reps)

pb <- txtProgressBar(min = 0, max = reps, style = 3)
for (i in 1:reps){
 
  x1 <- rbinom(ns, 1, .5)
  x2 <- rnorm(ns)
	predlo <- b0 + (x1*b1) + (x2*b2)
	y <- rbinom(ns, 1, pp(predlo))
	
	a1 <- lm(y ~ x1 + x2 )
	hetero[i] <- bptest(a1)$p.value #p values
	
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
	
	ps[i, 3] <- summary(a2)$coef[2,4] #x1
	ps[i, 4] <- summary(a2)$coef[3,4] #x2
	bs[i, 3] <- summary(a2)$coef[2,1] #b1 LOG
	bs[i, 4] <- summary(a2)$coef[3,1] #b2 LOG
	inter[i] <- summary(a2)$coef[1,1]
	
	df1 <- data.frame(y, x1, x2)
	df1$pp <- fitted(a1)
	df1$pp2 <- fitted(a2)
	#print(i)
	viol[i] <- sum(df1$pp < 0 | df1$pp > 1)
	setTxtProgressBar(pb, i)
}
close(pb)

##############
table(viol)
#mean(viol)/ns
 table(viol)[1] / reps #% within bounds, 20% had no violations
df1 <- data.frame(table(viol))
df1$viol <- as.numeric(as.character(df1$viol))
Hmisc::wtd.mean(df1$viol[-1], df1$Freq[-1])
range(df1$viol[-1])
#test <- lm(y ~ x1 + exp(I(x2)), data = df1)

plot(df1$x2, df1$pp) #ols
points(df1$x2, df1$pp2, pch = 2) #binomial

range(pred)
abline(h = 0)

#ps2, beta 2, continuous

 
#######
##########
sum(hetero < .05) / reps #% with hetero violations

plot(df1$x2, df1$pp, col = 'red',ylim = c(0,1))
points(df1$x2, df1$pp2, pch = 2)
abline(h = 0)

#library(lattice)
sum(ps[,1] < .05) #corrected
sum(ps[,2] < .05) #not corrected
sum(ps[,3] < .05) #log reg

#regression slope 2
sum(ps[,4] < .05) #log reg, b2
sum(ps2[,1] < .05) #corrected
sum(ps2[,2] < .05) #not corrected


colMeans(bs) #slopes, first is ols, 2nd is log, 3rd is LRM, 4th is LRM 2nd
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