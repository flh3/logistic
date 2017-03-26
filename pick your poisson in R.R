
### REPLICATING POISSON RESULTS

x <- rio::import("C:/Users/flh3/Dropbox/Bully/Bully/CountJK/MASTERDATA7-09.sav")
names(x)

x2 <- dplyr::select(x, NEWBULLY, TOTAL_F_R, 
                    DIVERSITYINDEX, PROPNONWHITE, PD1000, siz1000
                    )
stargazer::stargazer(x2, type = 'text', digits = 2)

m1 <- lm(NEWBULLY ~ . , data = x2)
summary(m1)

m2 <- glm(NEWBULLY ~ . , family = poisson, data = x2)
summary(m2)
exp(coef(m2))

with(m2, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE))
     
library(MASS)
library(pscl)

mod4 <- glm.nb(NEWBULLY ~ ., data = x2)
summary(mod4)
exp(coef(mod4))

summary(mod4)$theta
odTest(mod4)
AER::dispersiontest(m2)

m5 <- zeroinfl(NEWBULLY ~ . | siz1000, dist = "negbin", data = x2)
summary(m5)
exp(coef(m5))

p1 <- fitted(m5)
hist(p1, breaks = 100)
p2 <- fitted(mod4)
p2d <- density(p2)
p3 <- density(fitted(m2))

plot(density(p1))
lines(p2d, col = 'red')
lines(p3, col = 'blue')

p1i <- round(p1)
t1 <- table(p1i)
plot(t1)
range(p1)
