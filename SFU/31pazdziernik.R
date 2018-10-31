dane <- read.table("http://www.mini.pw.edu.pl/~bobecka/rzeki.txt",header=TRUE)

y <- dane$Nitrogen
x1 <- dane$Agr
x2 <- dane$Forest
x3 <- dane$Rsdntial
x4 <- dane$ComIndl


m <- lm(y~x1+x2+x3+x4)

summary(m)

r1 <- m$residuals

r2 <- rstandard(m)

par(mfrow=c(2,1))
plot(r1)
plot(r2)

dane1 <- subset(dane,River!="Neversink")

y <- dane1$Nitrogen
x1 <- dane1$Agr
x2 <- dane1$Forest
x3 <- dane1$Rsdntial
x4 <- dane1$ComIndl

m1 <- lm(y~x1+x2+x3+x4)

summary(m1)

r1_2 <- m1$residuals

r2_2 <- rstandard(m1)

par(mfrow=c(2,1))
plot(r1_2)
plot(r2_2)


dane2 <- subset(dane,River!="Hackensack")

y <- dane2$Nitrogen
x1 <- dane2$Agr
x2 <- dane2$Forest
x3 <- dane2$Rsdntial
x4 <- dane2$ComIndl

m2 <- lm(y~x1+x2+x3+x4)

summary(m2)

r1_3 <- m2$residuals

r2_3 <- rstandard(m2)

par(mfrow=c(2,1))
plot(r1_3)
plot(r2_3)


y <- dane$Nitrogen
x1 <- dane$Agr
x2 <- dane$Forest
x3 <- dane$Rsdntial
x4 <- dane$ComIndl
m4 <- lm(y~x4)
summary(m4)

r1_4 <- m4$residuals

r2_4 <- rstandard(m4)

par(mfrow=c(2,1))
plot(r1_4)
plot(r2_4)

par(mfrow=c(1,1))
plot(x4,y)
abline(m4,col="green")

dane4 <- subset(dane,River!="Hackensack")
m5 <-  lm(dane4$Nitrogen~dane4$ComIndl)
abline(m5,col="blue")

dane5 <- subset(dane,River!="Neversink")
m6 <-  lm(dane5$Nitrogen~dane5$ComIndl)
abline(m6,col="black")

library(car)
outlierTest(m6)
hi <- hatvalues(m6)
lm.influence(m6)
influence.measures(m6)
plot(m6,which=4)
n <- nrow(dane5)
p <- n - m6$df

hi[hi>2*(p/n)]

di <- cooks.distance(m6)
di[di>1]

dffits(m6)



