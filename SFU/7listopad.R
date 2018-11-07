dane <- read.table("http://www.mini.pw.edu.pl/~bobecka/diab.txt")
m <- lm(Y~.,dane)
summary(m)
library(car)
avPlots(m)
avPlots(m,"AGE")
avPlots(m,"S2")
crPlots(m)

library(leaps)

X=model.matrix(m)[,-1]
r.sk=leaps(X,dane$Y,method="adjr2",nbest=3)
r.sk
max(r.sk$adjr2)
r.sk[which(r.sk$adjr2==max(r.sk$adjr2)),]
r.sk$which[22,]

r.sk2=leaps(X,dane$Y,method="Cp",nbest=3)
r.sk2
min(r.sk2$Cp)
which(r.sk2$Cp==min(r.sk2$Cp))
r.sk2$which[16,]

names(dane)[r.sk2$which[16,]==FALSE]

extractAIC(m)
library(MASS)
stepAIC(m)

step(m,direction="both")

m0=lm(Y~1,data=dane)
add1(m0,m,test="F")

m1=update(m0,.~.,+BMI)
add1(m1,m,test="F")


summary(regsubsets(Y~.,data=dane,method="forward"))


drop1(m,test="F")
m1=update(m,.~.,-AGE)
drop1(m1,test="F")

####Lista 4

data("phones")

data2 <- phones

m3=lm(data2$calls~data2$year)

summary(m3)

# obserwacje odstaj¹ce

outlierTest(m3)



