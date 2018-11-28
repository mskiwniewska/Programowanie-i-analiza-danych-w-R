dane = read.table("http://www.mini.pw.edu.pl/~bobecka/owady.txt",header=TRUE)

plot(dane$conc,dane$dead/dane$number)

tab <- cbind(dane$dead,dane$number-dane$dead)

m <- glm(tab~dane$conc,family=binomial)

summary(m)

m$null.deviance

m$deviance

D <- m$null.deviance - m$deviance


a <-  m$df.null - m$df.residual

m0 <- glm(tab~1,family=binomial)

anova(m0,m,test="Chi")


D2 <- m$dev

pv <- 1-pchisq(D2,m$df.residual)

1-m$deviance/m$null


predict(m,newdata=list(conc=2.5),type="response")



plot(dane$conc,dane$dead/dane$number)
points(dane$conc,m$fit,col="red",pch=3)

####Zad.4

dane2 = read.table("http://www.mini.pw.edu.pl/~bobecka/bankruci.txt",header=TRUE)

m2 <- glm(Y~X1+X2+X3,data=dane2, family=binomial)

1-m2$deviance/m2$null

predict(m2,newdata=list(X1=50,X2=15,X3=1),type="response")




