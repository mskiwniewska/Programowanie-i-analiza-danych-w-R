y <- c(204,200,198,204,197,205,213,209,190,208,202,210)
a <- rep(1:3,each=4)
a <- factor(a)
boxplot(y~a)

sr <- tapply(y,a,mean)
sr
stripchart(y~a,vert=TRUE)
lines(sr,type="b",pch=20)

tapply(y,a,qqnorm)
tapply(y,a,shapiro.test)


bartlett.test(y~a)

m <- aov(y~a)

m1 <- lm(y~a)
anova(m1)

model.matrix(m1)

contrasts(a)

TukeyHSD(m)
plot(TukeyHSD(m))
pairwise.t.test(y,a,p.adjust.method="bonferroni")




#zad.2

y <- dane$zarobki

a <- factor(dane$miasto)

boxplot(y~a)

data <- split(y, a) #  wynik: lista wektorow
srednie <- sapply(data, mean)

points(axTicks(1), srednie , pch=16, cex=2, col=4)



install.packages("dunn.test")
library(dunn.test)
dunn.test(y,a,method="bonferroni")



