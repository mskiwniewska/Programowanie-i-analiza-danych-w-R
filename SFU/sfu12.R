#Zestaw 1
#Zad 1
#1.1
x=c(4,1,3,2)
t(x)

t(x)%*%x
crossprod(x)

x%*%t(x)
tcrossprod(x)


d <- sqrt(crossprod(x)) #dl.wektora
x/as.vector(d) #unormowany

x-mean(x) #centrowanie
scale(x,scale=FALSE)

(x-mean(x))/sd(x) #standaryzacja wektora
scale(x)

d=as.matrix(x)
t(d)

#Zad 2
y=c(1,-1,3,0)
t(x) %*% y #iloczyn skalarny
crossprod(x,y)

#Zad 3
D=diag(c(1,2,3))
I=diag(c(1,1,1))
I=diag(rep(1,3))

#Zad 4

A=matrix(c(1,5,1,2,3,8),3,2, byrow = T)
B=matrix(c(1,4,3,2),2,2, byrow = T)

A%*%B #iloczyn
B%*%t(A)
kronecker(A,B)

#Zad 5
A=matrix(c(3,4,6,1,2,3,5,7,9),3,3, byrow = T)

sum(diag(A)) #slad
det(A)
solve(A) #m.odwrotna

#Zad 6
A=matrix(c(10,3,9,3,40,8,9,8,15),3,3, byrow=TRUE)

eigen(A)
eigen(A)$values
eigen(A)$vectors

chol(A) #rozklad choleskiego
U=chol(A) #czyli gornotrojkatna  U t.ze A=U'U

H=eigen(A)$vectors #ortogonalna

D=diag(eigen(A)$values) #diagonalna

H%*%D%*%t(H) #A=HDH'

#pierwiastek z macierzy

H%*%sqrt(D)%*%t(H) -> B

B %*% B

#Zad.7

#rozklad A na wartosci szczegolne

A=matrix(c(1,1,0,1,1,0),3,2,byrow=TRUE)
svd.A=svd(A, nu=nrow(A), nv=ncol(A))


U=svd.A$u
V=svd.A$v
D=rbind(diag(svd.A$d),c(0,0))
round(U%*%D%*%t(V))
#eigen(A%*%t(A))
#d=rbind(diag(svd.A$d),c(0,0))
#D=rbind(diag(svd.A$d),c(0,0))
#D
#U-svd.A$u
#round(U%*%D%*%t(v))
#U=svd.A$u

#zad.8

A=matrix(c(1,1,0,2,1,1,2,0,2,0,1,1),3,4,byrow=TRUE)

svd.A=svd(A, nu=nrow(A), nv=ncol(A))


U=svd.A$u
V=svd.A$v
D=cbind(diag(svd.A$d),c(0,0,0))
round(U%*%D%*%t(V))





#Zad.9
y=c(57.5, 52.8, 61.3, 67, 53.5, 62.7, 56.2, 68.5, 69.2)
x=c(78, 69, 77, 88, 67, 80, 74, 94, 102)
plot(x,y)

m=lm(y~x)
m$coefficients
X=model.matrix(m)
summary(m)

#(solve(t(X)%*%X))
#(solve(t(X)%*%X))%*%t(X)%*%Y
#(solve(t(X)%*%X))%*%t(X)%*%y

sig=summary(m)$sigma

deviance(m)

m$residuals

sum(m$residuals^2)
n=length(y)

deviance(m)/(n-2)

m$fitted.values

y-m$fitted.values
m$residuals

plot(x,y)
abline(m, col="red", lwd=2)
predict(m, newdata=list(x=90))

#Drugie zajecia:

#Zestaw 1 
#dokonczenie

#Zad.11
library(MASS)
data(hills)
x1=hills$dist
y=hills$time
x2=hills$climb

m=lm(y~x1+x2)
m

# p=3
m$coefficients
(summary(m)$sigma)^2
predict(m, newdata = list(x1=10, x2=2500))

#################

#Zad.2

data("anscombe")
anscombe
attach(anscombe)
x1 #zamiast anscombe$x1

par(mfrow=c(2,2))
m1=lm(anscombe$y1~anscombe$x1)
plot(anscombe$x1,anscombe$y1)
abline(m1, col="red", lwd=2)

m2=lm(anscombe$y2~anscombe$x2)
plot(anscombe$x2,anscombe$y2)
abline(m2, col="red", lwd=2)

m3=lm(y3~x3)
plot(x3,y3)
abline(m3, col="red", lwd=2)

m4=lm(y4~x4)
plot(x4,y4)
abline(m4, col="red", lwd=2)

summary(m1)
summary(m2)
summary(m3)
summary(m4)

r1=m1$residuals
plot(r1)
plot(anscombe$x1,r1)
plot(y1, r1)
plot(m1$fitted.values,r1)

r2=m2$residuals
plot(r2)
plot(x2,r2)
plot(y2, r2)
plot(m1$fitted.values,r2)

r3=m3$residuals
plot(r3)
plot(x3,r3)
plot(y3, r3)
plot(m1$fitted.values,r3)

r4=m4$residuals
plot(r4)
plot(x4,r4)
plot(y4, r4)
plot(m1$fitted.values,r4)

####### Wykresy normalnosci
#test shapiro wilka
shapiro.test(r1)
shapiro.test(r2)
shapiro.test(r3)
shapiro.test(r4)

#wykresy quantylowe
qqnorm(r1)
qqline(r1)

qqnorm(r2)
qqline(r2)

qqnorm(r3)
qqline(r3)

qqnorm(r4)
qqline(r4)

############################ zadanie 2 z listy nr2

data("trees")
trees
attach(trees)

mg=lm(Volume~Girth)

mh=lm(Volume~Height)
par(mfrow=c(2,1))

plot(Girth,Volume)
abline(mg, col="red", lwd=2)

plot(Height,Volume)
abline(mh, col="red", lwd=2)

summary(mg)
summary(mh)
summary(mg)$r.squared
summary(mh)$r.squared

confint(mg, level=0.95)
trees
predict(mg, newdata = list(Girth=21))

predict(mg, newdata = list(Girth=21), int="c", lwd=0.95)
predict(mg, newdata = list(Girth=21), se=T)

predict(mg, newdata = list(Girth=21), int="p")  #prediction