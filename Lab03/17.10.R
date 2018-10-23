#Zadanie 1
dane = read.table("http://mini.pw.edu.pl/~bobecka/bakterie.txt",header=TRUE)
x = dane$t
y=dane$N_t

#wykres rozproszenia danych
plot(x,y)

#a

#model
m1 = lm(y~x)
abline(m1,col="red")

summary(m1)

r1 = m1$residuals
plot(r1)
abline(h=0)

qqnorm(r1)
qqline(r1)

#model nie pasuje, widac po wykresach, mimo ze diagnostyka (summary) daje co innego

#b

z = log(y)

plot(x,z)
m2=lm(z~x)
abline(m2,col="red")

plot(x,y)
lines(x,m1$fitted.values,col="red")
lines(x,exp(m2$fitted.values),col="green")

summary(m2)

r2 = m2$residuals
plot(r2)

qqnorm(r2)
qqline(r2)

shapiro.test(r2)

#prognoza
#dla modelu 1
predict(m1,newdata=list(x=20))
#dla modelu 2
a = predict(m2,newdata = list(x=20))
exp(a)


#Zadanie 3
dane2 = read.table("http://mini.pw.edu.pl/~bobecka/drgania.txt",header = TRUE)
head(dane2)
x=dane2$odl
y=dane2$drg

plot(x,y)

#a
#model odwrotnoœciowy wzgl x
z = 1/x

plot(z,y)
m = lm(y~z)
abline(m,col="red")
summary(m)
r = m$residuals
plot(r)
qqnorm(r)
qqline(r)

shapiro.test(r)

#model odwrotnosciowy wzgl y 
z = 1/y

plot(x,z)
m = lm(z~x)
abline(m,col="red")
summary(m)
r = m$residuals
plot(r)
qqnorm(r)
qqline(r)

shapiro.test(r)

#model wykladniczy
z = log(y)

plot(x,z)
m = lm(z~x)
abline(m,col="red")
summary(m)
r = m$residuals
plot(r)
qqnorm(r)
qqline(r)

shapiro.test(r)

#prognoza dla modelu odwr. wzgl. x
predict(m,newdata=list(z=1/100))

#Zadanie 5
dane3 = read.table("http://mini.pw.edu.pl/~bobecka/cellular.txt",header = TRUE)
x = dane3$Period
y = dane3$Subscribers

#a
plot(x,y)

m1 = lm(y~x)
abline(m1,col="red")
summary(m1)
r1 = m1$residuals
plot(r1)
qqnorm(r1)
qqline(r1)
shapiro.test(r1)

#c
z1 = sqrt(y)
plot(x,z1)
m2 = lm(z1~x)
abline(m2,col="red")
summary(m2)
r2 = m2$residuals
plot(r2)
qqnorm(r2)
qqline(r2)
shapiro.test(r2)

#b
z2=log(y)
plot(x,z2)
m3 = lm(z2~x)
abline(m3,col="red")
summary(m3)
r3 = m3$residuals
plot(r3)
qqnorm(r3)
qqline(r3)
shapiro.test(r3)

#d
z3 = y^(1/4)
plot(x,z3)
m4 = lm(x~z3)
abline(m4,col="red")
summary(m4)
r4 = m4$residuals
plot(r4)
qqnorm(r4)
qqline(r4)
shapiro.test(r4)

#e
library(MASS)
#box-cox dla modelu m1 bo szukamy lambdy
b = boxcox(m1,plotit = T)

#dodatkowo szukamy dla jakiego x jest max y
which(b$y==max(b$y))
b$x[71]

#patrzymy z bliska na wykres 
boxcox(m4,lambda = seq(0.0,0.4,len = 20))


#Zadanie 2
dane4 = read.table("http://mini.pw.edu.pl/~bobecka/onko.txt",header = TRUE)
x = dane4$dawka
y = dane4$red

plot(x,y)

mod = lm(y~x)
boxcox(mod,plotit = T)

#lambda = 1/2; y = (a+bx)^2
z = sqrt(y)
mod2 = lm(z~x)
plot(x,z)
abline(mod2,col="red")
summary(mod2)
r = mod2$residuals
plot(r)
qqnorm(r)
qqline(r)
shapiro.test(r)

#lambda = 2; y^2 = a+bx
z2 = y^2
mod3 = lm(z2~x)
plot(x,z2)
abline(mod3,col="red")
summary(mod3)
r = mod3$residuals
plot(r)
qqnorm(r)
qqline(r)
shapiro.test(r)

#y=a+bx+cx^2
x1 = x
x2 = x^2
mod4 = lm(y~x1+x2)
summary(mod4)
r = mod4$residuals
plot(r)
qqnorm(r)
qqline(r)
shapiro.test(r)

#Zadanie 6

data(hills)
head(hills)
x1 = hills$dist
y = hills$time
x2 = hills$climb

model = lm(y~x1+x2)
summary(model)
(summary(model)$sigma)^2
