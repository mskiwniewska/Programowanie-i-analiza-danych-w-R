### ZESTAW 1

# Zad.9

y=c(57.5, 52.8, 61.3, 67, 53.5, 62.7, 56.2, 68.5, 69.2)
x=c(78, 69, 77, 88, 67, 80, 74, 94, 102)


# symboliczne oznaczenie modelu lm(y~x) (regresja jednokrotna) 
# lm(y~x) zawiera w sobie estymatory bety, jest to lista

m=lm(y~x) 
m$coefficients # wyestymowane bety (wektor z names-ami)
m$residuals # wektor rezidui (ei)
# rownowaznie ze wzorow
y-m$fit


m$coefficients[1] #beta zero-wyraz wolny
m$coefficients["(Intercept)"]

m$coefficients[2] #beta jeden
m$coefficients["x"]


summary(m)  #szczegolowe dane: (jest to lista)
# rezidua (kwantyle)
# estymatory bety i bledy standardowe estymatorow
# wartosc testu t-studenta = 1kolumna/2 kolumna oraz p-value i obszar krytyczny
# rezidua blad standardowy =sigma z r.normalnego N(0,sigma^2)
# =/=  sd(m$residuals) (bo ten z N to estymator, a ten drugi to dokaldna wartosc) 
# i stopien swobody = ilosc obserwacji - ilosc estymatorow beta
# R^2 (wsp.determinacji) i przyblizony R^2 
# wartosc testu F, stopnie swobody i p-value

summary(m)$sigma 

(summary(m)$sigma)^2 #wariancja
#rownowaznie ze wzorow
deviance(m)/(length(y)-2) 

summary(m)$r.squared
summary(m)$adj.r.squared 
summary(m)$df #stopnie swobody

m$df #stopnie swobody w testach t i F, =n-(p+1)

summary(m)$fstatistic #wartosc statystyki F i stopnie swobody =p, n-(p+1)

summary(m)$coef[,"t value"] # wartosci statystyki t
summary(m)$coef[,"Pr(>|t|)"] # p-values


plot(x,y) #wykres rozrzutu/rozproszenia
abline(m, col="red", lwd=2) #dorysowanie linii do danego wykresu juz istniejacego

predict(m, newdata=list(x=90)) #pprognozowana wartosc y dla x=90
#rownowazne 
m$coef[1]+m$coef[2]*90

deviance(m) #RSS suma (ei)^2

#dopasowane wartosci Y w czapce, cyzli bez rezidui, tylko na podstawie wzoru regresji i danych x
m$fitted.values

###Zad. 11
# kilka X (zmiennych objasniajacyh)

library(MASS)
data(hills)
dist=hills$dist
time=hills$time
climb=hills$climb

m=lm(time~dist+climb) # nowe oznaczenie dla wielokrotnej regresji 
# w m beda nazwy takie jak dalismy cyzli dist,climb
# trzyma oszacowanie bety, b0,b1,b2

m$coefficients
(summary(m)$sigma)^2
predict(m, newdata = list(dist=10, climb=2500)) #szacowanie

summary(m)
summary(m)$df #ilosc x+1=p+1, n-(p+1), ilosc y+1

#####ZESTAW 2

#Zad.1

data("anscombe")

x1 =anscombe$x1
x2 =anscombe$x2
x3 =anscombe$x3
x4 =anscombe$x4
y1 =anscombe$y1
y2 =anscombe$y2
y3 =anscombe$y3
y4 =anscombe$y4

par(mfrow=c(2,2)) # 4 wykresy na 1 rysunku
m1=lm(y1~x1)
plot(x1,y1,xlab="xiki",ylab="yeki", las=2) #zmienia nazwy osi, las odwraca wartosci na osi y-kow
abline(m1, col="red", lwd=2)

m2=lm(y2~x2)
plot(x2,y2)
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

#wspolczynnik korelacji
cor(x1,y1)
sqrt(summary(m1)$r.squared) #rownowaznie ze wzoru (ale tutaj nie zostanie zachowany znak)

m1$coef[2] #wspolczynnik nachylenia prostej

summary(m1)$coef[3] #jego blad standardowy (z macierzy)

par(mfrow=c(2,2))
e1=m1$residuals
plot(e1)
plot(x1,e1)
plot(y1,e1)
plot(m1$fitted.values,e1,xlab="y dopasowane",ylab="rezidua")


e1=m1$residuals
e2=m2$residuals
e3=m3$residuals
e4=m4$residuals
####### Wykresy normalnosci
#test shapiro wilka
shapiro.test(e1)
shapiro.test(e2)
shapiro.test(e3)
shapiro.test(e4)

#wykresy quantylowe

par(mfrow=c(2,2))
qqnorm(e1)
qqline(e1)

qqnorm(e2)
qqline(e2)

qqnorm(e3)
qqline(e3)

qqnorm(e4)
qqline(e4)

#przedzial ufnosci dla beta0,beta1 na poz.ufnosci=-0.95
confint(m1, level=0.95)

#przedzial ufnosci dla prognozowanej wartosci

predict(m1, newdata = list(x1=21))

#dla wartosci sredniej tzw.przedzial ufnosci
predict(m1, newdata = list(x1=21), interval="confidence", level=0.95)
#dla prognozy(przewidywanej wartoisc) tzw.przedzial ufnosci-predykcji
predict(m1, newdata = list(x1=21), interval="prediction",level=0.95)  
#dopasowana wartosc,blad standardowy,blad standardowy rezidui,st.swobody bledu
predict(m1, newdata = list(x1=21), se=T)


###ZESTAW 3

install.packages("scatterplot3d")
library(scatterplot3d)
#potrzebne do wykresow 3d

par(mfrow=c(1,1))
#dla danych z ramki hills wykres z punktami
wykres=scatterplot3d(hills,type="p",highlight.3d=TRUE,angle=55,scale.y=0.7,pch=16)
#time=hills$time
#climb=hills$climb
#dist=hills$dist

#mozna tak ale i nie deklarujac zmiennych napisac nazwe ramki danych
m=lm(time~dist+climb,hills)

#dopasowana plaszczyzna metdoa regresji
wykres$plane3d(m,lty.box="solid")

summary(m)
avPlots(m) #wykres czesciowej regreji

#ten sam wykres robiony na pieszo
par(mfrow=c(1,1))
r1=lm(dist~climb,hills)$res
r2=lm(time~hills$climb,hills)$res
r3=lm(r2~r1)
plot(r1,r2)
abline(r3,col="red")
summary(r3)


crPlots(m) #component+residaul plots xi od xi*bi+e

###testowanie hipotez

m0=lm(time~1,hills) #zaleznosc od stalej, tylko b0 jest rozna od zera
m1=lm(time~dist+climb,hills) #caly model
anova(m0,m1) #anova(model_mniejszy,model_wiekszy)

m0=lm(time~dist+climb-1) #stala = 0
anova(m0,m1)

m0=lm(time~I(dist+climb),hills) #b1=b2
anova(m0,m1)

m0=lm(time~I(2*dist+climb),hills) #b1=2*b2
anova(m0,m1)

m0=lm(time~dist+offset(climb),hills) #offset przewisuje wartosci bi
#b2=1
anova(m0,m1)

m0=lm(time~dist+offset(5*climb),hills) #b2=5
anova(m0,m1)

#stopnie swobody
summary(m1)
summary(m1)$fstatistic
summary(m1)$df #np. 3,32,3
m1$df # np.32
nrow(hills)-m1$df #np.3

a=anova(m0,m1)
a$Df 
m0$df-m1$df #rownowaznie, roznica stopni swobody modelu mniejszego i wiekszego

#### zadanie z lambdami

dane = read.table("http://mini.pw.edu.pl/~bobecka/bakterie.txt",header=TRUE)
x = dane$t
y = dane$N_t


plot(x,y)
m1 = lm(y~x)
abline(m1,col="red")
summary(m1)

#analiza reszt
r1 = m1$residuals
#wykres reszt z poziomym h=0
plot(r1)
abline(h=0)
qqnorm(r1)
qqline(r1)

#model nie pasuje, widac po wykresach, mimo ze diagnostyka (summary) daje co innego
#UWAGA wykresy>>>>>summary

# przyblizanie f.wykladnicza
z = log(y)
#jak wyglada przy splaszczeniu na liniowa funckje
plot(x,z)
m2=lm(z~x)
abline(m2,col="red")

#porownanie
plot(x,y)
lines(x,m1$fitted.values,col="red")
#f.wykladnicza na wykresie
lines(x,exp(m2$fitted.values),col="green") #y=exp(z)
summary(m2)

#prognoza
#dla modelu 1
predict(m1,newdata=list(x=20))
#dla modelu 2
a = predict(m2,newdata = list(x=20)) #prognoza dla z
exp(a) #bo y=exp(z)

#model odwrotnoœciowy wzgl x
z = 1/x
plot(z,y)
m = lm(y~z)
abline(m,col="red")
#wykres odwrotnosciowy
plot(x,y)
lines(x,m$fitted.values,col="green")
#prognoza dla modelu odwr. wzgl. x
predict(m,newdata=list(z=1/100)) #bo x=1/z

#model odwrotnosciowy wzgl y 
z = 1/y
plot(x,z)
m = lm(z~x)
abline(m,col="red")

# f.odwrotnosciowa na wykresie
plot(x,y)
lines(x,(m$fitted.values)^(-1),col="green")

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

plot(x,y)
lines(x,exp(m$fitted.values),col="green")


#pierwiastek

z1 = sqrt(y)
plot(x,z1)
m2 = lm(z1~x)
abline(m2,col="red")

plot(x,z1^2) #czyli plot(x,y)
lines(x,(m2$fitted.values)^2,col="green")



#lambda=1/4
z3 = y^(1/4)
plot(x,z3)
m4 = lm(z3~x)
abline(m4,col="red")
summary(m4)

plot(x,y) 
lines(x,(m4$fitted.values)^4,col="green")

# box-cox

#box-cox dla modelu m1 bo szukamy lambdy
b = boxcox(m1,plotit = T) #m1-model podstawowy
b = boxcox(y~x,plotit = T)

#dodatkowo szukamy dla jakiego x jest max y
which(b$y==max(b$y))

b$x[which(b$y==max(b$y))]

lambda=b$x[which(b$y==max(b$y))]
#wykres dla najlpeszego dopasowania wyznaczonego metoda boxa-coxa
z=y^lambda
plot(x,z)
m = lm(z~x)
abline(m,col="red")
summary(m)

plot(x,y) 
lines(x,(m$fitted.values)^(1/lambda),col="green")



#patrzymy z bliska na wykres 
boxcox(m4,lambda = seq(0.0,0.4,len = 20))

#y=a+bx+cx^2
x1 = x
x2 = x^2
mod4 = lm(y~x1+x2)
summary(mod4)
plot(x,y)
lines(x,mod4$fitted.values,col="green")

