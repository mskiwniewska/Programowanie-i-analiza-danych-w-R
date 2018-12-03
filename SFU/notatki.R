#######biblioteki
install.packages("scatterplot3d")
library(scatterplot3d)
library(MASS)
library(car) #avplots
library(mvinfluence)
library(leaps)
library(nlme)
library(lmtest)

######Lista 1

m <- lm(y~x) 
summary(m)

m$coefficients # wyestymowane bety (wektor z names-ami)

m$coefficients[1] #beta zero-wyraz wolny
m$coefficients["(Intercept)"]

m$coefficients[2] #beta jeden
m$coefficients["x"]

m$residuals # wektor rezidui (ei)
y-m$fit # rownowaznie ze wzorow

#dopasowane wartosci Y w czapce, cyzli bez rezidui, tylko na podstawie wzoru regresji i danych x
m$fitted.values

m$df #stopnie swobody w testach t i F, =n-(p+1)

m$model


summary(m)$sigma 
(summary(m)$sigma)^2 #wariancja
deviance(m)/(length(y)-2) #rownowaznie ze wzorow
deviance(m) #RSS suma (ei)^2

summary(m)$r.squared
summary(m)$adj.r.squared 

summary(m)$df #stopnie swobody
m$df #stopnie swobody w testach t i F, =n-(p+1)

summary(m)$fstatistic #wartosc statystyki F i stopnie swobody =p, n-(p+1)
summary(m)$fstatistic["value"] 
summary(m)$coef[,"t value"] # wartosci statystyki t
summary(m)$coef[,"Pr(>|t|)"] # p-values
summary(m1)$coef[,2,drop=FALSE] #bledy std. dla bet

plot(x,y) #wykres rozrzutu/rozproszenia
abline(m, col="red", lwd=2) #dorysowanie linii do danego wykresu juz istniejacego

predict(m, newdata=list(x=90)) #pprognozowana wartosc y dla x=90
m$coef[1]+m$coef[2]*90 #rownowazne 


## kilka X (zmiennych objasniajacyh)

library(MASS)
data(hills)
dist=hills$dist
time=hills$time
climb=hills$climb

m=lm(time~dist+climb)

predict(m, newdata = list(dist=10, climb=2500))


#####Lista 2

dane = read.table("http://mini.pw.edu.pl/~bobecka/rzeki.txt",header=TRUE)
y=dane$Nitrogen
x1=dane$Agr
x2=dane$Forest


par(mfrow=c(2,2))

plot(x1,y1,xlab="xiki",ylab="yeki", las=2)

cor(x1,y1) #wspolczynnik korelacji
sqrt(summary(m1)$r.squared) #rownowaznie ze wzoru (ale tutaj nie zostanie zachowany znak)

m1$coef[2] #wspolczynnik nachylenia prostej
summary(m1)$coef[4] #jego blad standardowy (z macierzy)
summary(m1)$coef[2,2]

e1=m1$residuals
plot(e1)
plot(x1,e1)
plot(y1,e1)
plot(m1$fitted.values,e1,xlab="y dopasowane",ylab="rezidua")

## Wykresy normalnosci
#test shapiro wilka
shapiro.test(e1)
#wykresy quantylowe
qqnorm(e1)
qqline(e1)

#przedzial ufnosci dla beta0,beta1 na poz.ufnosci=-0.95
confint(m1, level=0.95)
confint(m1, level=0.95)[1,] #prz.ufnosci dla beta0


#przedzial ufnosci dla prognozowanej wartosci
#dla wartosci sredniej tzw.przedzial ufnosci
predict(m1, newdata = list(x1=21), interval="confidence", level=0.95)
#dla prognozy(przewidywanej wartoisc) tzw.przedzial ufnosci-predykcji
predict(m1, newdata = list(x1=21), interval="prediction",level=0.95)  
#dopasowana wartosc,blad standardowy,blad standardowy rezidui,st.swobody bledu
predict(m1, newdata = list(x1=21), se=TRUE)

unique(predict(m, newdata = list(x=x+1), interval="confidence", level=0.95))
predict(m, newdata = list(x=0), interval="confidence", level=0.95)

## diagnostyka dopasowanego modelu

#wykres rozproszenia z naniesionym wykresem dopasowanej prostej MNK

#wartosc wsp.determinacji (analiza)

#testy itotnosci parametrow

#rozklad zmiennych resztowych

#####Lista 3

install.packages("scatterplot3d")
library(scatterplot3d)

wykres=scatterplot3d(hills,type="p",highlight.3d=TRUE,angle=55,scale.y=0.7,pch=16)

#time=hills$time
#climb=hills$climb
#dist=hills$dist
#mozna tak ale i nie deklarujac zmiennych napisac nazwe ramki danych
m=lm(time~dist+climb,hills)

#dopasowana plaszczyzna metdoa regresji
wykres$plane3d(m,lty.box="solid")

avPlots(m) #wykres czesciowej regreji
crPlots(m) #component+residaul plots xi od xi*bi+e

#avPlots robiony na pieszo
par(mfrow=c(1,1))
r1=lm(dist~climb,hills)$res
r2=lm(time~hills$climb,hills)$res
r3=lm(r2~r1)
plot(r1,r2)
abline(r3,col="red")
summary(r3)

##zadania z lamdami

#analiza reszt
r1 = m1$residuals
#wykres reszt z poziomym h=0
plot(r1)
abline(h=0)


# przyblizanie f.wykladnicza exp
z = log(y)
#jak wyglada przy splaszczeniu na liniowa funckje
plot(x,z)
m2=lm(z~x)
abline(m2,col="red")


#porownanie
plot(x,y)
lines(x,m1$fitted.values,col="red")
#f.wykladnicza na wykresie
lines(x,exp(m2$fitted.values),col="blue") #y=exp(z)

#prognoza
#dla modelu 1
predict(m1,newdata=list(x=20))
#dla modelu 2
a = predict(m2,newdata = list(x=20)) #prognoza dla z
exp(a) #bo y=exp(z)

#model odwrotnoœciowy wzgl x (model hiperboliczny)
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
plot(x,y)
lines(x,(m$fitted.values)^(-1),col="green") #f.odwrotnosciowa na wykresie

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

##box-cox

b = boxcox(m1,plotit = T) #m1-model podstawowy
b = boxcox(y~x,plotit = T)

#dodatkowo szukamy dla jakiego x jest max y
lambda=b$x[which(b$y==max(b$y))]
lambda=b$x[which.max(b$y)] #rownowaznie

#wykres dla najlpeszego dopasowania wyznaczonego metoda boxa-coxa
z=y^lambda
plot(x,z)
m = lm(z~x)
abline(m,col="red")
plot(x,y) 
lines(x,(m$fitted.values)^(1/lambda),col="green")

#patrzymy z bliska na wykres 
boxcox(m1,lambda = seq(0.0,0.4,len = 20))
boxcox(m1,lambda = seq(-0.1,0.1,len = 20))
#y=a+bx+cx^2, model wielomianowy, a model kwadratowy to lepszy ten z lambda=1/2
x1 = x
x2 = x^2
mod4 = lm(y~x1+x2)
summary(mod4)
plot(x,y)
lines(x,mod4$fitted.values,col="green")

#model potegowy
a <- log(y)
b <- log(x)
model <- lm(a~b)
plot(x,y)
lines(x,exp(model$fitted.values),col="red")

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

m0=lm(time~dist+offset(climb),hills) #offset przepisuje wartosci bi #b2=1
anova(m0,m1)

m0=lm(time~dist+offset(5*climb),hills) #b2=5
anova(m0,m1)

m0=lm(y~x1+offset(0*x2)+offset(0*x3)) #b2=b3=0
m1=lm(y~x1+x2+x3)
m0=lm(y~I(2*x1+x3)+x2) #b1=2*b

#st.swobody
nrow(hills)-m1$df #np.3 (p+1)
a=anova(m0,m1)
a$Df 
m0$df-m1$df #rownowaznie, roznica stopni swobody modelu mniejszego i wiekszego

#statystyka i p-value
anova(m0,m1)$F[2]
anova(m0,m1)$`Pr(>F)`[2]


##usuwanie danych, obserwacje odstajace i wplywowe

model1 = lm(Nitrogen~.-River,data=dane)

#rozne zmienne resztowe

r1 = model1$residuals
plot(r1)
abline(h=0)

#studentyzowane zmienne resztowe

r2 = rstandard(model1)
plot(r2)
abline(h=0)

#studentyzowane zmienne resztowe modyfikowane

r3 = rstudent(model1)
plot(r3)
abline(h=0)

#usun wiersz z Neversink
danenowe=subset(dane,River!="Neversink")
model2 = lm(Nitrogen~.-River,data=danenowe)

model2_1=update(model1,subset=-c(4)) #rownowaznie (4 wiersz to Neversink)


danenowe2=subset(dane,River!="Neversink" & River!="Hackensack") #usuwanie 2 rzeczy od razu

#spojrz na reszty i R^2

y=dane$Nitrogen
x3=dane$ComIndl
plot(x3,y)
m1=lm(y~x3)
abline(m1)

y2=danenowe$Nitrogen
x3_2=danenowe$ComIndl
m2=lm(y2~x3_2)
abline(m2,col="red")

y3=danenowe2$Nitrogen
x3_3=danenowe2$ComIndl
m3=lm(y3~x3_3)
abline(m3,col="green")

m4=update(m1,subset=-c(8))
abline(m4,col="orange")

outlierTest(model1) #obserwacje odstajace,oddalone,outlier
#maja wplyw na wspolczynnik kierunkowy prostej, duza abs(ei)
#obserwacje odstajace moga ale nie musza byc obserwacjami wplywowymi
#poprawka Bonferoniego
#raportuje Bonferoni p-value dla najbardziej ekstremalnych obserwacji
#Bonferoni p/ p-value = ilosc obserwacji
#sa tu tez rstudent dla odstajacych
hi=hatvalues(model1) #wplywy
hi
lm.influence(model1)$hat #rownowaznie

n=nrow(dane)
p=n-model1$df.residual
hi[hi>2*p/n] #potencjalnie wplywowe, 3*p/n dla duzego modelu
#obserwacje wysokiej dŸwigni leverage
#hi z [0,1]

influence.measures(model1) #wplywowe to te z *, influential

di=cooks.distance(model1)
di[di>1] #wplywowe w terminach odleglosci Cook'a
#>1 to wysoka wplywowosc

plot(model1,which=4) #wykres z odl.Cooke'a tzw.diagram Cook'a
influencePlot(model1, main="Influence Plot")
#rozmiar kolka jest proporcjonalny do odleglosci Cook'a

dif=dffits(model1) #miara DFFITS
dif[abs(dif)>2*sqrt(p/n)] #wplywowe wg DFFITS

plot(dif,ylim=c(-30,30)) #[-30,30 ] bo max(dif)=1.26,min(dif)=-28.58
abline(h=2*sqrt(p/n))
abline(h=-2*sqrt(p/n))

rr=rstandard(model1)
rr[abs(rr)>3] #odstajace, patrzac na wartosci rstandard


####zmniejszanie danych AIC,Cp Mallows, skorygowany wsp.determinacji

dane = read.table("http://mini.pw.edu.pl/~bobecka/diab.txt",header=TRUE)
m1=lm(Y~.,dane)
avPlots(m1) #10 wykresow
crPlots(m1) #10 wykresow

adjR2=summary(m1)$adj.r.squared

#algorytm leaps
#kryterium max skorygowanego wsp.determinacji
X=model.matrix(m1)[,-1] #wszystkie X bez Y i bez Intercept w postaci macierzy
rsk=leaps(X,dane$Y,method = "adjr2",nbest=3)
rsk
najlepszy1=rsk$which[which(rsk$adjr2==max(rsk$adjr2)),]
names(dane)[najlepszy1==FALSE] #ktore usunac
names(dane)[najlepszy1==TRUE] #zostawiam

#kryterium minimum wsp. Cp Mallows'a
rsk2=leaps(X,dane$Y,method = "Cp",nbest=3)
rsk2
najlepszy2=rsk2$which[which(rsk2$Cp==min(rsk2$Cp)),]
najlepszy2
names(dane)[najlepszy2==FALSE] #odrzucam

#kryterium informacyjne Akaikego (AIC)

extractAIC(m1) #wartosc AIC dla modelu
stepAIC(m1) #wybor odpowiednich zmiennych wg AIC

#regresja krokowa

step(m1,direction = "both")
#usuwanie+dodawanie, do modelu tylko z wyrazem wolnym dodajemy zmienna istotna
#z min p-value, a potem usuwamy zmienna nieistotna z max p-value
#powtarzamy kroki az model przestanie sie zmieniac
step(m1,direction = "backward")
#backward - usuwanie najmniej istotnych zmiennych z pe³nego modelu (ze wszystkimi zmiennymi objasniajacymi)
#dopoki wszystkie zmienne w modelu beda istotne
step(m1,direction = "forward")
#dodawanie najbardziej istotnych zmiennych do modelu, ktory zawiera tylko wyraz wolny
#funckja step domyslnie wykorzystuje AIC (parametr k=2)
#BIC dla k=log(n), gdzie n <- nrow(dane) (libcza obserwacji)

#isostnosc zmiennych okreslana jest na podstawie kryterium Akaike (AIC)
#lub kryterium Schwartza (BIC)
#jest rowniez parametr steps, ktory okresla max ilosc krokow
n <- nrow(dane)
step(m1,direction = "both",k=log(n)) #BIC

#krokowa
summary(regsubsets(Y~.,data=dane,method = "forward")) 
#mozna dodac jeszcze paranetr nvmax ktory okresla ilosc zmiennych, ktore pozostawiamy
summary(regsubsets(Y~.,data=dane,method = "forward",nvmax=4))


#krokowa na pieszo

#dodawanie, forward
m0=lm(Y~1,data=dane) #zaczynamy od modelu tylko ze stala
add1(m0,m1,test="F")
mm1=update(m0,.~.+BMI) #dodaje BMI (bo ma max testu F)
add1(mm1,m1,test="F")
mm2=update(mm1,.~.+S5)
add1(mm2,m1,test="F")
mm3=update(mm2,.~.+BP)
add1(mm3,m1,test="F")

#usuwanie, backward
m1=lm(Y~.,dane) #zaczynam od pelnego modelu
drop1(m1,test="F") 
m2=update(m1,.~.-AGE) #usuwam AGE (bo ma min  testu F)
drop1(m2,test="F")
m3=update(m2,.~.-S3)
drop1(m3,test="F")

###############Lista 4


library(MASS)
dane2=phones
dane2 <- as.data.frame(dane2) #lista jako ramka danych
y=dane2$calls
x=dane2$year
model=lm(y~x)

rr <- rstandard(model)
rr[abs(rr)>2]

#metoda regresji odpornej
#lm - korzysta z MNK, wrazliwa na outliers
#rlm - korzysta z M-estymatorow, nie jest wrazliwa na outliers
odporne=rlm(y~x,maxit=50) #maxit-ograniczenie liczby iteracji
abline(odporne,col="green")

#regresja odporna - metoda Hubera,Hampela,Bisquare

plot(x,y)
m0=lm(y~x)
summary(m0)
abline(m0,col="red")

#domyslnie- Huber
mhu=rlm(y~x,maxit=50)
abline(mhu,col="green")

#Hampel
mha=rlm(y~x,maxit=50,psi=psi.hampel)
abline(mha,col="blue")

#Bisquare
mt=rlm(y~x,maxit=50,psi=psi.bisquare)
abline(mt,col="orange")

#wykres funkcji q*psi na [-6,6]
curve(psi.huber(x)*x,from = -6,to=6) #Huber
curve(psi.hampel(x)*x,from = -6,to=6) #Hampel
curve(psi.bisquare(x)*x,from = -6,to=6) #Bisquare

###wspolliniowosc regresorow
dane = read.table("http://mini.pw.edu.pl/~bobecka/schools.txt",header=TRUE)
y=dane$ACHV
x1=dane$FAM
x2=dane$PEER
x3=dane$SCHOOL
model1 = lm(y~x1+x2+x3)


pairs(dane) #wykresy przedstawiajace zaleznosc pomiedzy wszystkimi zmiennymi w parach

vif(model1) #Variance Inflation Factor
#wspolczynnik podbicia (inflacji) wariancji
#zawsze VIF>=1, ale moze ->nsk
#gdy VIF>10 to silna wspolliniowsc, aby sie jej pozbyc usuwamy cechy ktore sa 
#liniowa kombinacja innych zmiennych niezaleznych
#zmienne objasniajace wysoko skorelowane psuja wyniki analizy regresji
#VIF>5 to umiarkowana wspolliniowosc


#obliczenie wskaznika kappa uwarunkowania macierzy t(Z)*Z 
#Z-przeskalowana macierz regresji X
X=cbind(x1,x2,x3) #macierz regresji X
K=cor(X) #macierz korelacji miedzy x1,x2,x3 (macierz symetryczna z 1 na przekatnej)
v=eigen(K)$values #wartosci wlasne macierzy K
kappa = sqrt(max(v)/min(v)) #wyznaczenie wskaznika kappa
kappa #wysoka wartosc dla wysokiej wspolliniowosci
#tzw parametr CN-Condition Number


#regresja grzbietowa
mg=lm.ridge(y~x1+x2+x3)
mg #wynik z lambda=0
#lambda to tzw parametr kary
#lambda=0 to estymatory bety to parametry wyznaczone metoda MNK
#lamda -> nsk, to model regresji zlozony jedynie z wyrazu wolnego
select(mg) #pokazuje estymatory kHKB,L-W,GCV
mg$GCV #estymator uogolnionej walidacji krzyzowej
mg$kHKB #estymator Hoerla i Kennarda
mg$kLW #estymator Lawlessa i Wanga

plot(lm.ridge(y~x1+x2+x3,lambda=seq(0,1,0.001))) #wartosci estymatorow bety dla 
#lambdy z przedzialu [0,1]

mg2=lm.ridge(y~x1+x2+x3,lambda=mg$kHKB) #estymatory bety wyznaczone dla
#lambda = estymator Hoerla i Kennarda
mg2
abline(v=mg$kHKB,col="magenta") #narysowanie khkB na wykresie z lambdami

####autokorelacja bledow

#(i,e_i)
r = m$residuals
plot(r,type="b") #type="b" to punkty + laczace je linie

#(e_i,e_(i-1))
#ogolny wzor Lag_k(Y_i)=Y_(i-k)
#domyslnie lag.plot ma k=1 (parametr lags=1) czyli opoznienie o 1
lag.plot(r,do.lines=F) #do.lines=F czyli nie lacze pkt liniami
#czy jest tendencja, zaleznosc, czy zbior danych jest losowy
#jezeli da sie zidentyfikowac strukture wykresu to dane nie sa przypadkowe
#lag.plot to wykres rozrzutu dla wartosci opoznionych
#bada czy bledy sa niezalezne, przypadkowe, bez wzorca

#czy testy potwierdza nasze podejrzenia?
#test Durbina-Watsona (DW)

durbinWatsonTest(m)
#lag 1 - opoznienie o 1 ; autocorr. = rho z daszkiem ; dwstatis. = T wartosc statystyki
#p-v = 0 < 0,05 czyli odrzucamy H_0 na rzecz H_1 tzn epsilon_1,...,epsilon_n sa AR(1)
#DW=2 to brak autokorelacji
#DW<2 to autokorelacja dodatnia
#DW>2 to autokorelacja ujemna
#ogolnie statystyka DW nalezy  do [0,4]
#AR(1) = autokorelacja I rzedu


#model regresji prostej z bledami AR(1)

m_AR1=gls(y~x,correlation = corAR1(form=~1),method="ML")
#wyznacza estymatory bety_gls
#ML = metoda najw. wiar.
summary(m_AR1) 
#phi = rho z daszkiem
abline(m_AR1,col="red")

#GLS-Generalized Least Squares (Uogolniona metoda MNK)
#w GLS bledy moga byc skorelowane i/lun miec rozne wariancje

###heteroscedastycznosc (brak stalosci wariancji bledow)

#zalozenia klasycznego Modelu Regresji Liniowej to 
#homoscedastycznosc (stalosc wariancji)

r=m1$residuals
plot(r)
abline(h=0,col='red')

plot(x,r) #wariancja zalezy od X
abline(h=0,col='red')

plot(y,r)
abline(h=0,col='red')


ncvTest(m1) #Non-Constant Variance Score Test
#H_0 wariancja stala
#H_1 wariancja sie zmienia
#p-v male < 0,05 czyli odrzucamy H_0 na rzecz H_1 (czyli wariancje nie sa stale)

bptest(m1) #Breusch Pagan Test

shapiro.test(r)

#przeksztalcenie stabilizujace wariancje zmienncych resztowych

#spojrzmy na wykres Boxa Coxa
boxcox(m1,plotit = T)
#czesto stosowane lambdy: 1/3, 1/2, 0 (logarytm, fajne, silne przeksztalcenie)
#1 => brak transformacji, -1=>odwrotnosc

#lambda=1/2
y2=sqrt(y)
m2=lm(y2~x)
summary(m2)
r2=m2$residuals
plot(r2)
plot(x,r2)
plot(y2,r2)
ncvTest(m2)
bptest(m2)
shapiro.test(r2)

#lambda=0 czyli logarytm
y3=log(y)
m3=lm(y3~x)
summary(m3)
r3=m3$residuals
plot(r3)
plot(x,r3)
plot(y3,r3)
ncvTest(m3)
bptest(m3)
shapiro.test(r3)

#czyli lambda=0 dziala

library(lmtest)
bptest(m1) #0.05>p.v
bptest(m3) #0.5<p.v czyli ok

#lambde sugeruje rowniez wykres
spreadLevelPlot(m1)
#lambda=1-b
#sugeruje lambde
#im bardziej przerywana jest blizsza y=x to lambda=0 jest lepsza

#wazona metoda MNK, Weighted Least Squares
#szczegolny przypadek uogolnionej MNK (GLS)
#nadaje obserwacjom o niskiej wariancji skladnika losowego wieksze wagi
#a tym ktore maja ja zbyt wysoka mniejsze wagi
#Obserwacje o malych wariancjach, a wiec bardziej dokladnie
#dostaja wieksze wagi wobec tego maja wiekszy wplyw na wielkosc uzyskanych oszacowan

#czeste wagi to 1/x lub mocniejsze 1/x^2

#1. probujemy robic wagi gdyby lambda=1/2 bylo ok
#za slaba waga
w1=1/x
m.w=lm(y~x,weights = w1)
summary(m.w)
rmw=m.w$residuals
plot(rmw)
ncvTest(m.w) #nie
bptest(m.w)

#2. lambda=0
#dobra waga
w2=1/x^2
m.w2=lm(y~x,weights = w2)
summary(m.w2)
rmw2=m.w2$residuals
plot(rmw2)
ncvTest(m.w2) #ok
bptest(m.w2)
abline(m.w2,col="red")

#bptest nie dziala bo dla roznych lamb wypisuje te same wartosci testu
#nie uzywamy tak
#ale jakby porownac se to sa mniejsze dla lambda = 0

summary(m1)$sigma #duze
summary(m.w1)$sigma
summary(m.w2)$sigma #male, po ustabilizowaniu wariancji























