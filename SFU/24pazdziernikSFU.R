install.packages("scatterplot3d")
install.packages("car")
library(scatterplot3d)
library(car)

par(mfrow=c(1,1))
wykres=scatterplot3d(hills,type="p",highlight.3d=TRUE,angle=55,scale.y=0.7,pch=16)
#time=hills$time
#climb=hills$climb
#dist=hills$dist
m=lm(time~dist+climb,hills)
wykres$plane3d(m,lty.box="solid")

summary(m)

avPlots(m) #wykres czesciowej regreji

par(mfrow=c(1,1))
r1=lm(dist~climb,hills)$res
r2=lm(time~hills$climb,hills)$res
r3=lm(r2~r1)
plot(r1,r2)
abline(r3,col="red")
summary(r3)


crPlots(m) #component+residaul plots

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

####stopnie swobody

summary(m1)
summary(m1)$fstatistic
summary(m1)$df
m1$df
nrow(hills)-m1$df

a=anova(m0,m1)
a$Df
m0$df-m1$df




m0=lm(time~I(climb+dist),hills)
anova(m0,m1)

m0=lm(time~dist+offset(climb),hills)
anova(m0,m1)

m0=lm(time~dist+offset(5*climb),hills)
anova(m0,m1)

m0=lm(time~offset(0*dist)+climb,hills)
anova(m0,m1)

m0=lm(time~dist+offset(2*dist),hills)
anova(m0,m1)

m0=lm(time~I(2*climb+dist),hills)
anova(m0,m1)