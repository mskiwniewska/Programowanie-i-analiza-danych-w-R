wykres=scatterplot3d(hills,type="p",highlight.3d=TRUE,angle=55,scale.y=0.7,pch=16)

m=lm(time~dist+climb,hills)
wykres$plane3d(m,lty.box="solid")

summary(m)

avPlots(m)

crPlots(m)


m0=lm(time~1,hills)
m1=lm(time~dist+climb,hills)

anova(m0,m1)

summary(m1)
summary(m1)$df
nrow(hills)-m1$df
m1$df

summary(m1)$fstatistic


a=anova(m0,m1)
a$Df

m0=lm(y~dist+climb-1)

anova(m0,m1)

m0$df-m1$df

m0=lm(time~I(climb+dist),hills)
anova(m0,m1)

m0=lm(time~climb+offset(dist),hills)
anova(m0,m1)

m0=lm(time~climb+offset(5*dist),hills)
anova(m0,m1)



