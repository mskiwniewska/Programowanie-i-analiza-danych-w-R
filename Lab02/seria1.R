set.seed(123)
x <- round(rnorm(20,0,1),2)

#Zad.1
#1.1

x[(x>=-2 & x<=-1) | (x>=1 & x<=2)]

#1.2

sum(x>=0)
x[x>=0]

#1.3

mean(abs(x))

#1.4

max(abs(x))
x == max(abs(x))
x[x == max(abs(x)) | x == -max(abs(x))]

x[x == min(abs(x)) | x == -min(abs(x))]

#1.5

max(abs(x-2))
x == -max(abs(x-2))
x[x-2 == max(abs(x-2)) | x-2 == -max(abs(x-2))]

x[x-2 == min(abs(x-2)) | x-2 == -min(abs(x-2))]

#1.6

x-trunc(x)

#1.7

min(x)
max(x)

(x-min(x))/(max(x)-min(x))

#1.8

y <- character(length(x))

for(i in 1:length(x)) y[i]=ifelse(x[i]>=0,"nieujemna","ujemna")

y

#1.9

y <- character(length(x))

for(i in 1:length(x)) y[i]=ifelse(x[i] < -1,"ma³y",ifelse(abs(x[i]) <= 1,"œredni","du¿y"))

y

#1.10

y <- numeric(length(x))

for(i in 1:length(x)) y[i]=floor(x[i])+1/2

y


#Zad.2

r <- function(x,y){
  
r <- (1/(length(x)-1))*sum(((x-mean(x))/sd(x))*((y-mean(y))/sd(y)))

return(r)

}
  
par(mfrow=c(2,2))  

x <- rnorm(20,0,1); y <- 10*x+2
print(r(x,y))
plot(x,y,main="Wykres 1")

x <- rnorm(20,0,1); y <- -4*x+1
print(r(x,y))
plot(x,y, main="Wykres 2")

x <- rnorm(2000,0,1); y<-rnorm(2000,5,2)
print(r(x,y))
plot(x,y, main="Wykres 3")

#Zad.3

x <- rnorm(20,0,1); y <- 10*x+2

q <- function(x,y){
  
q <- 1-(6/(length(x)*((length(x))^2-1)))*sum((rank(x)-rank(y))^2)
return(q)
  
}
print(q(x,y))

pwartosc <- function(x,y){
  
  pwartosc <- 1-pt(q(x,y) * sqrt((length(x)-2)/(1-(q(x,y))^2)),df=length(x)-2)
  
  
}

print(pwartosc(x,y))










