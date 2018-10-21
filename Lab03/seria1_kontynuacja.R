# Seria 1 kontynuacja na kolejnych labach

### Zad.1.4.

srednia_wins <- function(x,k){
  
  stopifnot(is.numeric(x),length(x)%%2==1)
  stopifnot(k<=((length(x))-1)/2,k>=0,k-trunc(k)==0)
  
  x <- sort(x)
  x[1:k] <- x[k+1]
  x[length(x):(length(x)-k+1)] <- x[length(x)-k]
  
  mean(x)
  
}


## Zad.1.5.

gini <- function(y){
  
  stopifnot(sum(y>=0)==length(y),is.numeric(y))
  
  k <- 1:length(y)
  sum(y*(2*k-length(y)-1))/((length(y))*(length(y)-1)*mean(y))
}

y <- rpois(1000,3)
print(gini(y))

y <- rnorm(1000,2,0.5)
print(gini(y))

y <- rchisq(1000,5,0)
print(gini(y))

## Zad.1.6.

moda <-function(x){
  
  stopifnot(is.numeric(x),x-trunc(x)==0)
  
  y <- numeric(length(rle(x)$values))
               
  for(i in 1:length(rle(x)$values)) y[i]=sum(rle(x)$length[which(rle(x)$values==rle(x)$values[i])])
  
  # print(y)
  
  rle(x)$values[which.max(y)]
  
}

## Zad.1.7.


calkaMonteCarlo <- function(f,n=1000,a,b){
  
  stopifnot(is.function(f),f(a)>=0,f(b)>=0)
  
  fmin=min(f(a),f(b))
  fmax=max(f(a),f(b))
  
  x1=runif(n,min=a,max=b)
  x2=runif(n,min=fmin,max=fmax)
  
  frakcja <- sum(x2<=f(x1))/length(x2)
  
  frakcja*(b-a)*(fmax-fmin)+(b-a)*fmin
  
  
}

## Zad.1.15.


dystr_emp <- function(x,y){
  
  stopifnot(is.numeric(x),is.numeric(y))
  x <- sort(x)
  findInterval(y,x)/length(x)
  
  
}



## Zad.1.20


approxinvert <- function(f,y,a,b,k=100){
  
  stopifnot(is.function(f),length(a)==1,length(b)==1,b>a,is.double(a),is.double(b),k>2,k-trunc(k)==0)
  stopifnot(sum(y>=f(a) & sum(y<=f(b)))==length(y))
  
  x <- seq(from=a,to=b,length.out=k)

  a <- approx(x,f(x),n=k)
  
  przyblizenie <- numeric(length(y))
  
  for(i in 1:length(y)) przyblizenie[i]=a$x[a$y-y[i]==min(abs(a$y-y[i])) | a$y-y[i]== -min(abs(a$y-y[i]))]
  
  
  przyblizenie
  
}


## Zad.1.24.

gendyskr <- function(n,x,p){
  
  stopifnot(is.numeric(x),n>=0,n-trunc(n)==0)
  
  for(i in 1:length(x)) y[i]=length(x[x==x[i]])
  
  stopifnot(any(y==1))
  
  
  if(sum(p)!=1){
    
    print("Podales nieunormowany wektor wiec go sam znormalizowalem")
    
    p <- p/sqrt(as.vector(crossprod(p)))
  }
  
  probka <- numeric(n)
  
  for(i in 1:n){
  u <-runif(1)
  
  m <- findInterval(u,cumsum(p))+1
  
  probka[i] <- x[m]
  
  }
  
  probka
}


















