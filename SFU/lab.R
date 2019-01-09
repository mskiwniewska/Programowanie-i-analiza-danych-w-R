Rcpp::sourceCpp("nazwa.cpp")

one()


dodaj(c(1,2),c(1,2,3))

hilbert(3)


##################Plik C


#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
int one(){
  return 1;
}

// [[Rcpp::export]]
NumericVector dodaj(NumericVector x, NumericVector y){
  
  int nx = x.size();
  int ny = y.size();
  int n;
  
  if(nx < ny) n=ny;
  else n=nx;
  
  NumericVector z(n);
  
  for(int i=0;i<n;++i)
    z[i]=x[i%nx]+y[i%ny]; 
  
  return z;
  
}


// [[Rcpp::export]]
NumericMatrix hilbert(int n){
  
  NumericMatrix X(n,n);
  
  
  for (int i=0; i<n; ++i)
    for (int j=0; j<n; ++j)
      X(i,j)=1.0/(i+j+1); 
    
    
    
    return X;
    
    
}


// [[Rcpp::export]]

NumericVector movingavg(NumericVector x, int k){
  
  int n = x.size();
  
  
  
}


