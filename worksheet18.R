library(Rcpp)

funcR <- function(vec)
{
  # calculating the requried fraction
  frac <- log(vec)/sum(log(vec))  
  
  # log(vec) gives the log of every element of the given vector
  # sum(log(vec)) gives the sum of the log of the elements of the given vector
  
  return(frac)
}

cppFunction('NumericVector funcC(NumericVector x){
  
  double sum_log = 0;
  int n = x.size();

  for(int i = 0; i < n; i++){
    sum_log = sum_log + log(x[i]);
  }
  NumericVector frac(n);
  for(int j = 0; j < n; j++){
   frac[j] = log(x[j])/sum_log;
  }
  
  return frac;
  
}')

funcR(1:3)
funcC(1:3)

