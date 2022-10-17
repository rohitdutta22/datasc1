#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector funcC(NumericVector x){
  
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
  
}



// [[Rcpp::export]]
NumericMatrix addmat(NumericMatrix x, NumericMatrix y){

  double m  = x.nrow();
  double n = x.ncol();
  NumericMatrix sum_mat(m , n);
  for(int i = 0; i < m; i++){
    for(int j = 0; j < n; j++){
      sum_mat(i,j) = x(i,j) + y(i,j);
    }
  }
  return sum_mat;
}

// [[Rcpp::export]]
int test(NumericMatrix x, NumericMatrix y){
  
  int m  = x.ncol();
  
  return m;
}


// [[Rcpp::export]]
NumericVector column_sums(NumericMatrix x){
  
  double m  = x.nrow();
  double n = x.ncol();
  NumericVector col_sums(n);
  for(int j = 0; j < n; j++){
    
     double sum = 0;
    
     for(int i = 0; i < m; i++){
       
       sum = sum + x(i,j);
     }
    col_sums[j] = sum;
  }
  return col_sums;
}




// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

