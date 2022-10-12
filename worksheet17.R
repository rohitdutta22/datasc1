num1 <- numeric(length = 1e3)
object.size(num1)

num2 <- numeric(length = 1e6)
mat1 <- matrix(runif(100*1000), nrow = 100, ncol = 1000)
mat2 <- matrix(0, nrow = 100, ncol = 1000)
arr <- array(0, dim = c(100,100,100))

object.size(num1)


sam <- numeric(length = 1e4)
for(i in 1:1e4){
  sam[i] <- rnorm(1)
}




## problem 2 ##

library(profvis)
library(rbenchmark)

profvis({
  sam <- numeric(length = 1e4)
  for(i in 1:1e4){
    sam[i] <- rnorm(1)
  }
})

profvis({
  sam <- rnorm(1e4)
})


benchmark({
  sam <- numeric(length = 1e4)
  for(i in 1:1e4){
    sam[i] <- rnorm(1)
  }},
  sam <- rnorm(1e4), replications = 100)

## problem 3 ##
benchmark({
 sam <- rnorm(1e4)},
  sam <- runif(1e4), replications = 100)

## problem 4 ##
benchmark({rho_mat_1 <- function(n, rho){
  mat <- matrix(0, nrow = n, ncol = n)
  for(i in 1:n){
    for(j in 1:n){
      mat[i,j] <- rho^(abs(i - j)) 
    }
  }
  return(mat)
}
rho_mat_1(100,2)},
{rho_mat_2 <- function(n, rho){
  mat <- matrix(rho, nrow = n, ncol = n)
  mat <- mat^(abs(col(mat) - row(mat)))
  return(mat)
}
rho_mat_2(100,2)},replications = 10)

factorial(4)
exp(1)
pi  


log_sum <- function(n){
  s <- sum(log(1:n))
  return(s)
}
log_sum(500)
stirling <- function(n){
  lim <- exp(log_sum(n) - ((n*log(n)) - n + (0.5*log(2*pi*n))))
  return(lim)
}

x <- 10:1e3
fn <- c()
for (i in 1:length(x)) {
  fn[i] <- stirling(x[i])
}

plot(x, fn, col = 2, pch = 19, cex = 0.5)


