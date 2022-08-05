##worksheet 3

##problem 1 (part 1)

sum(rbinom(1000,1,0.5))/1000  ##proportion of heads

##problem 1 (part 2)

sum(rbinom(1000,1,0.3))/1000 


##problem 2 (part a)

ball.col=c("red","green","blue")
ball.identification.number=rep(1:3,time=c(3,2,2))
ball.col[sample(ball.identification.number,size = 1,rep(c(3/7,2/7,2/7),time=c(3,2,2)),replace=F)]


##problem 2 (part b)
A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)
A

A1 <- matrix(A[,1],nrow=1)
A2 <- matrix(A[,2],nrow=1)
A3 <- matrix(A[,3],nrow=1)

is.matrix(as.matrix(A[,2]))

##vector of norm of all the columns
norm.vector=c(norm(A1,type = "F"),norm(A2,type = "F"),norm(A3,type = "F"))


prob <- norm.vector/sum(norm.vector)

##drawing i-th column corresponding to probability p.i
sample(c(1,2,3),size <- 1,prob,replace=F)


##problem 2 (part c)
paste("The dirt is thrown at",runif(n=1,0,5),"cm")


##prblem 3 (part a)
sum <- 0
i <- 0
while(sum <= 1)
  {
  sum <- sum + runif(1,0,1)
  i <- i+1
  }
i


##problem 3 (part b)
output <- array(0)
for(j in 1:1000)
{
sum <- 0
i <- 0
while(sum <= 1)
  {
  sum <- sum + runif(1,0,1)
  i <- i+1
  }
output[j]=i
}

output


##problem 3 (part c)
exp(1) 
sum(output)/1000   ##average of the 1000 outputs which is close to exp(1)
     


##problem 4 (part a)
candle.off <- 0

f <- function(n)
{

  i <- 0
  while(candle.off <= n)
  {
    candle.off <- candle.off + sample(1:n-candle.off,1)
    i <- i+1
  }
  return(i)
}

f(25)








