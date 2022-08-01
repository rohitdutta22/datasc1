## worksheet 1

##problem 1

fact=function(n)
{
  product=1
  for(j in 1:n)
  {
    product=product*(j)
  }
  return(product)
}
fact(4)


##problem 2

e<-function(n)
{
  product<-(1+(1/n))^n
 
  return(product)
}
e(100000000000)

##problem 3

seat<-read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
head(seat,10)
attach(seat)
Seat[which(ï..Roll ==221396)]

##problem 4
getwd()
setwd("D:/IITK Data Science Lab 1/worksheet1")
seat.prb4<-read.csv("seating.csv")
seat.prb4
