##worksheet 2

##problem 1

rm(list=ls())
seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat
attach(seat)

roll.beginning22 <- Roll[Roll >= 220000]    ##Roll no. beginning with 22(M.sc. students)
roll.beginning21 <- Roll[Roll >= 210000 & Roll <= 219999]  ##Roll no. beginning with 21(BS-MS)
roll.beginning20 <- Roll[Roll >= 200000 & Roll <= 209999]  ##Roll no. beginning with 20(BS-MS)

##number of Msc. Students
length(roll.beginning22)




##problem 2

rm(list=ls())
cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
cricket
attach(cricket)

## problem 2 (part 1)
set.of.allrounder <- subset(cricket, Batting > 25 & Bowling < 40)
attach(set.of.allrounder)
Team

## problem 2 (part 2)
table(Team)[max(table(Team))]  ##It is New zealand

## problem 2 (part 3)
table(Team)[min(table(Team))]  ##It is Australia


##Problem 3

y <- seq(0,10,2)
x <- seq(0,10,2)

plot(x,y,type="l")  ##plotting y = x


##problem 4
  
f<-function(n)
{
  func<-(1+(1/n))^n
    
  return(func)
}
  
n <- 1:1000
plot(n,f(n),type = "l")
abline(h=exp(1),col = "red")
 



