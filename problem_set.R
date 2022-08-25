## problem set 

rm(list = ls())

# 1
seq(1,1999,2)

# 2
a1 <- 1
a2 <- 1
fibonacci <- array(0)
fibonacci[1] <- 1
fibonacci[2] <- 1
for (i in 3:500) {
  fibonacci[i] <- a2 + a1
  a1 <- fibonacci[i-1]
  a2 <- fibonacci[i]
}
fibonacci

# 3
func3 <- function()
{
  ifelse(sample(1:6,1,1/6) %% 2 == 0, return(1), return("Odd number has appeared"))
}
func3()

# 4
die.roll <- function()
{
  ifelse(sum(sample(c(1,0), 15, prob = c(0.5,0.5), replace = TRUE)) < 8, return("loss"), return("win"))
}
die.roll()

# 5 
A <- matrix(rep(1,25), nrow = 5, ncol = 5)
A

# 6
B <- diag(1:5)
B

D <- matrix(0, nrow = 5, ncol = 5, byrow = TRUE)
for (i in 1:5){
  for (j in 1:5){
    if(i == j){
      D[i,j] <- i
    }else{
      D[i,j] <- 0
    }
  }
}
D


# 7
Die.mat <- matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    Die.mat[i,j] <- sample(1:6, 1, replace = TRUE, prob = rep(1/6,6))
  }
  
}
Die.mat


# 8
func.mat <- function(n,rho)
{
  if(n <= 0 | (n - floor(n)) > 0){
    return("1st argument must be a positive integer")
  }else{
    M <- matrix(0,nrow = n, ncol = n)
    for(i in 1:n){
      for (j in 1:n) {
        if(i == j){
          M[i,j] <- 1
        }else{
          M[i,j] <- rho
        }
      }
    }
    return(M)
  }

}
func.mat(-5,2)



# 9
func.mat.9 <- function(n,rho)
{
  if(n <= 0 | (n - floor(n)) > 0){
    return("1st argument must be a positive integer")
  }else{
    M <- matrix(0,nrow = n, ncol = n)
    for(i in 1:n){
      for (j in 1:n) {
        M[i,j] <- rho^(abs(i - j))
      }
    }
    return(M)
  }
  
}
H <- func.mat.9(5,2)


# 10
small.mat <- function(n){
  M <- matrix(0,nrow = nrow(n),ncol = 1)
  for (i in 1:ncol(n)) {
    if(i %% 2 != 0){
      M <- cbind(M,n[,i])
    }
  }
  return(M[,-1])
}
small.mat(H)


# 11
d <- c(10,4,6,5)
Dim.4 <- array(0,dim = d)
for (i in 1:d[1]){
  for(j in 1:d[2]){
    for (k in 1:d[3]) {
      for (l in 1:d[4]) {
        Dim.4[i,j,k,l] <- 1 
      }
    }
  }
}
Dim.4



## worksheet based question

# 3
die.even <- function(){
  output <- numeric(1000)
  for (i in 1:1000){
    output[i] <- sample(1:6,1,replace = TRUE,prob = rep(1/6,6))
  }
  return(length(output[which(output %% 2 == 0)]))
} 
die.even()


# 4
set.seed(1234)
prop <- function(){
  random.num <- runif(1000)
  return(length(random.num[which(random.num > 0.1 & random.num < 0.2)])/1000)
}
prop()


# 5
rm(list = ls())
set.seed(1234)
packet.count <- function(){
  a <- array(0)
  i <- 1
  while(length(unique(a)) < 7){
    a[i] <- sample(1:7,1,replace = TRUE,prob = c(0.25,0.20,0.20,0.15,0.10,0.05,0.05))
    i <- i + 1  
  }
  return(i-1)
}
packet.count()

mean(replicate(1000,packet.count()))



# 6
set.seed(1234)
day.count <- function(){
tab <- 100
day <- 0
a <- 1
 while(a != 2){
  a <- sample(c(1,2),1,replace = TRUE,prob = c(((tab - day)/100),day/100))
  day <- day + 1
 }
 return(day)
}
day.count()
mean(replicate(1000,day.count()))


# 7
MontyHall <- function(){
comb <- c("goat","car","goat")
comb.permutation <- sample(comb,3,replace = FALSE,prob = rep(1/3,3))
num <- sample(1:3,1,replace = TRUE,prob = rep(1/3,3))
ifelse(comb.permutation[num] == "goat",return(1),return(0))
}
MontyHall()

mean(replicate(1000,MontyHall()))


## problem 8
rm(list = ls())
library(rvest)
library(stringr)
html <- read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")

movie.names <-  html %>% html_elements(".article_movie_title a") %>% html_text()


Tomato_score <- html %>% html_elements(".article_movie_title .tMeterScore") %>% html_text()



movie.year <- html %>% html_elements(".article_movie_title .subtle") %>% html_text()
movie.year <- as.numeric(str_sub(movie.year,2,-2))

final.netflix <- data.frame(Ranking = 100:1, Movie_Name = movie.names, Tomato_Score = Tomato_score, Year = movie.year)
View(final.netflix)




## problem 9
rm(list = ls())
library(rvest)
library(stringr)

html <- read_html("https://en.wikipedia.org/wiki/United_States_at_the_Olympics")

all.medals <- html %>% html_table()
View(rbind(all.medals[[4]][2:31,1:8],replace(all.medals[[4]][35,1:8],2,"*")))


## problem 10
rm(list = ls())
library(rvest)
library(stringr)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")

questions <- html %>% html_elements(".s-post-summary--content-title a") %>% html_text()

views <- as.numeric(substring(html %>% html_elements(".is-supernova") %>% html_attr("title"), 1, 6))


votes.ans <- html %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()

only.votes <- as.numeric(votes.ans[seq(1,43,3)])

only.ans <- as.numeric(votes.ans[seq(2,44,3)])

final_frame <- data.frame(Question.titles = questions, No.of.views = views, No.of.ans = only.ans, No.of.votes = only.votes)

View(final_frame)
