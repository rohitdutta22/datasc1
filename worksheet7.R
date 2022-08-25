## worksheet 7
rm(list = ls())
library(rvest)
library(stringr)
library(imager)

html1 <- read_html("https://www.imdb.com/chart/top/")

movie.name <- html1 %>% html_elements(".titleColumn a") %>% html_text()
movie.name   ## name of the movies


movie.year <- html1 %>% html_elements("span .secondaryInfo") %>% html_text()
movie.year <- as.numeric(str_sub(movie.year,2,-2))  ## year of the movies


movie.rating <- html1 %>% html_elements("strong") %>% html_text()
movie.rating <- as.numeric(movie.rating)  ## movie ratings

## link unique id of all the movies
link <- html1 %>% html_elements("div .seen-widget") %>% html_attr("data-titleid")


movie_links =0
for(i in 1:250){
  movie_links[i] <- paste("https://www.imdb.com/title/",link[i],"/ratings",sep = "")
}
movie_links    ## links of all the movies


total_votes = male_votes = female_votes = array(0)
rate1 = rate2 = rate3 = rate4 = rate5 = rate6 = rate7 = rate8 = rate9 = rate10 = array(0)
male_ratings = female_ratings = array(0)

for(j in 1:250){

html <- read_html(movie_links[j])
all_ratings <- html %>% html_table()

all.male.female_votes <- as.numeric(gsub(",","",str_sub(gsub("\n","",gsub(" ","",all_ratings[[2]]$`All Ages`)),4,-1)))

total_votes[j] <- all.male.female_votes[1]
male_votes[j] <- all.male.female_votes[2]
female_votes[j] <- all.male.female_votes[3] 

vote_number <- as.numeric(gsub(",","",all_ratings[[1]][[3]]))
rate10[j] <- vote_number[1]
rate9[j] <- vote_number[2]
rate8[j] <- vote_number[3]
rate7[j] <- vote_number[4]
rate6[j] <- vote_number[5]
rate5[j] <- vote_number[6]
rate4[j] <- vote_number[7]
rate3[j] <- vote_number[8]
rate2[j] <- vote_number[9]
rate1[j] <- vote_number[10]



male.female_overall_rating <- as.numeric(substring(all_ratings[[2]]$`All Ages`,1,3))
male_ratings[j] <- male.female_overall_rating[2]
female_ratings[j] <- male.female_overall_rating[3]


}

final <- data.frame(Ranking = 1:250, Movies = movie.name, Year = movie.year, Total_Vote = total_votes, Final_Rating = movie.rating,Vote.R1 = rate1,  Vote.R2 = rate2, Vote.R3 = rate3, Vote.R4 = rate4, Vote.R5 = rate5, Vote.R6 = rate6, Vote.R7 = rate7, Vote.R8 = rate8, Vote.R9 = rate9, Vote.R10 = rate10, Votes_Men = male_votes, Votes_female = female_votes, Rating_Male = male_ratings, Rating_Female = female_ratings)                                                                                                                

View(final)  ## final data frame of problem 1


## problem 2
## url of posters of 250 movies
poster.links <- html1 %>% html_nodes("[width = '45']") %>% html_attr("src")
 


## problem 3
## Function that calculates proportions 
prop.color <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  prop <- length(dist[which(dist <= 0.2, arr.ind = TRUE)])/(dims[1]*dims[2])
  
  # return the required proportion of pixel 
  return(prop)
}


proportions_of_black2.0 <- array(0)
for(i in 1:250){
  proportions_of_black2.0[i] <- prop.color(load.image(poster.links[i]),c(0,0,0))

}

img.prop <- data.frame(Movie_Name = movie.name, Black_proportions = proportions_of_black2.0)
View(img.prop)
