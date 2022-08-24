## worksheet 7
rm(list = ls())
library(rvest)
library(stringr)

html1 <- read_html("https://www.imdb.com/chart/top/")

movie.name <- html1 %>% html_elements(".titleColumn a") %>% html_text()
movie.name

## link unique id of all the movies
link <- html1 %>% html_elements("div .seen-widget") %>% html_attr("data-titleid")

movie_links =0
for(i in 1:250){
 movie_links[i] <- paste("https://www.imdb.com/title/",link[i],"/ratings",sep = "")
}
movie_links    ## links of all the movies


html <- read_html(movie_links[1])
all_ratings <- html %>% html_table()
vote_number <- as.numeric(gsub(",","",all_ratings[[1]][[3]]))
vote_number

male_votes <- as.numeric(gsub(",","",str_sub(gsub("\n","",gsub(" ","",all_ratings[[2]][2,2])),4,-1)))
female_votes <- as.numeric(gsub(",","",str_sub(gsub("\n","",gsub(" ","",all_ratings[[2]][3,2])),4,-1)))


