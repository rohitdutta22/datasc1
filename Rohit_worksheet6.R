rm(list = ls())
library(rvest)
library(stringr)
html <- read_html("https://www.iitk.ac.in/math/faculty")

name <- html %>% html_elements(".head3 a") %>% html_text()
name


html1 <- read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
name1 <- html1 %>% html_elements(".head2") %>% html_text()
name1


html2 <- read_html("https://www.imdb.com/chart/top/")

movie.name <- html2 %>% html_elements(".titleColumn a") %>% html_text()
movie.name


movie.year <- html2 %>% html_elements(".secondaryInfo") %>% html_text()
movie.year <- as.numeric(str_sub(movie.year,2,-2))
movie.year


movie.rating <- html2 %>% html_elements(".ratingColumn strong") %>% html_text()
movie.rating <- as.numeric(movie.rating)
movie.rating


movie.votes <- html2 %>% html_elements("strong") %>% html_attr("title")
movies.votes <- as.numeric(gsub(",","",str_sub(movie.votes,13,-13)))
movies.votes

data.frame(movie.name,movie.year,movie.rating,movies.votes)





nam <- html2 %>% html_elements(".posterColumn span") %>% html_attr("name")
ind = which(nam == "nv")
nam1 <- html2 %>% html_elements(".posterColumn span") %>% html_attr("data-value")
as.numeric(nam1[ind])

