rm(list = ls())
library("tidyverse")
library(rvest)
html <- read_html("https://www.iitk.ac.in/math/faculty")

name <- html %>% html_elements(".head3 a") %>% html_text()
name


html1 <- read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
name1 <- html1 %>% html_elements(".head2") %>% html_text()
name1


html2 <- read_html("https://www.imdb.com/chart/top/")
name2 <- html2 %>% html_elements(".titleColumn a") %>% html_text()
name2


x<- "apple is red"
substring(x,10,12)





name3 <- html2 %>% html_elements(".ratingColumn imdbRating strong") %>% html_attr(strong,substring(title,14,18)) ;name3
