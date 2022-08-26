## paste all your relevant code here
## Make sure your code runs without errors.
## assignment 3
## assignment 3
rm(list = ls())
library(rvest)
html <- read_html("https://www.icc-cricket.com/rankings/womens/player-rankings/odi/batting")


r1 <- html %>% html_elements(".rankings-block__banner a") %>% html_text()
r1.name <- r1[2]
r.all.name <- html %>% html_elements(".table-body__cell a") %>% html_text()

rank <- 1:100


r.all.team <- html %>% html_elements(".table-body__logo-text") %>% html_text()


r.all.rating <- as.numeric(html %>% html_elements(".rating") %>% html_text())

name <- c(r1.name,r.all.name)

team <- c("AUS",r.all.team)


rating <- c(785,r.all.rating)


icc_rank <- data.frame(rank,name,team,rating)
