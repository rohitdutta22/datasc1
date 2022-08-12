
rm(list = ls())
library(imager)
setwd("D:/IITK Data Science Lab 1/worksheet-4-rohitdutta22-main")

dog <- load.image("dog.jpeg")
dog
dim(dog)
plot(dog)


col.mat <- as.array(dog[,,1,])
dim(col.mat)

crop.dog <- as.cimg(col.mat[1:300,,])
plot(crop.dog)

col.dog.green <- as.cimg(col.mat[,,2])
plot(col.dog.green)

##problem 1 (purest green)

pixel.purest.green <- sqrt((col.mat[,,1])^2 + (col.mat[,,2] - 1)^2 + (col.mat[,,3])^2)

ind.green <- which(pixel.purest.green == min(pixel.purest.green), arr.ind=TRUE)
ind.green

plot(dog)
points(x = ind.green[,1], y = ind.green[,2], col = "red",pch = 19, cex = 1)


##problem 2 (purest red)

pixel.purest.green <- sqrt((col.mat[,,1] - 1)^2 + (col.mat[,,2])^2 + (col.mat[,,3])^2)

ind.red <- which(pixel.purest.green == min(pixel.purest.green), arr.ind=TRUE)
ind.red

plot(dog)
points(x = ind.red[,1], y = ind.red[,2], col = "red",pch = 19, cex = 1)


##problem 2 (purest blue)

pixel.purest.blue <- sqrt((col.mat[,,1])^2 + (col.mat[,,2])^2 + (col.mat[,,3] - 1)^2)

ind.blue <- which(pixel.purest.blue == min(pixel.purest.blue), arr.ind=TRUE)
ind.blue

plot(dog)
points(x = ind.blue[,1], y = ind.blue[,2], col = "red",pch = 19, cex = 1)




##problem 3 (Identifying red, blue or green)

col.predict <- function(m){
  
  col.mat.load <- as.array(load.image(m)[,,1,])

  red.mean <- mean(col.mat.load[,,1])
  green.mean <- mean(col.mat.load[,,2])
  blue.mean <- mean(col.mat.load[,,3])
  if(red.mean > green.mean & red.mean > blue.mean){
    return("The file is of Red color")
  }else if(green.mean > red.mean & green.mean > blue.mean){
    return("The file is of Green color")
  }else{
    return("The file is of Blue color")
  }
}

col.predict('col3.png')

## alternative

col.predict <- function(m){
  
  col.mat.load <- as.array(load.image(m)[,,1,])
  
  mean.col <- c(mean(col.mat.load[,,1]),mean(col.mat.load[,,2]),mean(col.mat.load[,,3]))
  
  ifelse(which(mean.col == max(mean.col)) == 1,return("Red"),ifelse(which(mean.col == max(mean.col)) == 2,return("Green"),return("Blue")))
}
col.predict("col3.png")



snow.predict <- function(m){
  
  col.mat.snow <- as.array(load.image(m)[,,1,])
  
  distance.white.mat <- sqrt((col.mat.snow[,,1] - 1)^2 + (col.mat.snow[,,2] - 1)^2 + (col.mat.snow[,,3] - 1)^2)
  
  ifelse(mean(distance.white.mat) < 0.57,return("The picture has a lot of snow"),return("The picture has not that much snow"))

  }
snow.predict("land1.jpeg")


## alternative

snow.predict1 <- function(m){
  
  col.mat.snow <- as.array(load.image(m)[,,1,])
  
  distance.white.mat <- sqrt((col.mat.snow[,,1] - 1)^2 + (col.mat.snow[,,2] - 1)^2 + (col.mat.snow[,,3] - 1)^2)
  
  dimension.mat <- dim(col.mat.snow)
  
  a <- length(distance.white.mat[which(distance.white.mat < 0.51)])/(dimension.mat[1]*dimension.mat[2])
  #ifelse(a > 0.50,return("The picture has lot of snow"),return("Picture does not have lot of snow"))
  return(a)
}
snow.predict1("land3.png")

