## worksheet 4
getwd()
    
setwd("/users/math/msc/rohitdutta22/Desktop/221396/worksheet-4-rohitdutta22-main")
library(imager)
dog <- load.image("dog.jpeg")
dim(dog) # stored as RGB
plot(dog) # plot image


graydog <- grayscale(dog)
plot(graydog)
dim(graydog)


# Extract the black and white image as matrix
gray.mat <- as.matrix(graydog[,,1,1])
dim(gray.mat)

# Extracts the array will all three rgb channels
col.mat <- as.array(dog[, ,1, ])
dim(col.mat)
col.mat[,,1]
col.dog <- as.cimg(col.mat[,,2])
plot(col.dog)
points(which(min(norm(col.mat-c(0,1,0))))

# Vertical cropping
cropped.mat <- col.mat[1:300, , ]
crop.dog <- as.cimg(cropped.mat)
plot(crop.dog)
  

## problem 1
cropped.mat.green <- col.mat[,,1,which(col.mat[,,,2]==c(0,1,0),arr.ind = TRUE)]
crop.dog.green <- as.cimg(cropped.mat.green)
plot(cropped.dog.green)
