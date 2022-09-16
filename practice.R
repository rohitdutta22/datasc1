##woksheet 3
A <- matrix(c(-3,1,-2,4,5,3,-1,2,-2), nrow = 3, ncol = 3)
p1 <- norm(as.matrix(A[,1]), type = "F")
p2 <- norm(as.matrix(A[,2]), type = "F")
p3 <- norm(as.matrix(A[,3]), type = "F")
p.sum <- p1 + p2 + p3

A[,sample(1:3, size = 1, prob = c(p1,p2,p3)/p.sum, replace = FALSE)]

runif(1,0,5)


sum.count = function(){
count <- 0
sum <- 0
 while(sum <= 1){
   sum = sum + runif(1)
   count <- count + 1
 }
return(count)
}
sum.count()

mean(replicate(1000,sum.count()))


blows.count = function(n){
  
candle.count <- 0  ## burning
iteration <- 0
while(candle.count < n){
  candle.count <- candle.count + sample(0:(n-candle.count),1,replace = FALSE)
  iteration <- iteration + 1
}
return(iteration)
}
blows.count(25)
set.seed(1234)
mean(replicate(1000,blows.count(30)))



## worksheet 5

library(imager)
dog.mat <- as.array(load.image("dog.jpeg")[,,1,])
dim.dog <- dim(dog.mat)
plot(grayscale(load.image("dog.jpeg")))

dog.mat.clock <- array(0,dim = c(dim.dog[2], dim.dog[1], 3))

for(j in 1:dim.dog[2]){
   for(i in 1:dim.dog[1]){
     for(k in 1:3){
       dog.mat.clock[j,i,k] <- dog.mat[i,dim.dog[2]-j+1,k]
     }
  }
}
plot(as.cimg(dog.mat.clock))    ## clockwise 90 deg


dog.mat.anticlock <- array(0, dim = c(dim.dog[2], dim.dog[1], 3))

for(j in 1:dim.dog[2]){
  for(i in 1:dim.dog[1]){
    dog.mat.anticlock[j,i,] <- dog.mat[dim.dog[1]-i+1,j,]
  }
}
plot(as.cimg(dog.mat.anticlock))  ## anti clockwise 90 deg


dog.mat.watermirror <- array(0, dim = dim.dog)
for(i in 1:dim.dog[1]){
  for(j in 1:dim.dog[2]){
    dog.mat.watermirror[i,j,] <- dog.mat[i,dim.dog[2]-j+1,]
  }
}
plot(as.cimg(dog.mat.watermirror))  ## water image


dog.mat.mirror <- array(0, dim = dim.dog)
for(i in 1:dim.dog[1]){
  for(j in 1:dim.dog[2]){
    dog.mat.mirror[i,j,] <- dog.mat[dim.dog[1]-i+1,j,]
  }
}
plot(as.cimg(dog.mat.mirror))  ## mirror image














diff.col = function(img,col.vec){
  img.mat <- as.array(img[,,1,])
  dist.mat <- matrix(0, nrow = dim(img.mat)[1], ncol = dim(img.mat)[2])
  for (i in 1:dim(img.mat)[1]) {
    for (j in 1:dim(img.mat)[2]) {
       dist.mat[i,j] <- norm(img.mat[i,j,] - col.vec, "2" )
    }
  }
  return(dist.mat)
}

dist.green <- diff.col(load.image("dog.jpeg"),c(0,1,0))
plot(load.image("dog.jpeg"))
purest.green.coor <- which(dist.green == min(dist.green), arr.ind = TRUE)
points(purest.green.coor[,1],purest.green.coor[,2], col = "red", pch = 19)



col1 <- load.image("col1.png")
col1 <- as.cimg(col1[,,,1:3])
col2 <- load.image("col2.png")
col3 <- load.image("col3.png")

which.col = function(img){
  dist.col <- numeric(length = 3)
  col.vector <- diag(3)
  for(k in 1:3){
    dist.col[k] <- mean(diff.col(img,col.vector[,k]))
  }
  guess <- c("Red","Green","Blue")[which.min(dist.col)]
  return(guess)
}

which.col(col2)


## compressed image

library(imager)
dog.mat <- as.array(load.image("dog.jpeg")[,,1,])
dim(dog.mat)
dog.mat.600 <- dog.mat[1:600,1:600,]
dim(dog.mat.600)

red.mat <- array(0,dim = c(300,300,3))

averaging <- rep(0,3)
for(i in 1:300){
  for (j in 1:300) {
    ind1 <- (2*(i - 1) + 1) : (2*i)
    ind2 <- (2*(j - 1) + 1) : (2*j)
    averaging[1] <- mean(dog.mat.600[ind1,ind2,1])
    averaging[2] <- mean(dog.mat.600[ind1,ind2,2])
    averaging[3] <- mean(dog.mat.600[ind1,ind2,3])
    red.mat[i,j,] <- averaging
  }
}
plot(as.cimg(red.mat))


red.mat.10 <- array(0,dim = c(60,60,3))
averaging <- rep(0,3)
for(i in 1:60){
  for (j in 1:60) {
    ind1 <- (10*(i - 1) + 1) : (10*i)
    ind2 <- (10*(j - 1) + 1) : (10*j)
    averaging[1] <- mean(dog.mat.600[ind1,ind2,1])
    averaging[2] <- mean(dog.mat.600[ind1,ind2,2])
    averaging[3] <- mean(dog.mat.600[ind1,ind2,3])
    red.mat.10[i,j,] <- averaging
  }
}
plot(as.cimg(red.mat.10))



library(imager)
pic <- load.image("campus.jpeg")

prop.color = function(img,col){
  
  pic.mat <- as.array(img[,,1,])
  dims <- dim(pic.mat)
  
  dist.mat <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1]){
    for(j in 1:dims[2]){
      dist.mat[i,j] <- norm(pic.mat[i,j,] - col, "2")
    }
  }
 prop <- length(dist.mat[dist.mat<=0.5])/(dims[1]*dims[2])
 return(prop)
}

prop.color(pic,c(.4,.3,.2))








