## problem 5

rm(list = ls())
library(imager)
setwd("D:/IITK Data Science Lab 1/worksheet-4-rohitdutta22-main")

reverse.180 <- function(m){
  
col.mat <- as.array(load.image(m)[,,1,])

reverse.mat <- array(0, dim = dim(col.mat))

 for(i in 1:dim(col.mat)[1]){
  for(j in 1:dim(col.mat)[2]){
    for(k in 1:3){
    
      reverse.mat[i,j,k] <- col.mat[dim(col.mat)[1]-i+1,dim(col.mat)[2]-j+1,k]
    }
  }
 }
 return(plot(as.cimg(reverse.mat)))

}
reverse.180("land1.jpeg")




mirror.img <- function(m){
  
  col.mat <- as.array(load.image(m)[,,1,])
  
  mirror.img <- array(0, dim = dim(col.mat))
  rev.mirror <- array(0, dim = dim(col.mat))
  for (i in 1:dim(col.mat)[1]){
    for(j in 1:dim(col.mat)[2]){
        mirror.img[i,j,] <- col.mat[i,dim(col.mat)[2]-j+1,]
    }
  }
  
  for(i in 1:dim(col.mat)[1]){
    for(j in 1:dim(col.mat)[2]){
      for (k in 1:3){
      rev.mirror[i,j,k] <- mirror.img[dim(mirror.img)[1]-i+1,dim(mirror.img)[2]-j+1,k]
      }
    }
  }
  return(plot(as.cimg(rev.mirror)))
}
mirror.img("dog.jpeg")




mirror.main <- function(m){
  col.mat <- as.array(load.image(m)[,,1,])
  d <- dim(col.mat)
  mirror.main.func <- array(0, dim = d)
  for (i in 1:dim(col.mat)[1]){
    for (j in 1:dim(col.mat)[2]){
      mirror.main.func[i,j,] <- col.mat[dim(col.mat)[1]-i+1,j,]      
    }
  }
  return(plot(as.cimg(mirror.main.func)))
}
mirror.main("dog.jpeg")



water.img <- function(m){
  col.mat <- as.array(load.image(m)[,,1,])
  d <- dim(col.mat)
  water <- array(0,dim = d)
  for (i in 1:d[1]){
    for (j in 1:d[2]){
      water[i,j,] <- col.mat[i,d[2]-j+1,]
    }
  }
  return(plot(as.cimg(water)))
}
water.img("dog.jpeg")

par(mfrow = c(2,1))
plot(load.image("dog.jpeg"))
water.img("dog.jpeg")






reverse.90 <- function(m){
  
  col.mat <- as.array(load.image(m)[,,1,])
  
  reverse.mat <- array(0, dim = c(dim(col.mat)[2],dim(col.mat)[1],3))
  for(j in 1:dim(col.mat)[2]){
    for(i in 1:dim(col.mat)[1]){
      for(k in 1:3){
        
        reverse.mat[j,i,k] <- col.mat[i,dim(col.mat)[2]-j+1,k]
      }
    }
  }
  return(plot(as.cimg(reverse.mat)))
  
}
reverse.90("land1.jpeg")




reverse.anti.90 <- function(m){
  
  col.mat <- as.array(load.image(m)[,,1,])
  
  reverse.mat <- array(0, dim = c(dim(col.mat)[2],dim(col.mat)[1],3))
  for(j in 1:dim(col.mat)[2]){
    for(i in 1:dim(col.mat)[1]){
      for(k in 1:3){
        
        reverse.mat[j,i,k] <- col.mat[dim(col.mat)[1]-i+1,j,k]
      }
    }
  }
  return(plot(as.cimg(reverse.mat)))
  
}
reverse.anti.90("land1.jpeg")





## problem 4

col.mat.dog <- as.array(load.image("dog.jpeg")[,,1,])
plot(as.cimg(col.mat.dog[1:600,1:600,]))
crop.600 <- col.mat.dog[1:600,1:600,]
dim(crop.600)
compress.300 <- array(0, dim = c(300,300,3))

i1 <- 1
for (k in 1:3) {
  i1 <- 1
  for(i in 1:300){
    j1 <- 1
    for (j in 1:300){
       compress.300[i,j,k] <- (crop.600[i1,j1,k]+crop.600[i1,j1+1,k]+crop.600[i1+1,j1,k]+crop.600[i1+1,j1+1,k])/4             
       j1 <- j1 + 2
    }
    i1 <- i1 + 2
  }  
}

plot(as.cimg(compress.300))

for (i in 1:300){
  ind1 <- (2*(i-1) + 1) : (2*i)
  for (j in 1:300){
    ind2 <- (2*(j-1) + 1) : (2*j)
     for(k in 1:3){
    compress.300[i,j,k] <- mean(crop.600[ind1,ind2,k])
     }
  }
}
save.image(as.cimg(compress.300), file = "dog_300.jpeg")



## problem 5

col.mat.dog <- as.array(load.image("dog.jpeg")[,,1,])
plot(as.cimg(col.mat.dog[1:600,1:600,]))
crop.600 <- col.mat.dog[1:600,1:600,]
dim(crop.600)
compress.60 <- array(0, dim = c(60,60,3))


for (i in 1:60){
  ind1 <- (10*(i-1) + 1) : (10*i)
  for (j in 1:60){
    ind2 <- (10*(j-1) + 1) : (10*j)
    for(k in 1:3){
      compress.60[i,j,k] <- mean(crop.600[ind1,ind2,k])
    }
  }
}
plot(as.cimg(compress.60))
save.image(as.cimg(compress.60), file = "dog_60.jpeg")





