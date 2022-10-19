dat <- read.csv("auto-mpg.csv")
View(dat)


cor(dat$acceleration, dat$mpg)
# it seems that they are moderately positively correlated among themselves


plot(dat$acceleration, dat$mpg, xlab = " accelaration",
     ylab = "miles per gallon", main = "scatterplot of acceleration vs mpg",
     col = "blue", pch = 19)
abline(lm(dat$mpg ~ dat$acceleration), col = "red", lwd = 2)


plot(dat$acceleration, dat$mpg, xlab = " accelaration",
     ylab = "miles per gallon", main = "scatterplot of acceleration vs mpg",
     col = dat$cylinders, pch = 19)
for(i in unique(dat$cylinders)){
  temp_dat <- subset(dat, dat$cylinders == i)
  abline(lm(temp_dat$mpg ~ temp_dat$acceleration), col = i)
}
legend("topleft", legend = unique(dat$cylinders), col = unique(dat$cylinders), pch = 19)




View(iris)
plot(iris$Petal.Length,iris$Petal.Width, col = iris$Species, pch = 19)
abline(lm(iris$Petal.Width ~ iris$Petal.Length), col = "pink", lwd = 2)
legend("topleft", legend = unique(iris$Species), col = unique(iris$Species), pch = 19)


foo <- subset(iris, iris$Species == "virginica")
cor(iris$Petal.Length, iris$Petal.Width)











