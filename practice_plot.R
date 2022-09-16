## plots
library(ggplot2)
attach(iris)
ggplot(iris, aes(y = Sepal.Length, fill = Species))+
  geom_boxplot()+
  labs(title = "Boxplots of Sepal Length w.r.t Species\n")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


lengths <- c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) 
fact <- rep(c(rep("setosa",50), rep("versicolor",50), rep("virginica",50)), times = 4)
vec <- rep(c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"), each = 150)
  
iris.data <- data.frame(lengths, vec, fact)
View(iris.data)
install.packages("wesanderson")
library(wesanderson)
ggplot(iris.data, aes(x = vec,y = lengths, fill = fact))+
  geom_boxplot(color = "white", size = 0.8) +
  scale_fill_manual(values = wes_palette(n = 3, name = "Darjeeling1", )) + 
  theme_dark()


ggplot(iris.data, aes(x = vec,y = lengths, fill = fact))+
  geom_boxplot() 

ggplot(iris, aes(Sepal.Length,Petal.Length, col = Species, shape = Species))+
  geom_point(lwd = 2)


