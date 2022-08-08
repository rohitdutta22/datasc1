## assingment 1

tennis <- function(p)
{
  win.set <- 0
  count <- 0
  while(win.set <= 3 & count < 5)
  {
    win.set <- win.set + sample(c(1,0),1,prob <- c(p,1-p),replace = F)
    count <- count + 1
  }
  return(count)
}
tennis(0.7)

matches <- numeric(length = 1000)
for(i in 1:1000)
{
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)
ans