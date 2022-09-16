tennis = function(p){
  wins <- array(0)
  i <- 1
  while(max(table(wins)) < 3){
   wins[i] <- sample(c("A","B"),1,prob = c(p,1-p),replace = TRUE)
   i <- i + 1
  }
  return(i-1)  
}
tennis(0.7)

set.seed(1234)
matches <- numeric(1000)

for(i in 1:1000){
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)
ans


packet.count = function(){
packet <- array(0)
count <- 0
i <- 1
while(length(unique(packet)) != 7){
  packet[i] <- sample(1:7, 1, prob = c(.25,.2,.2,.15,.10,.05,.05), replace = TRUE)
  i <- i + 1
  count <- count + 1
}
return(count)
}
mean(replicate(1000,packet.count()))


day.count = function(){
no.half <- 0
no.full <- 100
i <- 1
tab <- array(0)
while((no.half + no.full) == 100){
  
tab[i] <- sample(c("F","H"),1,prob = c(no.full/100,no.half/100), replace = TRUE)
 if(tab[i] == "F"){
 no.half <- no.half + 1
 no.full <- no.full - 1
 }else{
   no.half <- no.half -1
 }
i <- i + 1
}
return(length(tab))
}
set.seed(1234)
mean(replicate(1000,day.count()))


vitamin = function(){
i <- 0
count <- 0
tab <- "F"
while(tab != "H"){
  tab <- sample(c("F","H"), 1, prob = c((100-i)/100,i/100), replace = TRUE)
  count <- count + 1
  i <- i + 1
}
return(count)
}
vitamin()
set.seed(1234)
mean(replicate(1000,vitamin()))






