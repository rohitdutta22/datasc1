
###########################################
## Pseudo-random number generation
## using multiple congruential method
###########################################
m <- (2^31) - 1  # choose a value you want
a <- 7^5   # choose a value you want
x <- numeric(length = 1e3)
x[1] <- 2^20  #x0 -- choose
for(i in 2:1e3){
  x[i] <- ((a * x[i-1]) ) %% m
}


# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too

x[1:10]


#mixed congruentiqal method
m <- (2^31) - 1  # choose a value you want
a <- 7^5   # choose a value you want
c <- 2^25
x <- numeric(length = 1e3)
x[1] <- 10  #x0 -- choose
for(i in 2:1e3){
  x[i] <- ((a * x[i-1]) + c) %% m
}


# We will visualize using histograms (for testing uniformity)
# and trace plots (for checking independence)
par(mfrow = c(1,2))
hist(x/m) # looks close to uniformly distributed
plot.ts(x/m) # look like it's jumping around too
