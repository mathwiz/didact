## Model below with mean 1.05 and variance 0.01
##
## P[t] / P[0] = R[t-1] * ... * R[0]
##
T <- 50
r <- 0.05
sd <- 0.1
h <- pmax(0, rnorm(T, 1 + r, sd)) # values of truncated normal dist
PT <- cumprod(h) # ratio of final over initial population density

plot(PT, type="l", xlab="Year", ylab=expression(P[t]/P[0]))

