# using Fisherian optimization
rm(list=ls())

rcalc = function(x) {
    af <- 0; bf <- 16; as <- 1; bs <- 0.5
    a <- af + bf*x
    b <- as + bs*x
    return(a*exp(-b) / (1 - exp(-b)))
}

Result = optimize(f=rcalc, interval=c(0.1, 3), maximum=T)$maximum
print('Calculated')
print(Result)


# using numerical method
## rm(list=ls())

## summation = function(r, x) {
##     maxage <- 50
##     af <- 0; bf <- 16; as <- 1; bs <- 0.5
##     age <- seq(from=1, to=maxage)
##     m <- rep(af + bf*x, times=maxage)
##     surv <- exp(-(as + bs*x) * age)
##     res <- sum(exp(-r*age) * surv * m)
##     return(1 - res)
## }    

## rcalc = function(x) {
##     uniroot(summation, interval=c(-1, 2), x)$root
## }

## Result = optimize(f=rcalc, interval=c(0.1, 3), maximum=T)$maximum
## print('Numerical')
## print(Result)

## # using Leslie matrix
## rm(list=ls())

## rcalc = function(x) {
##     maxage <- 50
##     age <- seq(from=1, to=maxage)
##     af <- 0; bf <- 16; as <- 1; bs <- 0.5
##     m <- rep(af + bf*x, times=maxage)
##     surv <- exp(-(as + bs*x) * age)
##     age.surv <- matrix(0, maxage, 1)
##     age.surv[1] = surv[1]
##     for(i in 2:(maxage-1)) {
##         age.surv[i] <- surv[i] / surv[i-1]
##     }
##     fertility <- m * age.surv
##     dummy <- matrix(0, maxage-1, maxage-1)
##     diag(dummy) <- age.surv[1:(maxage-1)]
##     leslie.mat <- matrix(0, maxage, maxage)
##     leslie.mat[1,] <- fertility
##     leslie.mat[2:maxage,1:(maxage-1)] <- dummy
##     leslie.eigen <- eigen(leslie.mat)
##     lambda <- leslie.eigen$values[1]
##     return(abs(log(lambda)))
## }    

## Result = optimize(f=rcalc, interval=c(1.0, 3.0), maximum=T)
## Best.X = Result$maximum
## Best.R = Result$objective
## print('Leslie matrix')
## print(c('Best x = ', signif(Best.X, 6), 'Best r = ', signif(Best.R, 6)))
## N = 50
## X = matrix(seq(from=1.0, to=2.5, length=N))
## R.Est = apply(X, 1, rcalc)
## par(mfrow=c(1,1))
## plot(X, R.Est, xlab='Body size, x', ylab='r est', type='l')
## points(Best.X, Best.R, cex=2) # highlight optimum
