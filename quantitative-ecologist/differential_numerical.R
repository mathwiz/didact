library(deSolve)

init<- c(p=30, n=5)
times<- seq(0, 200, 0.1)
r<- 0.2  # prey growth rate
a<- 0.04 # functional response parameter
b<- 0.01 # numerical response parameter
m<- 0.1  # predator mortality
parms<- c(r, a, b, m)

predprey<- function(times, init, parms) {
    p<- init[1]
    n<- init[2]
    with(as.list(parms), {
        dp<- r*p - a*p*n # prey diff eq
        dn<- b*p*n - m*n # pred diff eq
        return(list(c(dp, dn)))
    })
}

solution<- as.data.frame(lsoda(init, times, predprey, parms))

## phase plot
plot(solution$p, solution$n)
lines(solution$p, solution$n)

## time series
plot(solution$t, solution$p, type="l", xlab="Time", ylab="Density", xlim=c(0,200), ylim=c(0,30))
lines(solution$t, solution$n, lty=2)
