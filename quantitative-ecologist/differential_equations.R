library(deSolve)

init<- c(p=10)
times<- seq(0,20,1/24)
params<- c(K=500, r=0.3)

f<- function(times, init, params) {
    p<- init[1]
    with(as.list(params), {
        dp<- r*p*(1 - p/K)  # put model here
        list(dp)
    })}

solution<- as.data.frame(lsoda(init, times, f, params))
head(solution)
plot(solution$time, solution$p, type="l", xlab="time", ylab="P(t)")

solution<- as.data.frame(lsoda(init, times, f, c(K=500, r=1.0)))
head(solution)
plot(solution$time, solution$p, type="l", xlab="time", ylab="P(t)")

