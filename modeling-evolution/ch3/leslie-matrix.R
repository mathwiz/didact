rm(list=ls())


#### MAIN PROGRAM ####
Leslie.Mat = matrix(
    c(0.8, 1.2, 1.0, 0.0,
      0.8, 0.0, 0.0, 0.0,
      0.0, 0.4, 0.0, 0.0,
      0.0, 0.0, 0.25, 0.0),
4, 4, byrow=T)
Lambda = eigen(Leslie.Mat)$values[1]
Maxgen = 12
N = c(1, 0, 0, 0)
Pop = matrix(0, Maxgen, 5)
Pop[1,] = c(N, sum(N))
Obs.Lambda = matrix(0, Maxgen, 5)

for(i in 2:Maxgen) {
    N = Leslie.Mat %*% N
    Pop[i, 1:4] = N[1:4]
    Pop[i, 5] = sum(N)
    Obs.Lambda[i,] = Pop[i,] / Pop[i-1,]
}    


output = function() {
    print('Observed lambda in last generation and ratio')
    print(c(Obs.Lambda[Maxgen], Obs.Lambda[Maxgen] / Lambda))
    par(mfrow=c(2,2))
    Generation = seq(from=1, to=Maxgen)
    # Plot population and cohort trajectories
    plot(Generation, Pop[,1], type='l', ylim=c(min(Pop), max(Pop)), ylab='Population and cohort sizes')
    for(i in 2:4) { lines(Generation, Pop[,i]) } # cohorts 2-4
    lines(Generation, Pop[,5], lty=2) # total population
    # Plot log of population and cohort trajectories
    x <- matrix(Pop, length(Pop), 1)
    ymin <- min(log(x[x != 0]))
    ymax <- max(log(Pop))
    plot(Generation, log(Pop[,1]), type='l', ylim=c(ymin, ymax), ylab='Log population and cohort sizes')
    for(i in 2:4) { lines(Generation, log(Pop[,i])) } # cohorts 2-4
    lines(Generation, log(Pop[,5]), lty=2) # total population
    # PLot observed lambdas
    plot(Generation, Obs.Lambda[,1], type='l', ylab='Lambda')
    for(i in 2:4) { lines(Generation, Obs.Lambda[,i]) } # cohorts 2-4
    lines(Generation, Obs.Lambda[,5], lty=2) # total population
    # Plot observed r
    plot(Generation, log(Obs.Lambda[,1]), type='l', ylab='r')
    for(i in 2:4) { lines(Generation, log(Obs.Lambda[,i])) } # cohorts 2-4
    lines(Generation, log(Obs.Lambda[,5]), lty=2) # total population
}    
