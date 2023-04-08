rm(list=ls())

Maxgen = 0
Generation = seq(from=1, to=Maxgen)

#### MAIN PROGRAM ####


output = function() {
    print('Observed lambda in last generation and ratio')
    print(c(Obs.Lambda[Maxgen], Obs.Lambda[Maxgen] / Lambda))
    par(mfrow=c(2,2))
    plot(Generation, Pop[,1], type='l', ylim=c(min(Pop), max(Pop)), ylab='Population and cohort sizes')
}    
