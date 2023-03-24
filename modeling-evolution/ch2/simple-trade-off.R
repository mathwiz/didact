rm(list=ls())

fitness = function(x) {
    -2*x^2 + 4*x
}

#### Main program ####

X = seq(0, 2, length=1000)
W = sapply(X, fitness)

print('simple-trade-off done.')

output = function() {
    plot(X, W, type='l', xlab='Body size', ylab='Fitness, W', las=1, lwd=3)
}    
