rm(list=ls())

age.weight = function(n) {
    x <- 1
    age <- seq(from=1, to=n)
    wt <- 4*x*exp(-(1+x/2)*age)
    return(sum(wt))
}    

size.fitness = function(x) {
    age <- seq(from=1, to=N.Max)
    wt <- 4*x*exp(-(1+x/2)*age)
    return(sum(wt))
}

derivative = function(x) {
    (4) + (0+4*x) * (-0.5) + (0+4*x) * (-1) * (0.5*exp(-(1+0.5*x))) / (1-exp(-(1+0.5*x)))
}


#### Main program ####
N.Max = 20

Age = matrix(seq(from=1, to=N.Max))
Weight = apply(Age, 1, age.weight)

Body.Size = matrix(seq(from=0, to=5, length=100))
Fitness = apply(Body.Size, 1, size.fitness)

Max.Root = uniroot(derivative, interval=c(0,4))$root
Max.Numeric = nlm(function(x) { -1*size.fitness(x) }, p=1)$estimate

print('age-specific-mortality done.')

output = function() {
    par(mfcol=c(2,1))
    plot(Age, Weight, type='l', xlab='Age', ylab='Weight, Wt', las=1, lwd=3)
    plot(Body.Size, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    print("Maximum by root of derivative")
    print(Max.Root)
    print("Maximum by numerical search")
    print(Max.Numeric)
}    
