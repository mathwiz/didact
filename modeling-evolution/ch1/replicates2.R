rm(list=ls())

set.seed(100)

Max.Gen = 100
N.Init = 20
Max.Lambda = 2.2
N.Reps = 10

pop = function(Maxgen, Npop, Max.Lambda) {
    # randomize growth rate
    Lambda = runif(Maxgen, min=0, max=Max.Lambda)

    i <- 1
    while (i < Maxgen && Npop > 1) {
        i <- i + 1
        Npop <- Lambda[i-1] * Npop
    }    

    return (c(Npop, i-1))
}

Gens = matrix(rep(Max.Gen, times=N.Reps))
# row 1 is final populations, row 2 is generations
Results = apply(Gens, 1, pop, N.Init, Max.Lambda)

print("replicates2 finished.")

# Output
output = function() {
#    plot(Generation, Npop, xlab='Generation', ylab='Popluation size', type='l')
    print("Final population sizes and generations")
    print(t(Results))
}

