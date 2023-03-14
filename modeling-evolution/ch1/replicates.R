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

results = matrix(0, N.Reps, 2) # column 1 is population, column 2 is generation
for (i in 1:N.Reps) {
    results[i,] <- pop(Max.Gen, N.Init, Max.Lambda)
}    

print("replicates finished.")

# Output
output = function() {
#    plot(Generation, Npop, xlab='Generation', ylab='Popluation size', type='l')
    print("Final population sizes and generations")
    print(results)
}

