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

    return (c(Npop, i))
}

Gens = matrix(rep(Max.Gen, times=N.Reps))
# row 1 is final populations, row 2 is generations
Results = apply(Gens, 1, pop, N.Init, Max.Lambda)
# set all populations below 1 to 0
Extinct.Cols = Results[1,] < 1
Results[1, Extinct.Cols] = 0
# set all generations that go extinct to the generation before the event
Results[2, Extinct.Cols] = Results[2, Extinct.Cols] - 1

print("replicates3 finished.")

# Output
output = function() {
#    plot(Generation, Npop, xlab='Generation', ylab='Popluation size', type='l')
    print("Final population sizes and generations")
    print(t(Results))
}

