rm(list=ls())

set.seed(100)

Max.Gen = 100
N.Init = 20
Max.Lambda = 2.2
N.Reps = 10000

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

Max.Gens = matrix(rep(Max.Gen, times=N.Reps))
# row 1 is final populations, row 2 is generations
Results = apply(Max.Gens, 1, pop, N.Init, Max.Lambda)
# set all populations below 1 to 0
Extinct.Cols = Results[1,] < 1
Results[1, Extinct.Cols] = 0
# set all generations that go extinct to the generation before the event
Results[2, Extinct.Cols] = Results[2, Extinct.Cols] - 1
# Named copies for convenience
Pops.Size = Results[1,]
Num.Generations = Results[2,]
Pops.Not.Extinct = Results[1, !Extinct.Cols]

# Run a t-test against the expected value
Pop.Expected = 250556.6
Data = Pops.Size - Pop.Expected
T.Results = t.test(Data)


print("replicates5 finished.")

# Output
output = function() {
    par(mfrow=c(2,2))
    hist(Pops.Size)
    hist(Pops.Not.Extinct)
    hist(Num.Generations)
    hist(log10(Pops.Not.Extinct))
    print("All Population Sizes")
    print(summary(Pops.Size))
    print("Non-Extinct Population Sizes")
    print(summary(Pops.Not.Extinct))
    print("Number of Generations")
    print(summary(Num.Generations))
    print("T-test vs Non-random model")
    print(T.Results)
}

