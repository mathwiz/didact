rm(list=ls())

set.seed(100)

Max.Gen = 100
N.Init = 20
Max.Lambda = 2.2
N.Patches = 10
N.Reps = 10000

pop = function(Max.Lambda, Npop, N.Patches) {
    # randomize growth rate
    Lambda <- runif(N.Patches, min=0, max=Max.Lambda)
    Npop <- Npop * Lambda
    # set extinct populations to exactly zero
    Npop[Npop < 1] <- 0
    return(Npop)
}

# initialize populations
Npop = matrix(N.Init, N.Patches, 1)
Npop.Sizes = matrix(0, Max.Gen)

# first generation
Npop.Sizes[1] = mean(Npop)
N.Extinct = matrix(0, N.Patches, 1)

i = 1
while (i < Max.Gen && Npop.Sizes[i] > 0) {
    i <- i + 1
    Npop <- pop(Max.Lambda, Npop, N.Patches)
    Npop.Sizes[i] <- mean(Npop)
    N.Extinct[i] <- length(Npop[Npop==0])
}    

print("spatial finished.")

# Output
output = function() {
    par(mfcol=c(1,2))
    plot(seq(1, i), Npop.Sizes[1:i], xlab="Generation", ylab="Mean Pop Size", type="l")
    plot(seq(1, i), N.Extinct[1:i], xlab="Generation", ylab="Number of Pops Extinct", type="l", ylim=c(0, N.Patches))
}

