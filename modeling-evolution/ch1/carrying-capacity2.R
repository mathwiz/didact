rm(list=ls())

set.seed(100)
Carrying.Capacity = 1000

pop = function(Max.Lambda, Npop, N.Patches, P.Mig, P.Surv) {
    # randomize growth rate
    Lambda <- runif(N.Patches, min=0, max=Max.Lambda)
    Emigrants <- Npop * P.Mig
    Immigrants <- sum(Emigrants) * P.Surv/N.Patches
    Npop <- (Npop - Emigrants + Immigrants) * Lambda
    # apply carrying capacity
    Npop[Npop > Carrying.Capacity] <- Carrying.Capacity
    # set extinct populations to exactly zero
    Npop[Npop < 1] <- 0
    return(Npop)
}

main = function(data) {
    # process sequence of probabilities into parameters for model
    P.Mig = data[1]
    P.Surv = data[2]

    Max.Gen = 1000
    N.Init = 20
    Max.Lambda = 2.2
    N.Patches = 10

    # initialize populations
    Npop = matrix(N.Init, N.Patches, 1)
    Npop.Sizes = matrix(0, Max.Gen)
    # first generation
    Npop.Sizes[1] = mean(Npop)
    N.Extinct = matrix(0, N.Patches, 1)

    Gen <- 1
    while (Gen < Max.Gen && Npop.Sizes[Gen] > 0) {
        Gen <- Gen + 1
        Npop <- pop(Max.Lambda, Npop, N.Patches, P.Mig, P.Surv)
        Npop.Sizes[Gen] <- mean(Npop)
        N.Extinct[Gen] <- length(Npop[Npop==0])
    }    

} #main

P.Mig = seq(from=0.1, to=0.9, length=10)
P.Surv = seq(from=0.8, to=0.9, length=10)
Data = expand.grid(P.Mig, P.Surv)
Z = apply(Data, 1, main)
Z.Matrix = matrix(Z, length(P.Mig), length(P.Surv))

print("carrying-capacity finished.")

# Output
output = function() {
    par(mfrow=c(1,2))
    contour(P.Mig, P.Surv, Z.Matrix, xlab='P.Mig', ylab='P.Surv')
    persp(P.Mig, P.Surv, Z.Matrix, theta=20, phi=20)
}

