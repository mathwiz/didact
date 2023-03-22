rm(list=ls())

set.seed(100)

Max.Gen = 1000
Carrying.Capacity = 1000
N.Init = 20
Max.Lambda = 2.2
N.Patches = 10
P.Mig = 0.5
P.Surv = 0.95

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

print("carrying-capacity finished.")

# Output
output = function() {
    par(mfcol=c(2,1))
    plot(seq(1, Gen), Npop.Sizes[1:Gen], xlab="Generation", ylab="Mean Pop Size", type="l")
    plot(seq(1, Gen), N.Extinct[1:Gen], xlab="Generation", ylab="Number of Pops Extinct", type="l", ylim=c(0, N.Patches))
}

