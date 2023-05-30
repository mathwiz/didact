# Host choice in parasitoids: fitness decreases with time

# General assumptions
# 1. The animal commences the time period with some fixed quantity of eggs, as occurs, for example, in some Lepidoptera. In general, animals can be classified into capital breeders that use only, or primarily, resources gathered prior to maturity and income breeders that garner resources for reproduction after maturity. The present model applies to capital breeders, though it can easily be adapted for income breeders.
# 2. Patches or hosts vary in quality.
# 3. The survival and growth of larvae depend on the number in the clutch and host quality.
# 4. Variation in host quality can be detected by the ovipositing female.
# 5. Survival of the female may or may not change over time. For computational simplicity, we assume that the sequence of events is that egg-laying precedes the determination of survival over the time period.
# 6. Only one host at most is encountered per time interval.
# 7. Hosts already with eggs are not encountered.
# 8. Fitness is a function of the number of offsping.
#
# Mathematical assumptions
# 1. There are four types of host.
# 2. The single host fitness can be modeled by a cubic function: Benefit(i,n) = a(i) + b(i)n + c(i)n^2 + d(i)n^3.
# 3.The probability of encountering a host is constant but different for each host, designated as Pbenefit(i), where i is the host type. Thus the probability of not encountnering a host, P0, is: P0 = 1 - sum(1, 4, Pbenefit(i))
# 4. We shall assume a constant mortality per unit time, Pmortality = 0.01. At the end of the season no further eggs can be laid, meaning that the female is, from the point of view of natural selection, dead. For computational simplicity we shall use Psurvival = 1 - Pmortality
#
#                                Model
#
# Host found     Survives           X
# Yes              Yes           x - Benfit(i, n)
# Yes              No            x - Benfit(i, n)
# No               Yes              X
# No               No               X
#
#

rm(list=ls())

print("Using the decision matrix: individual prediction")
rm(list=ls())

## set path to save files if desired
## setwd(".")

DM1 <- read.table(file="DM1.dat")
DM2 <- read.table(file="DM2.dat")
DM3 <- read.table(file="DM3.dat")
DM4 <- read.table(file="DM4.dat")

#### MAIN PROGRAM #####
set.seed(10)

Xmax <- 40
DM <- array(0, c(20, Xmax, 4))  # time, state, host

for(i in 1:20) {
    for(j in 1:Xmax) {
        DM[i,j,1] <- DM1[i,j]
        DM[i,j,2] <- DM2[i,j]
        DM[i,j,3] <- DM3[i,j]
        DM[i,j,4] <- DM4[i,j]
    }
}    

Pbenefit <- c(0.05, 0.05, 0.10, 0.80)
Times <- Pbenefit * 100
# Create vector for Host type probability
Host.Type <- c()
for(i in 1:length(Times)) {
    Host.Type <- c(Host.Type, rep(i, Times[i]))
}

Psurvival <- 0.99
Horizon <-10
N.Ind <- 1000
Output <- matrix(0, N.Ind, Horizon)

# Generate initial values of x from normal dist
x.init <- ceiling(rnorm(N.Ind, mean=20, sd=5))
for(Ind in 1:N.Ind) {
    # Generate vectors for choosing the Host type and probability of survival
    Host <- ceiling(100*runif(Horizon))  # vector of host types
    Survival <- runif(Horizon)  # probabilities
    # Zero out all values of Survival > Psurvival
    Survival[Survival > Psurvival] <- 0
    # Set all others to 1
    Survival[Survival != 0] <- 1
    x <- x.init[Ind]
    # iterate over time periods
    for(Time in 1:Horizon) {
        # If eggs remaning calculate clutch size using DM
        if(x > 0) {
            Clutch.Size <- DM[Time, x, Host.Type[Host[Time]]]
            Output[Ind, Time] <- Clutch.Size
            # compute new x
            x <- x - Clutch.Size
        }
        # set x=0 if female does not survive
        x <- x*Survival[Time]
    }    
}    


par(mfrow=c(5,2))

for(i in 1:10) {
    # Create bar charts
    Data <- Output[,i]
    # Eliminate zeros
    Data <- Data[Data > 0]
    xbar <- mean(Data)
    print(paste("Time:", i, "Mean Clutch:", xbar))
    Table <- table(Data)
    barplot(Table, xlab="Clutch Size", space=0, xlim=c(0,5), main=paste("Time = ", i), col=1)
}    

