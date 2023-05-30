# Optimizing egg and clutch size: dealing with two state variables

# General assumptions
# 1. The animal commences the time period with some fixed quantity of resources that can be divided into clutches and eggs of different sizes. Thus although we have two state variables, egg size and clutch size, these can be combined operationally into a single variable, reproductive biomass, X.
# 2. Patches or hosts vary in quality.
# 3. The survival and growth of larvae depend on the number in the clutch, egg size, and host quality.
# 4. Variation in host quality can be detected by the ovipositing female.
# 5. Survival of the female may or may not change over time. For computational simplicity, we assume that the sequence of events is that egg-laying precedes the determination of survival over the time period.
# 6. Only one host at most is encountered per time interval.
# 7. Hosts already with eggs are not encountered.
# 8. Fitness is a function of the number of offsping.
#
# Mathematical assumptions
# 1. There are four types of host.
# 2. The amount of reproductive biomass available at time t is B, which is equal to the product of egg size and clutch size, x(t) = X.E * X.C.
# 3. The single host fitness can be modeled by the function: Benefit(i,E,C) = Wmax - sqrt( a.E(i)*(X.E - b.E(i))^2 + a.C(i)*(X.C - b.C(i))^2 )
# 4. A host is encountered during each time step: the probability of encountering host type 1 is P(1) = 0.5 and host type 2 P(2) = 0.5.
# 4. We shall assume a constant mortality per unit time, Pmortality = 0.1. At the end of the season no further eggs can be laid, meaning that the female is, from the point of view of natural selection, dead. For computational simplicity we shall use Psurvival = 1 - Pmortality
#
#                                Model Parameters
#
# Host             Wmax             a.E               a.C             b.E            b.C
# 1                10               100               1               2              5
# 2                20               100               1               1              10
#
#

rm(list=ls())
print("Outcome chart and expected lifetime fitness function")

FITNESS <- function(X, Wmax, Xegg, Xclutch, ax, ay) {
    W <- Wmax - sqrt(ax*(X[1] - Xegg)^2 + ay*(X[2] - Xclutch)^2)
    return(W)
}    

Wmax <- c(10, 20)
Xegg <- c(2,1)
Xclutch <- c(5,10)
ax <- 100
ay <- 1
n <- 20
x <- seq(from=0, to=3, length=n)  # egg size
y <- seq(from=0, to=30, length=n)  # clutch size
d <- expand.grid(x, y)

par(mfrow=c(2,2))

for(i in 1:2) {
    # create a vector of fitness values for all combinations
    Wtemp <- apply(d, 1, FITNESS, Wmax[i], Xegg[i], Xclutch[i], ax, ay)
    # convert to matrix
    W <- matrix(Wtemp, n, n, byrow=F)
    # plots
    contour(x, y, W, xlab='Egg size', ylab='Clutch size', las=1, lwd=3, labcex=1)
    persp(x, y, W, xlab='Egg size', ylab='Clutch size', zlab='Fitness', lwd=3, theta=50, phi=25)
}


rm(list=ls())

print("Calculating the decision matrix")
rm(list=ls())

FITNESS <- function(Egg, Clutch, X, F.vectors, Xcritical, Xmax, Xinc, Psurvival, Wmax, A, XEgg, Xclutch, Ith.Patch) {
    W <- 0
    Biomass <- Clutch * Egg
    # calculate only if less than X
    if(Biomass < X) {
        Max.Index <- 1 + (Xmax - Xcritical) / Xinc
        Index <- 1 + (Biomass - Xcritical) / Xinc
        # Get fitness at lower and upper
        Index.lower <- floor(Index)
        Index.upper <- Index.lower + 1
        Index.upper <- min(Index.upper, Max.Index)
        Qx <- Biomass - floor(Biomass)
        Fxt.lower <- F.vectors[Index.lower,2]
        Fxt.upper <- F.vectors[Index.upper,2]
        Fxt.interpolated <- Qx*Fxt.upper + (1-Qx)*Fxt.lower
        W <- Wmax[Ith.Patch] - sqrt(A[1]*(Egg - Xegg[Ith.Patch])^2 + A[2]*(Clutch - Xclutch[Ith.Patch])^2)
    }    
    return(W)
}


OVER.PATCHES <- function(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Psurvival, Wmax, A, Xegg, Xclutch, P1) {
    # X is the total biomass available
    Index <- 1 + (X - Xcritical) / Xinc
    # Allocate storage of best combinations for each patch
    # Columns contain: Fitness, Clutch, Egg
    Choice.Flag <- matrix(0, 2)
    Best.in.Patch <- matrix(0, 2, 3)
    # Iterate over patches
    for(Ith.Patch in 1:Npatch) {
        # Make a matrix, W.host of 3 cols: Fitness, Clutch size, Egg size
        W.host <- matrix(0, 11, 3)
        # Iterate over clutch size
        for(Clutch in 1:11) {
            W.host[Clutch,2] <- Clutch
            Best.egg.size <- optimize(f=FITNESS, interval=c(0.01,3), Clutch, X, F.vectors, Xcritical, Xmax, Xinc, Psurvival, Wmax, A, Xegg, Xclutch, Ith.Patch, maximum=T)
            W.host[Clutch,1] <- Best.egg.size$objective  # fitness
            W.host[Clutch,3] <- Best.egg.size$maximum  # egg size
        }
        # Get best combination for this host
        R <- W.host[,1]
        Best <- order(R, na.last=T, decreasing=T)
        Best.in.Patch[Ith.Patch,] <- W.host[Best[1],]
        # Test for several equal optimal choices
        Choice1 <- W.host[Best[1],1]
        Choice2 <- W.host[Best[2],1]
        if(Choice1 == Choice2) Choice.Flag[Ith.Patch] <- 2
    } # next host
    
    # Overall fitness
    W <- P1*Best.in.Patch[1,1] + (1-P1)*Best.in.Patch[2,1]
    F.vectors[Index,1] <- W  # get best W = F(x, t)
    # Concatenate F(x, t) and the optimal clutch values for host type 2
    Temp1 <- c(F.vectors[Index,1], 1)
    Temp2 <- c(Best.in.Patch[1,2], Best.in.Patch[1,3])
    Temp3 <- c(Best.in.Patch[2,2], Best.in.Patch[2,3])
    # Add Temp vectors to bottom of F.vectors
    Temp <- rbind(F.vectors, Temp1, Temp2, Temp3)
    
    return(Temp)
}

OVER.STATES <- function(F.vectors, Xcritical, Xmax, Xinc, Npatch, Psurvival, Max.Index, Wmax, A, Xegg, Xclutch, P1) {
    Store <- matrix(0, Max.Index, 7)
    for(Index in 2:Max.Index) {
        X <- (Index-1)*Xinc + Xcritical
        # For given X call OVER.PATCHES to determine F(x, t) and best patch
        Temp <- OVER.PATCHES(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Psurvival, Wmax, A, Xegg, Xclutch, P1)
        # Extract components
        # Last row is F(x, t) and best clutch size and egg size for host 2
        # Last row is flag that multiple equal choices exist
        n <- nrow(Temp) - 4
        F.vectors <- Temp[1:n,]
        # Add the seven output values (omit dummy) to storage
        Store[Index,] <- c(Temp[n+1,1], Temp[n+2,1:2], Temp[n+3,1:2], Temp[n+4,1:2])
    }
    # Add Store values to end of F.vectors for pass back to main program
    Temp <- cbind(F.vectors, Store)  # combined by columns
                             
    return(Temp)
}    

#### MAIN PROGRAM #####
Xmax <- 10
Xcritical <- 0
Xinc <- 1
Max.Index <- 1 + (Xmax - Xcritical) / Xinc
Wmax <- c(10, 20)
Xegg <- c(2,1)
Xclutch <- c(5,10)
A <- c(100, 1)
P1 <- 0.5  # probability of host 1
Psurvival <- 0.90
Npatch <- 2
Horizon <- 10

# Set up matrix for fitnesses. Initialize to 0
# col 1 is temporary F(x, t+1), col 2 is F(x, t+1)
F.vectors <- matrix(0, Max.Index, 2)

# create matrices for output, one for each host
FxtT <- matrix(0, Horizon, Max.Index)  # F(x, t)
Best.Clutch1 <- matrix(0, Horizon, Max.Index)
Best.Clutch2 <- matrix(0, Horizon, Max.Index)
Best.Egg1 <- matrix(0, Horizon, Max.Index)
Best.Egg2 <- matrix(0, Horizon, Max.Index)
Choice.H1 <- matrix(0, Horizon, Max.Index)
Choice.H2 <- matrix(0, Horizon, Max.Index)


# start iterations
Time <- Horizon
while(Time > 1) {
    Time <- Time - 1
    # Call OVER.STATES to get best values for this time step
    Temp <- OVER.STATES(F.vectors, Xcritical, Xmax, Xinc, Npatch, Psurvival, Max.Index, Wmax, A, Xegg, Xclutch, P1)
    # Extract F.vectors
    TempF <- Temp[,1:2]
    # Update F1
    for(J in 2:Max.Index) {
        F.vectors[J,2] <- TempF[J,1]
    }
    # Store results
    FxtT[Time,] <- Temp[,3]
    Best.Clutch1[Time,] <- Temp[,4]
    Best.Egg1[Time,] <- Temp[,5]
    Best.Clutch2[Time,] <- Temp[,6]
    Best.Egg2[Time,] <- Temp[,7]
    Choice.H1[Time,] <- Temp[,8]
    Choice.H2[Time,] <- Temp[,9]
}    

# Output information. For display add states (=wts) to last row of matrices
Index <- seq(from=1, to=Max.Index)
FxtT[Horizon,] <- (Index - 1) * Xinc + Xcritical

Best.Clutch1[Horizon,] <- (Index - 1) * Xinc + Xcritical
Best.Clutch2[Horizon,] <- (Index - 1) * Xinc + Xcritical
Best.Egg1[Horizon,] <- (Index - 1) * Xinc + Xcritical
Best.Egg2[Horizon,] <- (Index - 1) * Xinc + Xcritical
Choice.H1[Horizon,] <- (Index - 1) * Xinc + Xcritical
Choice.H2[Horizon,] <- (Index - 1) * Xinc + Xcritical

print(Best.Clutch1[,1:Max.Index])
print(Best.Clutch2[,1:Max.Index])
print(signif(Best.Egg1[,1:Max.Index], 3))
print(signif(Best.Egg2[,1:Max.Index], 3))
print(Choice.H1[,1:Max.Index])
print(Choice.H2[,1:Max.Index])
print(signif(FxtT[,1:Max.Index], 3))

