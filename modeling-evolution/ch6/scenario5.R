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


OVER.PATCHES <- function(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Benefit, Pbenefit, Psurvival) {
    # create matrix for storing best clutch size for each host type
    Best.Clutch <- matrix(0, Npatch)
    Index <- 1 + (X - Xcritical) / Xinc
    # vector of clutch sizes to Index-1
    Clutch <- seq(from=1, to=Index-1)
    # Start fitness accumulation with component for case of not finding a host
    W <- Psurvival * (1 - sum(Pbenefit)) * F.vectors[Index,2]
    for(i in 1:Npatch) {
        # calculate "partial" fitness, W.partial for each clutch size
        W.partial <- Benefit[2:Index,i] + Psurvival*F.vectors[Index-Clutch,2]
        # find largest W.partial and hence best clutch size
        Best <- order(W.partial, na.last=T, decreasing=T)
        Best.Clutch[i] <- Best[1]  # store best clutch for host i
        W <- W + Pbenefit[i] * W.partial[Best[1]]
        # test for several equal optimal choices
        # only examine W.partial that contain more than one entry
        if(length(W.partial) > 1 & W.partial[Best[1]]==W.partial[Best[2]]) {
            print("Several possible equal choices")
        }    
    }

    F.vectors[Index,1] <- W  # get best W = F(x, t)
    # Concatenate F(x, t) and the optimal clutch values for host type 2
    Temp <- c(F.vectors[Index,1], Best.Clutch[2])
    # Add Temp to bottom of F.vectors and rename to Temp
    Temp <- rbind(F.vectors, Temp)
    
    return(Temp)
}

OVER.STATES <- function(F.vectors, Xcritical, Xmax, Xinc, Npatch, Benefit, Pbenefit, Psurvival, Max.Index) {
    Store <- matrix(0, Max.Index, 2)
    for(Index in 2:Max.Index) {
        X <- (Index-1)*Xinc + Xcritical
        # For given X call OVER.PATCHES to determine F(x, t) and best patch
        Temp <- OVER.PATCHES(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Benefit, Pbenefit, Psurvival)
        # Extract components. Last row is F(x, t) and best clutch size for host 2
        n <- nrow(Temp) - 1
        F.vectors <- Temp[1:n,]
        Store[Index,] <- Temp[n+1,]  # save F(x, t, T) and best patch
    }
    # Add Store values to end of F.vectors for pass back to main program
    Temp <- cbind(F.vectors, Store)  # combined by columns
                             
    return(Temp)
}    

#### MAIN PROGRAM #####
Xmax <- 40
Xcritical <- 0
Xinc <- 1
Max.Index <- 1 + (Xmax - Xcritical) / Xinc
Psurvival <- 0.99
Npatch <- 4
# Create host coefficient matrix from which to get Benefits
Host.coeff <- matrix(0, 4, 4)
Host.coeff[1,] <- c(-0.2302, 2.7021, -0.2044, 0.0039)
Host.coeff[2,] <- c(-0.1444, 2.2997, -0.1170, 0.0013)
Host.coeff[3,] <- c(-0.1048, 2.2097, -0.0878, 0.0004222)
Host.coeff[4,] <- c(-0.0524, 2.0394, -0.0339, 0.0003111)
# Calculate benefit as a function of clutch size
# Rows = clutch size, Cols = host type
Clutch <- seq(from=0, to=Xmax)
Benefit <- matrix(0, Xmax+1, 4)
# Iterate over host types
for(I.Host in 1:4) {
    acc <- 0
    acc <- acc + Host.coeff[I.Host,1]
    acc <- acc + Host.coeff[I.Host,2]*Clutch
    acc <- acc + Host.coeff[I.Host,3]*Clutch^2
    acc <- acc + Host.coeff[I.Host,4]*Clutch^3
    Benefit[,I.Host] <- acc
}

Benefit[1,] <- 0  # reset first row to 0
SHM <- c(9, 12, 14, 23)  # Single host maximum
# Make all values > SHM equal 0. Note we use 2 because of zero class
for(i in 1:4) { Benefit[(SHM[i]+2):Max.Index,i] <- 0 }
# Probability of encountering host type
Pbenefit <- c(0.05, 0.05, 0.10, 0.80)
Horizon <-21

# set up matrix for fitnesses
# col 1 is F(x, t), col 2 is F(x, t+1)
F.vectors <- matrix(0, Max.Index, 2)

# create matrices for output
FxtT <- matrix(0, Horizon, Max.Index)  # F(x, t, T)
Best.Patch <- matrix(0, Horizon, Max.Index)

# start iterations
Time <- Horizon
while(Time > 1) {
    Time <- Time - 1
    # Call OVER.STATES to get best values for this time step
    Temp <- OVER.STATES(F.vectors, Xcritical, Xmax, Xinc, Npatch, Benefit, Pbenefit, Psurvival, Max.Index)
    # Extract F.vectors
    TempF <- Temp[,1:2]
    # Update F1
    for(J in 2:Max.Index) {
        F.vectors[J,2] <- TempF[J,1]
    }
    # Store results
    Best.Patch[Time,] <- Temp[,4]
    FxtT[Time,] <- Temp[,3]
}    

# Output information. For display add states (=wts) to last row of matrices
Index <- seq(from=1, to=Max.Index)
Best.Patch[Horizon,] <- (Index - 1) * Xinc + Xcritical
FxtT[Horizon,] <- (Index - 1) * Xinc + Xcritical

print("Decision matrix, Fxt of Decision, and Matrix of Choices")
print(Best.Patch[,1:Max.Index])
print(signif(FxtT[,1:Max.Index], 3))

# Plots
y <- Best.Patch[Horizon,2:Max.Index]
x <- seq(from=1, to=Horizon-1)

par(mfrow=c(1,2))

persp(x, y, Best.Patch[1:(Horizon-1),2:Max.Index], xlab='Time', ylab='x', zlab='Optimum', theta=20, ph=25, lwd=1)
image(x, y, Best.Patch[1:(Horizon-1),2:Max.Index], col=terrain.colors(50), xlab='Time', ylab='x', las=1)

# Output data
DATA <- cbind(x, Best.Patch[1:(Horizon-1),41])
DATA <- t(DATA)
write(DATA, file="Oviposition.dat", nc=2)

# write data files for each host
for(i in 1:4) {
    filename <- paste("DM", i, ".dat", sep="")
    print(paste("Writing file:", filename))
    DATA <- t(Best.Patch[1:(Horizon-1),2:41])
    write(DATA, file=filename, nc=40)
}          


