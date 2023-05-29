# Testing for equivalent choices, indexing, and interpolation

# General assumptions
# 1. The animal can hunt singly or in packs, changing pack size at each time increment.
# 2. Each animal has a daily food requirement.
# 3. An animal whose gut contents fall below a critical value.
# 4. Food within the pack is shared equally.
# 5. Capture probability increases with the size of the pack (possibly to a limit).
# 6. Fitness is a function of the probability of surving some specified time interval.
#
# Mathematical assumptions
# 1. Pack (patch) size varies from 1 to N.
# 2. Each prey animal is of a fixed size Y.
# 3. The daily food requirement per individual is the same, irrespective of pack size.
# 4. The probability of making a kill, p(i), is a function of pack size, i.
# 5. Up to three kills can be made per day: designate the number of kills as k, where k = 0, 1, 2, and 3.
# 6. The probability of each kill is independent and hence the number of kills perl day is a binomial variable where Z is the amount per individual per day.
# 7. An animal whose gut contents fall to or below Xcritical is dead.
# 8. Gut capacity has a maximum value of Xmax.
# 9. Fitness is the survival probability to the end of some specified time interval, T.
#
#                                Model
#
# Kill Made     x > Xcritical   Survives                 X
# Yes               -             Yes       min(x - Cost + Benefit(i,k), Xmax)
# No               Yes            Yes           x - Cost  
# No               Yes            No            Xcritical
# No               No             NA              NA
#
#

rm(list=ls())

# Rows = pack size, Columns = number of kills + 1
Benefit <- matrix(0, 4, 4)  
Pbenefit <- matrix(0, 4, 4)
# Probability of single kill for pack size
Pi <- c(0.15, 0.31, 0.33, 0.33)
Y <- 11.25  # size of a single prey
k <- c(0,1,2,3)  # number of kills
for(PackSize in 1:4) {
    # Calculate binomial probabilities using dbinom
    Pbenefit[PackSize,] <- dbinom(x=k, size=3, prob=Pi[PackSize])
    # Calculate benefits
    Benefit[PackSize,2:4] <- k[2:4]*Y / PackSize
}

print("Benefits and probability of benefits as a function of pack size")
print(Benefit)
print(Pbenefit)


print("Calculating the decision matrix")
rm(list=ls())


FITNESS <- function(X, Xcritical, Xmax, Xinc, Cost, Benefit, Pbenefit, F.vectors) {
    # Note that the state value X is passed
    # Note that Benefit and Pbenefit are vectors
    # Iterate over the four kill values (0,1,2,3)
    Max.Index <- 1 + (Xmax - Xcritical) / Xinc
    W <- 0
    Xstore <- X  # set X to Xstore to preserve value through loop
    for(I.Kill in 1:4) {
        # New state value
        X <- Xstore - Cost + Benefit[I.Kill]
        # If X greater than Xmax, set to Xmax
        X <- min(X, Xmax)
        # If X less than or equal to Xcritial, set to Xcritical
        X <- max(X, Xcritical)
        # Convert to Index value
        Index <- 1 - (X - Xcritical) / Xinc
        # Index value probably not an integer
        Index.lower <- floor(Index)  # choose lower integer
        Index.upper <- Index.lower + 1  # choose upper integer
        # Must stop index exceeding Max.Index. Note that Qx=0 in this case
        Index.upper <- min(Index.upper, Max.Index)
        Qx <- X - floor(X)  # linear interpolation
        print(I.Kill)
        Interpolation <- (Qx*F.vectors[Index.upper,2] + (1-Qx)*F.vectors[Index.lower,2])
        W <- W + Pbenefit[I.Kill] * Interpolation
    }


    return(W)
}

OVER.PATCHES <- function(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Cost, Benefit, Pbenefit) {
    RHS <- matrix(0, Npatch, 1)
    for(i in 1:Npatch) {
        RHS[i] <- FITNESS(X, Xcritical, Xmax, Xinc, Cost, Benefit[i,], Pbenefit[i,],  F.vectors)
    }
    # Now find optimal patch. Best row is in Best[1]
    Best <- order(RHS, na.last=T, decreasing=T)
    Index <- 1 + (X - Xcritical) / Xinc
    F.vectors[Index,1] <- RHS[Best[1]]  # get best W = F(x, t, T)
    # Get best patch (=pack). Remember to convert from index value
    Best.Patch <- Best[1]
    # Concatenate F(x, t) and the optimal patch number
    Temp <- c(F.vectors[Index,1], Best.Patch)
    # Add Temp to bottom of F.vectors and rename to Temp
    Temp <- rbind(F.vectors, Temp)
    # Create 1x2 vector to hold decision on more than one choice
    # We only need one cell but it is convenient to use 2 for concatenation into Temp
    # Set Choice to zero
    Choice <- c(0,0)
    if(RHS[Best[1]] == RHS[Best[2]]) Choice <- c(1,1)
    Temp <- rbind(Temp, Choice)
    
    return(Temp)
}

OVER.STATES <- function(F.vectors, Xcritical, Xmax, Xinc, Npatch, Cost, Benefit, Pbenefit, Max.Index) {
    Store <- matrix(0, Max.Index, 3)
    for(Index in 2:Max.Index) {
        X <- (Index-1)*Xinc + Xcritical
        # For given X call OVER.PATCHES to determine F(x, t) and best patch
        Temp <- OVER.PATCHES(X, F.vectors, Xcritical, Xmax, Xinc, Npatch, Cost, Benefit, Pbenefit)
        # Extract components. Penultimate row is F(x, t, T) and best patch
        n <- nrow(Temp) - 2
        F.vectors <- Temp[1:n,]
        Store[Index,1:2] <- Temp[n+1,]  # save F(x, t, T) and best patch
        Store[Index,3] <- Temp[n+2,1]  # save flag for several choices
    }
    # Add Store values to end of F.vectors for pass back to main program
    Temp <- cbind(F.vectors, Store)  # combined by columns
                             
    return(Temp)
}    

#### MAIN PROGRAM #####
Xmax <- 30
Xcritical <- 0
Xinc <- 1
Max.Index <- 1 + (Xmax - Xcritical) / Xinc
Cost <- 6
Npatch <- 4  # number of packs
# Rows = pack size, Cols = number of kills + 1
Benefit <- matrix(0, 4, 4)
Pbenefit <- matrix(0, 4, 4)
# Probability of single kill for pack size
Pi <- c(0.15, 0.31, 0.33, 0.33)
Y <- 11.25  # size of a single prey
k <- c(0,1,2,3)  # number of kills
# Iterate over pack sizes
for(PackSize in 1:4) {
    # Calculate binomial probabilities using dbinom
    Pbenefit[PackSize,] <- dbinom(x=k, size=3, prob=Pi[PackSize])
    # Calculate benefits
    Benefit[PackSize,2:4] <- k[2:4]*Y / PackSize
}

Horizon <-31

# set up matrix for fitnesses
# col 1 is F(x, t), col 2 is F(x, t+1)
F.vectors <- matrix(0, Max.Index, 2)
F.vectors[2:Max.Index,2] <- 1  # Cell 1,2 = 0 = Dead

# create matrices for output
FxtT <- matrix(0, Horizon, Max.Index)  # F(x, t, T)
Best.Patch <- matrix(0, Horizon, Max.Index)

# matrix for flag indicating multiple equivalent choices
# 0 = only one choice, 1 = more than one choice
CHOICES <- matrix(0, Horizon, Max.Index)

# start iterations
Time <- Horizon
while(Time > 1) {
    Time <- Time - 1
    # Call OVER.STATES to get best values for this time step
    Temp <- OVER.STATES(F.vectors, Xcritical, Xmax, Xinc, Npatch, Cost, Benefit, Pbenefit, Max.Index)
    # Extract F.vectors
    TempF <- Temp[,1:2]
    # Update F1
    for(J in Xmin:Xmax) {
        F.vectors[J,2] <- TempF[J,1]
    }
    # Store results
    Best.Patch[Time,] <- Temp[,4]
    FxtT[Time,] <- Temp[,3]
    CHOICES[Time,] <- Temp[,5]
}    

# Output information. For display add states (=wts) to last row of matrices
Index <- seq(from=1, to=Max.Index)
Best.Patch[Horizon,] <- (Index - 1) * Inc + Xcritical
FxtT[Horizon,] <- (Index - 1) * Inc + Xcritical

print("Decision matrix, Fxt of Decision, and Matrix of Choices")
print(Best.Patch[,1:Max.Index])
print(signif(FxtT[,1:Max.Index], 3))
print(CHOICES[,1:Max.Index])

# Plots
y <- Best.Patch[Horizon,2:Max.Index]
x <- seq(from=1, to=Horizon-1)

par(mfrow=c(2,2))

persp(x, y, Best.Patch[1:30,2:Max.Index], xlab='Time', ylab='x = Gut contents', zlab='Optimal pack size', theta=20, ph=25, lwd=1)

image(x, y, Best.Patch[1:30,2:Max.Index], col=terrain.colors(50), xlab='Time', ylab='x = Gut contents', las=1)



