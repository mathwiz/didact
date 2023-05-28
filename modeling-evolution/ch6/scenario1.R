# Patch foraging model

# General assumptions
# 1. The habitat is divided into a number of patches.
# 2. The animal in question forages among the patches for food or some other fitness-related resource.
# 3. Patches vary in quality and hence the benefits obtained by the animal vary with patch type.
# 4. There is a metabolic cost to foraging.
# 5. Independent of the metabolic cost there is a possibility of dying from some external cause such as a predator or inclement weather conditions. The probability may vary among patches.
# 6. 2 Options
#    a. Fitness is measured as the probability of being alive at the end of some time interval (e.g. in a small territorial vertebrate this may be the summer period, the end of the season being the time at which hibernation is entered).
#    b. Fitness is a function of the state variable at the end of some time interval (e.g. in a small terrestrial vertebrate this may be the summer period, the end of the season being the time at which hibernation is entered, or ti could be the reproductive episode).
#
# Mathematical assumptions
# 1. There are three types of patches, which vary with respect to
#    a. Benefits of the resource, Benefit(i), if found
#    b. Probability of obtaining the benefit (e.g. food), Pbenefit(i)
#    c. Probability of mortality, Pmortality(i)
# 2. The cost to foraging, Cost, is fixed and independent of patch type.
# 3. There is a minimum state, Xcritical, at or below which the animal cannot survive. The minimum state at which an animal can survive is Xmin.
# 4. There is a limit to the state (e.g. space for storing the resource is limiting), Xmax.
# 5. Fitness is equal to the maximum probability of surviving until time T, the end of the season (or some other designated point).
#
#                                Model
# Patch Type      Benefit       Pbenefit       Pmortality       Cost
#      1            0             0.0            0.000           1
#      2            3             0.4            0.004           1
#      1            5             0.6            0.020           1
#

rm(list=ls())

FITNESS <- function(X, Xcritical, Xmax, Xmin, Cost, Benefit, Pbenefit, Pmortality, F.vectors) {
    # State in patch if forager finds food
    X.Food <- X - Cost + Benefit
    # If X.Food greater than Xmax then X.Food must be set to Xmax
    X.Food <- min(X.Food, Xmax)
    # If X.Food less than or equal to Xcritical then set to Xcritical
    X.Food <- max(X.Food, Xcritical)
    # State in patch if forager does not find food
    X.NoFood <- X - Cost
    # If X.NoFood is less than Xcritical set X.NoFood to Xcritical
    X.NoFood <- max(X.NoFood, Xcritical)
    Term1 <- Pbenefit * F.vectors[X.Food,2]  # if food is found
    Term2 <- (1 - Pbenefit) * F.vectors[X.NoFood,2]  # if food is not found
    W <- (1 - Pmortality) * (Term1 + Term2)
    
    return(W)
}

OVER.PATCHES <- function(X, F.vectors, Xcritical, Xmax, Xmin, Npatch, Cost, Benefit, Pbenefit, Pmortality) {
    for(i in 1:Npatch) {
        RHS[i] <- FITNESS(X, Xcritical, Xmax, Xmin, Cost, Benefit[i], Pbenefit[i], Pmortality[i], F.vectors)
    }
    # Now find optimal patch. Best row is in Best[1]
    Best <- order(RHS, na.last=T, decreasing=T)
    F.vectors[X,1] <- RHS[Best[1]]
    Best.Patch <- Best[1]
    # Concatenate F(x, t) and the optimal patch number
    Temp <- c(F.vectors[X,1], Best.Patch)
    # Add Temp to bottom of F.vectors and rename to Temp
    Temp <- rbind(F.vectors, Temp)
    
    return(Temp)
}

OVER.STATES <- function(F.vectors, Xcritical, Xmax, Xmin, Npatch, Cost, Benefit, Pbenefit, Pmortality) {
    Store <- matrix(0, Xmax, 2)
    for(X in Xmin:Xmax) {
        Temp <- OVER.PATCHES(X, F.vectors, Xcritical, Xmax, Xmin, Npatch, Cost, Benefit, Pbenefit, Pmortality)
        # Extract components. Last row is F(x, t) and best patch
        n <- nrow(Temp) - 1
        F.vectors <- Temp[1:n,]
        Store[X,] <- Temp[n+1,]
    }
    # Add Store values to end of F.vectors for pass back to main program
    Temp <- cbind(F.vectors, Store)  # combined by columns
                             
    return(Temp)
}    

#### MAIN PROGRAM #####
Xmax <- 10
Xcritical <- 3
Xmin <- Xcritical + 1
Cost <- 1
Pmortality <- c(0, 0.004, 0.02)
Pbenefit <- c(1, 0.4, 0.6)
Benefit <- c(0, 3, 5)
Npatch <- 3
Horizon <- 20
# pre-allocate RHS of equation
RHS <- matrix(0, Npatch, 1)   # when is this ever used?

# set up matrix for fitnesses
# col 1 is F(x, t), col 2 is F(x, t+1)
F.vectors <- matrix(0, Xmax, 2)

# use fitness option a
# F.vectors[Xmin:Xmax,2] <- 1
# use fitness option b
 F.vectors[Xmin:Xmax,2] <- seq(from=Xmin, to=Xmax)

# create matrices for output
FxtT <- matrix(0, Horizon, Xmax)  # F(x, t)
Best.Patch <- matrix(0, Horizon, Xmax)

# start iterations
Time <- Horizon
while(Time > 1) {
    Time <- Time - 1
    # Call OVER.STATES to get best values for this time step
    Temp <- OVER.STATES(F.vectors, Xcritical, Xmax, Xmin, Npatch, Cost, Benefit, Pbenefit, Pmortality)
    # Extract F.vectors
    TempF <- Temp[,1:2]
    # Update F1
    for(J in Xmin:Xmax) {
        F.vectors[J,2] <- TempF[J,1]
    }
    # Store results
    Best.Patch[Time,] <- Temp[,4]
    FxtT[Time,] <- Temp[,3]
}    

# Output information. For display add states (=wts) to last row of matrices
X <- seq(from=1, to=Xmax)
Best.Patch[Horizon,] <- X
FxtT[Horizon,] <- X
print(Best.Patch[,Xmin:Xmax])
print(signif(FxtT[,Xmin:Xmax], 3))


print("Using the decision matrix: individual prediction")
set.seed(10)
Horizon <- 15
Output <- matrix(0, Horizon, 10)
Time <- seq(1, Horizon)

par(mfrow=c(5,2))

for(Replicate in 1:10) {
    X <- 4
    for(i in 1:Horizon) {
        if(X > Xcritical) {
            Patch <- Best.Patch[i,X]
            # Check if animal survives predation
            if(runif(1) < Pmortality[Patch]) print("Dead from predator")
            # Find new weight
            # set multiplier to zero, corresponding to no food
            Index <- 0
            if(runif(1) < Pbenefit[Patch]) Index <- 1  # food discovered
            X <- X - Cost + Benefit[Patch]*Index
            # If X greater than Xmax then X must be set to Xmax
            X <- min(X, Xmax)
            # If X less than X then animal dies
            if(X < Xmin) print("Dead from starvation")
            Output[i, Replicate] <- Patch
        }
    }
    # end of time loop
    print(paste("Plotting Replicate", Replicate))
    plot(Time, Output[,Replicate], type='l', ylab="Patch selected")
 }   


print("Using the decision matrix: expected state')
