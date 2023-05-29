# To forage or not to forage when patches become options. A bird that must gather resources during a given period to survive the following period.

# General assumptions
# 1. During the day the bird can decide to forage or remain sedentary during any given period.
# 2. A non-foraging bird risks losing condition.
# 3. A foraging bird faces a predation risk that is a function of its condition, well-fed birds being more susceptible.
# 4. Fitness is a function of condition at the end of period (day).
#
# Mathematical assumptions
# 1. The condition of a bird ranges in unit increments from 1 to 7.
# 2. A bird in condition 1 is dead.
# 3. A non-foraging bird bird losing one unit of condition with some fixed probability, Ploss.
# 4. A foraging bird gains one unit of condition with some fixed probability, Pgain.
# 5. A foraging bird suffers a mortality probability from predators at a rate that is a linear function condition: Pmortality = a + bx, where mortality at x = 2 is 0 and Pmax at x = Xmax, the constants a and b being determined accordingly.
# 6. A non-foraging bird is not susceptible to predators.
# 7. Fitness is the expected state value (i.e. condition) at the end of the day.
#
#                                Model
#
# Patch Type      Benefit       Pbenefit       Pmortality       Cost
# Not forage        -1           Ploss             0             NA
# Forage             1           Pgain           a + bx          NA
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
    RHS <- matrix(0, Npatch, 1)
    # Pmortality is now also indexed by X
    for(i in 1:Npatch) {
        RHS[i] <- FITNESS(X, Xcritical, Xmax, Xmin, Cost, Benefit[i], Pbenefit[i], Pmortality[i,X], F.vectors)
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
Xmax <- 7
Xcritical <- 1
Xmin <- Xcritical + 1
Cost <- 0.0
# Probability of mortality if foraging
Pmin <- 0.0
Pmax <- 0.01
# Probability of mortality if not foraging
Pnoforage <- rep(0, Xmax)
# Probability of mortality if foraging
Pforage <- c(0, seq(from=Pmin, to=Pmax, length=Xmax-1))
Pmortality <- rbind(Pnoforage, Pforage)  # Mortality function
# Probability of foraging
Pbenefit <- c(0.4, 0.8)
Benefit <- c(-1, 1)
Npatch <- 2
Horizon <- 6

# set up matrix for fitnesses
# col 1 is F(x, t), col 2 is F(x, t+1)
F.vectors <- matrix(0, Xmax, 2)
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

