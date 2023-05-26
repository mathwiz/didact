# ESS Learned by Individuals

# General Assumptions
# 1. The population consists of individuals that adopt Hawk or Dove with a probability contingent on previous experience.
# 2. The payoff matrix has absolute values are not constrained by formula. The Dove interacting with Hawk may have some positive payoff. Dove interacting with Dove receives an intermediate value between D-H and H-D (rather than V/2).
# 3. Fitness equals payoff.

# Payoff Matrix
#                       Opponent
# Self       Hawk        Dove 
# Hawk       0           18
# Dove       12          15


rm(list=ls())
set.seed(100)


morph = function(p1.t, n.pop) {
    # Code:
    H=1; D=2
    m <- rep(D, n.pop)
    flag <- runif(n.pop, min=0, max=1)
    m[p1.t > flag] <- H  # set random elements to Hawk
    return(m)
}

fitness.func = function(morph, payoff.matrix, n.pop) {
    opponent <- sample(morph)
    # iterate over payoffs
    fit <- rep(0, n.pop)
    for(i.receiver in 1:2) {
        for(i.opponent in 1:2) {
            fit[morph==i.receiver & opponent==i.opponent] <- payoff.matrix[i.receiver, i.opponent]
        }
    }    
    return(fit)
}    

#### MAIN PROGRAM ####
N.Pop = 30
Max.Trial = 200
R = 14.4  # Residual
M = 0.99  # Memory coefficient

Output = matrix(0.0, Max.Trial, N.Pop + 1)
Pi.T = matrix(0.5, N.Pop, 1)  # vector of learned probabiliites
Mean.Pi.T = matrix(0.0, Max.Trial, 1)  

# payoff matrix
# Dim 1=Trial num, Dim 2= Behavior, Dim 3=Individual
Payoff = array(0.0, c(Max.Trial, 2, N.Pop))
Payoff.Mat = matrix(c(0, 12, 18, 15), 2, 2)

# execute first trial
Output[1,1] = 1  
Output[1,2:(N.Pop + 1)] = Pi.T
Morph = morph(Pi.T, N.Pop)
Fitness = fitness.func(Morph, Payoff.Mat, N.Pop)

# run the trials
for(trial in 2:Max.Trial) {
    Morph = morph(Pi.T, N.Pop)
    Fitness = fitness.func(Morph, Payoff.Mat, N.Pop)
    MaxT = trial - 1
    for(i in 1:N.Pop) {
        Hawk = 0; Dove = 0
        for(time in 1:MaxT) {
            Hawk = Hawk + M^(trial - time - 1) * Payoff[time, 1, i]
            Dove = Dove + M^(trial - time - 1) * Payoff[time, 2, i]
        }
        Pi.T[i] = (R + Hawk) / (2*R + Hawk + Dove)
    }  # done with all individuals
    
    # store data
    Output[trial,1] = trial 
    Output[trial,2:(N.Pop+1)] = Pi.T  # probabilities for individuals
    Mean.Pi.T[trial] = mean(Pi.T)
}    

par(mfrow=c(1,1))
# plot first individual
plot(Output[,1], Output[,2], type='l', xlab='Trial', ylab=' Hawk Pi.T', ylim=c(0.0, 1.0))
for(i in 3:(N.Pop+1)) {
    lines(Output[,1], Output[,i], type='l') 
}
# plot mean
points(Output[,1], Mean.Pi.T)


