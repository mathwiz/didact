# Rock-Paper-Scissors clonal model

# General Assumptions
# 1. The population consists of three behavioral clones: Rock, Paper, Scissors
# 2. The payoffs are symmetrical.
# 3. Fitness is equal to some initial quantity plus payoff.

# Payoff Matrix
#                       Opponent
# Self       Rock        Paper       Scissors
# Rock       -epsilon    -1           1 
# Paper       1          -epsilon    -1
# Scissors   -1          1           -epsilon


rm(list=ls())

# Code: Rock=1, Scissors=2, Paper=3
Rock = 1
Scissors = 2
Paper = 3

fitness = function(morph, payoff.mat, n.pop) {
    opponent <-sample(morph)
    fit <- matrix(0, n.pop, 1)
    for(i.receiver in 1:3) {
        for(i.opponent in 1:3) {
            fit[morph==i.receiver & opponent==i.opponent] <- 5 + payoff.mat[i.receiver, i.opponent]
        }
    }
    prop.rock <- sum(fit[morph==Rock]) / sum(fit)
    prop.scissors <- sum(fit[morph==Scissors]) / sum(fit)
    prop.paper <- sum(fit[morph==Paper]) / sum(fit)
    return(c(prop.rock, prop.scissors, prop.paper))
}    

#### MAIN PROGRAM ####
set.seed(100)
N.Pop = 1000
Maxgen = 1000
Output = matrix(0, Maxgen, 4)
Prop.Rock = 0.33
N.Rock = round(Prop.Rock * N.Pop)
Prop.Scissors = 0.33
N.Scissors = round(Prop.Scissors * N.Pop)
# Set up morph vector initially with all Paper
Morph = matrix(Paper, N.Pop, 1)
# Payoff matrix
Epsilon = 0.1    # quantification of same morph interaction
Payoff.Mat = matrix(c(-Epsilon,-1,1,1,-Epsilon,-1,-1,1,-Epsilon), 3, 3)

for(i in 1:Maxgen) {
    Output[i, 1] = i
    # reset Morph to all Paper
    Morph[1:N.Pop] = Paper
    # do the Rocks first
    Morph[1:N.Rock] = Rock
    # separator indices
    n1 = N.Rock + 1
    n2 = n1 + N.Scissors
    Morph[n1:n2] = Scissors
    N.Rock = length(Morph[Morph==Rock])
    N.Scissors = length(Morph[Morph==Scissors])
    N.Paper = length(Morph[Morph==Paper])
    Output[i, 2] = N.Rock / N.Pop
    Output[i, 3] = N.Scissors / N.Pop
    Output[i, 4] = N.Paper / N.Pop
    # calculate next iteration
    Props = fitness(Morph, Payoff.Mat, N.Pop)
    N.Rock = round(N.Pop * Props[Rock])
    N.Scissors = round(N.Pop * Props[Scissors])
}    

# Output
par(mfrow=c(2,2))
Offset1 = 0.25
Offset2 = Offset1 * 2
Startgen = 400

plot(Output[,1], Output[,2], type='l', xlab='Generation', ylab='Prop (+ offset)', ylim=c(0, 1))
lines(Output[,1], Output[,3] + Offset1)
lines(Output[,1], Output[,4] + Offset2)
# add lines for expected values
lines(Output[,1], rep(1/3, Maxgen))
lines(Output[,1], rep(1/3 + Offset1, Maxgen))
lines(Output[,1], rep(1/3 + Offset2, Maxgen))
# phase plots of pairwise morphs
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,3], type='l', xlab='Rock', ylab='Scissors')
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,4], type='l', xlab='Rock', ylab='Paper')
plot(Output[Startgen:Maxgen,3], Output[Startgen:Maxgen,4], type='l', xlab='Scissors', ylab='Paper')
print(c('Mean Proportions (R, S, P) from Generation', Startgen, 'to', Maxgen))
print(c(mean(Output[Startgen:Maxgen, 2]), mean(Output[Startgen:Maxgen, 3]), mean(Output[Startgen:Maxgen, 4])))

