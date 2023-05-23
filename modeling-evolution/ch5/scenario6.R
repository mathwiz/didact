# Rock-Paper-Scissors mendelian model

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

# Genotypes: RR=Rock, SS=Scissors, PP=Paper. RS, RP = Rock, SP = Scissors
RR = 1
RS = 2
RP = 3
SS = 4
SP = 5
PP = 6
Genotypes = c(RR, RS, RP, SS, SP, PP)

fitness = function(morph, payoff.mat, genotypes) {
    n.pop <- sum(genotypes)
    opponent <-sample(morph)
    fit <- matrix(0, n.pop, 1)
    for(i.receiver in 1:3) {
        for(i.opponent in 1:3) {
            fit[morph==i.receiver & opponent==i.opponent] <- 5 + payoff.mat[i.receiver, i.opponent]
        }
    }
    n0 <- Genotypes[RR]          # ending row of RR
    n1 <- n0 + 1                 # starting row of RS
    n2 <- n1 + Genotypes[RS] - 1 # ending row of RS
    n3 <- n2 + 1                 # starting row of RP
    n4 <- n3 + Genotypes[RP] - 1 # ending row of RP
    r.alleles <- 2*sum(fit[1:n0]) + sum(fit[n1:n2]) + sum(fit[n3:n4])
    n5 <- n4 + 1                 # staring row of SS
    n6 <- n5 + Genotypes[SS] - 1 # ending row of SS
    n7 <- n6 + 1                 # starting row of SP
    n8 <- n7 + Genotypes[SP] - 1 # ending row of SP
    s.alleles <- 2*sum(fit[n5:n6]) + sum(fit[n1:n2]) + sum(fit[n7:n8])
    n9 <- n8 + 1                 # starting row of PP
    p.alleles <- 2*sum(fit[n9:n.pop]) + sum(fit[n3:n4]) + sum(fit[n7:n8])
    # proportion of each allele
    all.alleles <-  r.alleles + s.alleles + p.alleles
    prop.rock <- r.alleles / all.alleles 
    prop.scissors <- s.alleles / all.alleles 
    prop.paper <- p.alleles / all.alleles 
    return(c(prop.rock, prop.scissors, prop.paper))
}

genotype = function(prop.r, prop.s, prop.p, n.pop) {
    rr <- round(prop.r * 2*n.pop)
    rs <- round(2*prop.r * prop.s * n.pop)
    rp <- round(2*prop.r * prop.p * n.pop)
    ss <- round(prop.s * 2*n.pop)
    sp <- round(2*prop.s * prop.p * n.pop)
    pp <- round(prop.p * 2*n.pop)
    n <- rr + rs + rp + ss + sp + pp
    rock <- (rr + rs + rp) / n
    scissors <- (ss + sp) / n
    paper <- pp / n
    return(c(rr, rs, rp, ss, sp, pp, rock, scissors, paper))
}

isocline = function(x, add, lty) {
    # is matrix of changes in proportion
    # add=0 means start a new plot
    # lty is the line type
    result <- contourLines(Proportion, Proportion, x, levels=0)
    b <- data.frame(unlist(result))
    n.zeros <- length(b[b==0])
    for(i in 1:n.zeros) {
        prop.r <- result[[1]]$x   # why does i cause index out of bounds?
        prop.s <- result[[1]]$y   # ditto
        prop.p <- 1 - prop.r - prop.s
        rr <- prop.r^2
        rs <- 2*prop.r * prop.s
        rp <- 2*prop.r * prop.p
        p.rocks <- rr + rs + rp
        ss <- prop.s^2
        sp <- 2*prop.s * prop.p
        p.scissors <- ss + sp
        
        # plot lines if requested
        if(i==1 && add==0) {
            plot(p.rocks, p.scissors, type='l', xlab='Proportion of Rock', ylab='Proportion of Scissors', xlim=c(0,1), ylim=c(0,1), lty=lty)
        } else {
            lines(p.rocks, p.scissors, lty=lty)
        }    
    }    
    return(0)
}


#### MAIN PROGRAM ####
set.seed(100)
N.Pop = 2000
# create sequence of proportions for the two R and S alleles
N.Props = 30
N.Combos = N.Props^2
Proportion = seq(from=0, to=0.95, length=N.Props)
# all possible combinations of R and S
RxS = expand.grid(Proportion, Proportion)
Data.Rock = matrix(9, N.Props^2, 1)
Data.Scissors = matrix(9, N.Props^2, 1)
Data.Paper = matrix(9, N.Props^2, 1)
Morph = matrix(0, N.Pop, 1)
# Payoff matrix
Epsilon = 0.1    # quantification of same morph interaction
Payoff.Mat = matrix(c(-Epsilon,-1,1,1,-Epsilon,-1,-1,1,-Epsilon), 3, 3)

# iterate over all possible combinations
for(i in 1:N.Combos) {
    Prop.R = RxS[i, 1]
    Prop.S = RxS[i, 2]
    Prop.P = 1 - Prop.R - Prop.S
    Total = Prop.R + Prop.S + Prop.P
    if(Total > 0) {
        D1 = genotype(Prop.R, Prop.S, Prop.P, N.Pop)
        N.Genotypes = D1[1:6]
        Prop.Morphs = D1[7:9]
        Morph[1:N.Pop] = Paper # initialize all
        N.Rock = sum(N.Genotypes[1:3])
        N.Scissors = sum(N.Genotypes[4:5])
        N.Paper = N.Genotypes[6]
        Morph[1:N.Rock] = Rock
        N1 = N.Rock + 1
        N2 = N1 + N.Scissors - 1
        Morph[N1:N2] = Scissors
        # apply fitness to get proportions
        Propns = fitness(Morph, Payoff.Mat, N.Genotypes)
        D2 = genotype(Propns[1], Propns[2], Propns[3], N.Pop)
        Data.Rock[i] = D1[7] - D2[7]
        Data.Scissors[i] = D1[8] - D2[8]
        Data.Paper[i] = D1[9] - D2[9]
    }    
}

# convert vectors to matrices
Delta.Rock = matrix(Data.Rock, N.Props, N.Props, byrow=F)
Delta.Scissors = matrix(Data.Scissors, N.Props, N.Props, byrow=F)
Delta.Paper = matrix(Data.Paper, N.Props, N.Props, byrow=F)

# Output
par(mfrow=c(3,3))
Offset1 = 0.25
Offset2 = Offset1 * 2
Startgen = 400

# plot zero isoclines for allele frequencies
contour(Proportion, Proportion, Delta.Rock, xlab='Proportion of R', ylab='Proportion of S', levels=0, drawlabels=F)
contour(Proportion, Proportion, Delta.Scissors, lty=2, levels=0, drawlabels=F, add=T)
contour(Proportion, Proportion, Delta.Paper, lty=3, levels=0, drawlabels=F, add=T)

# get zero isoclines for proportion of each morph
isocline(Delta.Rock, 0, 1)
isocline(Delta.Scissors, 1, 2)
isocline(Delta.Paper, 1, 3)
points(1/3, 1/3, cex=2)


# Finding ESS using a numerical approach
# Reset parameters
N.Pop = 2000
Maxgen = 2000
Output = matrix(0, Maxgen, 7)
Prop.R = 0.33
Prop.S = 0.3
Prop.P = 1 - Prop.R - Prop.S
Morph = matrix(0, N.Pop, 1)
# iterate over all possible combinations
for(i in 1:N.Combos) {
    D1 = genotype(Prop.R, Prop.S, Prop.P, N.Pop)
    N.Genotypes = D1[1:6]
    Prop.Morphs = D1[7:9]
    Morph[1:N.Pop] = Paper # initialize all
    N.Rock = sum(N.Genotypes[1:3])
    N.Scissors = sum(N.Genotypes[4:5])
    N.Paper = N.Genotypes[6]
    Morph[1:N.Rock] = Rock
    N1 = N.Rock + 1
    N2 = N1 + N.Scissors - 1
    Morph[N1:N2] = Scissors
    N = sum(N.Genotypes)
    Output[i, 1] = i
    Output[i, 2] = N.Rock / N
    Output[i, 3] = N.Scissors / N
    Output[i, 4] = N.Paper / N
    Output[i, 5] = Prop.R
    Output[i, 6] = Prop.S
    Output[i, 7] = Prop.P
    
    # apply fitness to get proportions
    Propns = fitness(Morph, Payoff.Mat, N.Genotypes)
    Prop.R = Propns[1]
    Prop.S = Propns[2]
    Prop.P = Propns[3]
}


Offset1 = 0.5
Offset2 = Offset1 * 2
Startgen = 1
plot(Output[,1], Output[,2], type='l', xlab='Generation', ylab='Prop (+ offset)', ylim=c(0, 1.8))
lines(Output[,1], Output[,3] + Offset1, lty=2)
lines(Output[,1], Output[,4] + Offset2, lty=3)
# add lines for expected values
lines(Output[,1], rep(1/3, Maxgen))
lines(Output[,1], rep(1/3 + Offset1, Maxgen))
lines(Output[,1], rep(1/3 + Offset2, Maxgen))

# phase plots of pairwise morphs
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,3], type='l', xlab='Rock', ylab='Scissors')
plot(Output[Startgen:Maxgen,2], Output[Startgen:Maxgen,4], type='l', xlab='Rock', ylab='Paper')
plot(Output[Startgen:Maxgen,3], Output[Startgen:Maxgen,4], type='l', xlab='Scissors', ylab='Paper')
Startgen = 400
print(c('Mean Proportions (R, S, P) from Generation', Startgen, 'to', Maxgen))
print(c(mean(Output[Startgen:Maxgen, 2]), mean(Output[Startgen:Maxgen, 3]), mean(Output[Startgen:Maxgen, 4])))
print(c('Mean Allele Frequencies (R, S, P) from Generation', Startgen, 'to', Maxgen))
print(c(mean(Output[Startgen:Maxgen, 5]), mean(Output[Startgen:Maxgen, 6]), mean(Output[Startgen:Maxgen, 7])))
