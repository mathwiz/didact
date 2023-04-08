rm(list=ls())

Mj = 0.05
Ma = 0.40
Age.Max = 30
Fecundity = 0.05
A = 10 # Wt increase/annum without reproduction


fitness = function(x) {
    fitness2(x[1], x[2])
}    

fitness2 = function(g, alpha) {
    alpha.minus.1 <- alpha - 1
    surv <- matrix(0, Age.Max)
    # growth prior to reproduction is linear
    wt <- matrix(0, Age.Max, 1)
    surv[1] <- exp(-Mj)
    
    for(i in 2:alpha.minus.1) {
        wt[i] <- wt[i-1] + A
        surv[i] <- surv[i-1]*exp(-Mj)
    }
    # now for age alpha to max
    for(i in alpha:Age.Max) {
        wt[i] <- wt[i-1] + A - g*wt[i-1]
        surv[i] <- surv[i-1]*exp(-(Mj + Ma*g))
    }
    surv.term <- sum(surv[alpha:Age.Max] * wt[alpha:Age.Max] * g)
    w <- Fecundity * surv.term
    return(-w)
}    


#### Main program ####

N.G = 11
G = seq(from=0.01, to=0.50, length=N.G)
Alpha = seq(from=3, to=20)
N.Alpha = length(Alpha)
D = expand.grid(G, Alpha)
W = apply(D, 1, fitness)
W.Mat = matrix(W, N.G, N.Alpha)

# finding maximum numerically
G.Num = matrix(0, N.Alpha)
W.Num = matrix(0, N.Alpha)
for(i in 1:N.Alpha) {
    G.Num[i] = nlm(fitness2, p=0.2, Alpha[i])$estimate
    W.Num[i] = -1 * fitness2(G.Num[i], Alpha[i])
}
Best = order(W.Num, na.last=T, decreasing=T)
Alpha.Best = Alpha[Best[1]]
G.Best = G.Num[Best[1]]
W.Best = W.Num[Best[1]]

# brute force approach
best.g = function(alpha) {
    g1 <- nlm(fitness2, p=0.1, alpha)$estimate
    w1 <- fitness2(g1, alpha)
    g2 <- nlm(fitness2, p=0.1, alpha+1)$estimate
    w2 <- fitness2(g2, alpha+1)
    wdiff <- w1 - w2
    return(c(wdiff, w1, g1))
}
Alpha.Brute = 5
Diff = best.g(Alpha.Brute)
while(Diff[1] > 0) { # if Diff[1] > 0 then W is still increasing
    Alpha.Brute = Alpha.Brute + 1
    Diff = best.g(Alpha.Brute) #Diff[3] = G, Diff[2] = -W
}


print('recursion-brute-force done.')

output = function() {
    par(mfrow=c(1,1))
    contour(G, Alpha, -W.Mat, xlab='G', ylab='Alpha')
    print('Maximum numerically')
    print(c(Alpha.Best, G.Best, W.Best))
    print('Brute force approach')
    print(c(Alpha.Brute, Diff[3], -Diff[2]))
}    
