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


## # finding maximum numerically
## Max.Num <- nlm(fitness, p=c(0.1, 0.1))$estimate
## X1 = Max.Num[1]
## X2 = Max.Num[2]
## X3 = (R - N*(X1 + X2)) / N

## # brute force approach
## Brute.Size = 100
## Brute.X = seq(from=1.0, to=5.0, length=Brute.Size)
## Brute.Y = seq(from=1.0, to=5.0, length=Brute.Size)
## Brute.D = expand.grid(Brute.X, Brute.Y)
## Brute.W = apply(Brute.D, 1, function(x) { -fitness(x) })
## # find position in row with highest fitness
## Best = order(Brute.W, na.last=T, decreasing=T)
## Brute.X1 = Brute.D[Best[1],1]
## Brute.X2 = Brute.D[Best[1],2]
## Brute.X3 = (R - N*(Brute.X1 + Brute.X2)) / N


print('recursion-brute-force done.')

output = function() {
    par(mfrow=c(1,1))
    contour(G, Alpha, -W.Mat, xlab='G', ylab='Alpha')
    ## persp(X, X, W, xlab='Propagule size 1st clutch', ylab='Popagule size 2nd clutch', zlab='Fitness', theta=25, phi=25, lwd=1)
    ## print('Maximum using calculus')
    ## print(Max.Symb)
    ## print('Maximum using calculus (alternate method)')
    ## print(Max.Symb2)
    ## print('Maximum numerically')
    ## print(Max.Num)
    ## print(c(X1, X2, X3))
    ## print('Brute force approach')
    ## print(c(Brute.X1, Brute.X2, Brute.X3, Brute.W[Best[1]]))
}    
