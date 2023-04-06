rm(list=ls())

S1 = 0.035; S2 = 0.030; S3 = 0.025
F.Max = 2.0; A = 0.1
N = 100
R = 400

fitness = function(x) {
    # x[1], x[2] are propagule size in first, second clutch
    w1 <- w2 <- w3 <- 0
    # check if first clutch exceeds reserves
    if(N*x[1] > R) {
        w <- 0
    } else {
        w1 <- N*S1*F.Max*(1-exp(-A*x[1]))

        # check second clutch
        if(N*(x[1] + x[2]) > R) {
            w <- w1
        } else {
            w2 <- N*S2*F.Max*(1-exp(-A*x[2]))

            # calculate third clutch
            x3 <- (R - N*(x[1] + x[2])) / N
            w3 <- N*S3*F.Max*(1-exp(-A*x3))
            w <- w1 + w2 + w3
        }    
    }    
    
    return(-w)
}    


#### Main program ####

Size = 20
X = seq(from=1.0, to=5.0, length=Size)
D = expand.grid(X, X)
W = apply(D, 1, fitness)
W = matrix(W, Size, Size)

# find maximum using calculus
fitness2 = function(x) {
    x2 <- x
    x1 <- 10*log(S1/S2) + x2
    x3 <- (R - N*(x1 + x2)) / N

    if(x3 < 0) {
        w <- 0
    } else if(N*x1 > R) {
        w <- 0
    } else {
        w1 <- N*S1*F.Max*(1-exp(-A*x1))
        if(N*x2 > R) {
            w <- w1
        } else {
            w2 <- N*S2*F.Max*(1-exp(-A*x2))
            w3 <- N*S3*F.Max*(1-exp(-A*x3))
            w <- w1 + w2 + w3
        }    
    }

    print(c(x1, x2, x3))
    return(-w)
}    
Max.Symb = nlm(fitness2, p=1)$estimate

# second method
gradient = function(w, y) {
    # x1 is 1st propagule, x2 is second
    dx1x2 <- deriv(~ (N*S1*F.Max*(1-exp(-A*x1)) + N*S2*F.Max*(1-exp(-A*x2)) + N*S3*F.Max*(1-exp(-A*(R/N-(x1+x2))))), c("x1", "x2"))
    x1 <- w
    x2 <- y
    z <- eval(dx1x2)
    g <- attr(z, "gradient")
    return(abs(g[1] - g[2]))
}

fitness3 = function(x) {
    x2 <- x
    x1 <- nlm(gradient, p=1, x2)$estimate
    x3 <- (R - N*(x1 + x2)) / N

    if(x3 < 0) {
        w <- 0
    } else if(N*x1 > R) {
        w <- 0
    } else {
        w1 <- N*S1*F.Max*(1-exp(-A*x1))
        if(N*x2 > R) {
            w <- w1
        } else {
            w2 <- N*S2*F.Max*(1-exp(-A*x2))
            w3 <- N*S3*F.Max*(1-exp(-A*x3))
            w <- w1 + w2 + w3
        }    
    }

    return(-w)
}    
Max.Symb2 = nlm(fitness3, p=1)$estimate

# finding maximum numerically
Max.Num <- nlm(fitness, p=c(0.1, 0.1))$estimate
X1 = Max.Num[1]
X2 = Max.Num[2]
X3 = (R - N*(X1 + X2)) / N

# brute force approach
Brute.Size = 100
Brute.X = seq(from=1.0, to=5.0, length=Brute.Size)
Brute.Y = seq(from=1.0, to=5.0, length=Brute.Size)
Brute.D = expand.grid(Brute.X, Brute.Y)
Brute.W = apply(Brute.D, 1, function(x) { -fitness(x) })
# find position in row with highest fitness
Best = order(Brute.W, na.last=T, decreasing=T)
Brute.X1 = Brute.D[Best[1],1]
Brute.X2 = Brute.D[Best[1],2]
Brute.X3 = (R - N*(Brute.X1 + Brute.X2)) / N


print('three-traits done.')

output = function() {
    par(mfrow=c(1,1))
    #contour(X, X, -W, xlab='Propagule size 1st clutch', ylab='Popagule size 2nd clutch')
    persp(X, X, W, xlab='Propagule size 1st clutch', ylab='Popagule size 2nd clutch', zlab='Fitness', theta=25, phi=25, lwd=1)
    print('Maximum using calculus')
    print(Max.Symb)
    print('Maximum using calculus (alternate method)')
    print(Max.Symb2)
    print('Maximum numerically')
    print(Max.Num)
    print(c(X1, X2, X3))
    print('Brute force approach')
    print(c(Brute.X1, Brute.X2, Brute.X3, Brute.W[Best[1]]))
}    
