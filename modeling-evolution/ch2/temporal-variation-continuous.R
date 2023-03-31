rm(list=ls())
set.seed(10)

fitness = function(bs, x) {
    c <- 5
    w.ind <- c * log((Af + Bf*x) * (As -bs*x))
    return(w.ind)
}    

fitness.integrated = function(x) {
    w <- integrate(fitness, B.Min, B.Max, x)$value
    return(-w)
}    

#### Main program ####
Af = 2.0; Bf = 2.0; As = 0.6

N = 100
X = seq(from=1.0, to=3.0, length=N)
Fitness.Log = matrix(0, N, 1)
B.Min = 0.0; B.Max = 0.2
for(i in 1:N) {
    Fitness.Log[i] = integrate(fitness, B.Min, B.Max, X[i])$value
}
Fitness = exp(Fitness.Log)

# find maximum with numeric integration
Max.Root.Num1 = nlm(fitness.integrated, p=1)$estimate
Max.Root.Num2 = optimize(f=fitness.integrated, interval=c(1,3), maximum=FALSE)$minimum

print('temporal-variation-continuous done.')

output = function() {
    par(mfcol=c(1,1))
    plot(X, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    print("Fitness by numeric integration (nlm, optimize)")
    print(c(Max.Root.Num1, Max.Root.Num2))
}    
