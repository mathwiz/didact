rm(list=ls())

fitness = function(x) {
    eval(FitnessExpr)
}

#### Main program ####

FitnessExpr = expression(-2*x^2 + 4*x)
X = seq(0, 2, length=1000)
W = sapply(X, fitness)

Dx = deriv(FitnessExpr, 'x')
grad = function(w) {
    x <- w # what does this do?
    return (attr(eval(Dx), 'gradient'))
}

Max.Root = uniroot(grad, interval=c(-2,4))$root
Max.Numeric = nlm(function(x) { -1*fitness(x) }, p=-2)$estimate

print('age-structure done.')

output = function() {
    plot(X, W, type='l', xlab='Body size', ylab='Fitness, W', las=1, lwd=3)
    print("Maximum by root of derivative")
    print(Max.Root)
    print("Maximum by numerical search")
    print(Max.Numeric)
}    
