rm(list=ls())

age.weight = function(n) {
    x <- 1
    age <- seq(from=1, to=n)
    wt <- 4*x*exp(-(1+x/2)*age)
    return(sum(wt))
}    

size.fitness = function(age, x) {
    af <- 0; bf <- 8; as <- 1; bs <- 0.5
    return((af-bf*x) * exp(-(as+bs*x) * age))
}

Dx = deriv(~ (4*x) * exp(-(1+0.5*x)) / (1+0.5*x), "x")
derivative = function(x) {
    return (attr(eval(Dx), 'gradient'))
}


#### Main program ####
N.Max = 20
Age = matrix(seq(from=1, to=N.Max))
Weight = apply(Age, 1, age.weight)

Points = 100
Body.Size = seq(0, 3, length=Points)
Fitness = matrix(0, Points, 1)
Fitness.Max = -1
Fitenss.Max.Root = -1

for (i in 1:Points) {
    X = Body.Size[i]
    Fitness[i] = -1 * integrate(size.fitness, 1, Inf, X)$value
    if (Fitness[i] > Fitness.Max) {
        Fitness.Max = Fitness[i]
        Fitness.Max.Root = Body.Size[i]
    }
}

# Find root symbolically
Max.Root = uniroot(derivative, interval=c(0,4))$root

# Find root numerically
inverse.size.fitness = function(age, x) {
    af <- 0; bf <- 8; as <- 1; bs <- 0.5
    return(-(af+bf*x) * exp(-(as+bs*x) * age))
}
Numeric.Func = function(x) { integrate(inverse.size.fitness, 1, Inf, x)$value }
Max.Root.Numeric = nlm(Numeric.Func, p=1)$estimate

print('age-specific-mortality-continuous done.')

output = function() {
    par(mfcol=c(2,1))
    plot(Age, Weight, type='l', xlab='Age', ylab='Weight, Wt', las=1, lwd=3)
    plot(Body.Size, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    print("Maximum Fitness by iteration")
    print(Fitness.Max)
    print("at Body Size (Root of Max)")
    print(Fitness.Max.Root)
    print("Root of maximum by derivative")
    print(Max.Root)
    print("Root of maximum by numerical search")
    print(Max.Root.Numeric)
}    
