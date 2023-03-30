rm(list=ls())

size.fitness = function(age, x, r) {
    af <- 0.0; bf <- 4*4; as <- 1; bs <- 0.5
    return((af + bf*x) * exp(-(as + bs*x + r) * age))
}

integral = function(r, x) {
    1 - integrate(size.fitness, 1, Inf, x, r)$value
}

rcalc = function(x) {
    uniroot(integral, interval=c(1e-7, 10), x)$root
}

rfunc1 = function(x) {
    af <- 0.0; bf <- 4*4; as <- 1; bs <- 0.5
    r <- bs*(af + bf*x) / (bf - bs*af - bs*bf*x) - bs*x - as
    return(log(af + bf*x) - (as + bs*x + r) - log(as + bs*x + r))
}    

rfunc2 = function(x) {
    af <- 0.0; bf <- 4*4; as <- 1; bs <- 0.5
    r <- bs*(af + bf*x) / (bf - bs*af - bs*bf*x) - bs*x - as
    return(abs(log(af + bf*x) - (as + bs*x + r) - log(as + bs*x + r)))
}    

#### Main program ####
Points = 100
Body.Size = matrix(seq(0.5, 3, length=Points))
Fitness = apply(Body.Size, 1, rcalc)

# Find root symbolically: 3 methods
Max.Root1 = uniroot(rfunc1, interval=c(1.2, 1.8))$root
Max.Root2 = nlm(rfunc2, p=1.2)$estimate
Max.Root3 = optimize(rfunc2, interval=c(1.2, 3), maximum=FALSE)$minimum

# Find root numerically
size.fitness.integrated = function(r, x) { 1 - integrate(size.fitness, 1, Inf, x, r)$value }
numeric.func = function(x) { uniroot(size.fitness.integrated, interval=c(1e-07, 10), x)$root }
Max.Root.Numeric1 = optimize(numeric.func, interval=c(1.2, 1.8), maximum=TRUE)$maximum
Max.Root.Numeric2 = nlm(function(x) { -1 * numeric.func(x) }, p=1.2)$estimate

print('malthusian done.')

output = function() {
    par(mfcol=c(1,1))
    plot(Body.Size, Fitness, type='l', xlab='Body Size', ylab='Fitness, W', las=1, lwd=4)
    print("Root of maximum by derivative")
    print(Max.Root1)
    print(Max.Root2)
    print(Max.Root3)
    print("Root of maximum by numerical search")
    print(Max.Root.Numeric1)
    print(Max.Root.Numeric2)
}    
