rm(list=ls())

fitness = function(x) {
    4*x - 2*x^2
}

wdiff = function(x, step) {
    return (fitness(x + step) - fitness(x))
}

#### Main program ####

x = 0
step = 0.001

diff = wdiff(x, step)
while (diff > 0) {
    x = x + step
    diff = wdiff(x, step)
}

print('numeric done.')

output = function() {
    print(c(x, diff))
}    
