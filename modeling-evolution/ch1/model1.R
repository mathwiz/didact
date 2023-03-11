rm(list=ls())

Maxgen = 100
N.Init = 20
Lambda = 1.1

Generation = seq(from=1, to=Maxgen) # vector of all the generation numbers
Npop = matrix(0, Maxgen, 1)
Npop[1] = N.Init

for (i in 2:Maxgen) {
    Npop[i] <- Lambda * Npop[i-1]
}    

print("model1 finished.")

# Output
output = function() {
    plot(Generation, Npop, xlab='Generation', ylab='Popluation size', type='l')
    print("Final population size")
    print(tail(Npop, 1))
}

