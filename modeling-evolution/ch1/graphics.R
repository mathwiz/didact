rm(list=ls())

zdist = function(x, y) {
    sqrt(x^2 + y^2)
}

n1 = 4*2
n2 = 3*2
x = seq(from=-10, to=10, length=n1)
y = seq(from=0, to=10, length=n2)

z.matrix = outer(x, y, zdist)

print('graphics finished')

output = function () {
    # divide graphics page
    par(mfrow=c(1,2))
    contour(x, y, z.matrix, xlab='x', ylab='y')
    persp(x, y, z.matrix, theta=30, phi=10)
}    
