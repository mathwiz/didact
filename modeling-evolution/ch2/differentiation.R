rm(list=ls())

print('differentiation done.')

Dx1 = deriv(~ a + b*x + c*x^2, "x")

Dx2 = deriv(~ exp(a*x) + b*x + c*x^2, "x")

output = function() {
    print(Dx1)
    print(Dx2)
}    
