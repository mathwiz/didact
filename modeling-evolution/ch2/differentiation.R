rm(list=ls())


Dx1 = deriv(~ a + b*x + c*x^2, "x")

Dx2 = deriv(~ exp(a*x) + b*x + c*x^2, "x")

print(Dx1)
print(Dx2)

diffx2 = function(x) {
    exp(pi/2*x) + pi/4*x + (-pi)*x^2 
}

root = function(f, lo, hi) {
    uniroot(f, interval=c(lo, hi))$root
}


print('differentiation done.')

output = function() {
    print(diffx2(5))
    plot(diffx2, -pi, pi)
    print(root(diffx2, -pi, pi))
    print(nlm(f=diffx2, p=0)$estimate)
    print(optimize(f=diffx2, interval=c(-pi, pi))$minimum)
}    
