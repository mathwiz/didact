D(expression(sin(x)), "x")

D(expression(3*x^3 + 0.5*x^2 - 2*x + 1), "x")

D(expression(a*x / (1 + b*x)), "x")

a<- 3
b<- 0.1
x<- 100
eval(D(expression(a*x / (1 + b*x)), "x"))


