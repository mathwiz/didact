D(expression(sin(x)), "x")

D(expression(3*x^3 + 0.5*x^2 - 2*x + 1), "x")

D(expression(a*x / (1 + b*x)), "x")

a<- 3
b<- 0.1
x<- 100
eval(D(expression(a*x / (1 + b*x)), "x"))

D(expression(5 + 5*cos(pi * (x-2) / 12)), "x")

D(D(expression(-3*x^2 + 5*x - 2), "x"), "x")

                                        # Partial differentiation

D(expression(x / (x + y)), "x")

D(D(expression(x / (x + y)), "x"), "x")

