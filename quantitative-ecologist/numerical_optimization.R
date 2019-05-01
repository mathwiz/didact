f<- function(t, T, b) { b*T - t^2 }
f # the derivative to solve at zero

g<- uniroot(f, lower=0, upper=10, T=0.5, b=1.3)
g

r<- function(t, T, b, a) { a*t / ((b+t)*(T+t)) }
r # direct function to optimize

g2<- optimize(r, lower=0, upper=10, a=10, b=1.3, T=0.5, maximum=TRUE)
g2

