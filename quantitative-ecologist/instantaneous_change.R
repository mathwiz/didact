# replace with any desired function
f<- function(x) { return(x^3) }

                                        # Parameters
x<- 3
y<- f(x)
Xmin<- -2
Xmax<- 8
N<- 10
Inc<- 0.1

                                        # Plot
xs<- seq(Xmin, Xmax, by=Inc)
plot(xs, f(xs), type="l", col="red")
i<- seq(N,1,by=-1)
dx<- i * (Xmax-x) / N
m<- (f(x+dx) - f(x)) / dx
c<- f(x) - x*m
for(i in 1:N) abline(c[i],m[i])

