                                        # General cobweb diagram

                                        # Model parameters
R<- 0.5
XMAX<- 2000
TMAX<- 5
x1<- 200
y1<- 0

                                        # Main plot
x<- seq(0, XMAX)
plot(x, x*(1+R), xlab=expression(P[t]), ylab=expression(P[t+1]), type="l", xlim=c(0,XMAX), ylim=c(0,XMAX))
abline(0,1)

                                        # Main loop to calculate the specific solution
for (t in 1:TMAX) {
    y2<- x1*(1+R)
    arrows(c(x1,x1), c(y1,y2), c(x1,y2), c(y2,y2), length=0.1, angle=10)
    x1<- y2
    y1<- y2
}

