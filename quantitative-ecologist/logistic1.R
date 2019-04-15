                                        # Logisitic model

                                        # Model parameters
R<- 2.57
K<- 200
XMAX<- 400
TMAX<- 100
x1<- 20
y1<- 0

                                        # Logistic function
logistic<- function(p, rmax, K) {
    p + rmax*(1-p/K)*p
}


                                        # Main plot
x<- seq(0, XMAX)
plot(x, logistic(x,R,K), xlab=expression(P[t]), ylab=expression(P[t+1]), type="l", xlim=c(0,XMAX), ylim=c(0,XMAX))
abline(0,1)

                                        # Main loop to calculate the specific solution
for (t in 1:TMAX) {
    y2<- logistic(x1,R,K)
    arrows(c(x1,x1), c(y1,y2), c(x1,y2), c(y2,y2), length=0.1, angle=10)
    x1<- y2
    y1<- y2
}

