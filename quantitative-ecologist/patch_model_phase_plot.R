                                        # Model Specification

ep<- 0.8  # proportion of emigrants that survive
r<- 0.25  # local population growth rate
a<- 0.4   # rate of departure from patch 1
b<- 0.4   # rate of departure from patch 2

## differential equations for two patches
dx<- function(x, y) { return (r*x - a*x + ep*b*y) }
dy<- function(x, y) { return (r*y + ep*a*x - b*y) }


                                        # Plotting Parameters

xrang<- c(0, 40) # range of x-axis
yrang<- c(0, 40) # range of y-axis
res<- 15         # number of arrows in each direction
maxLength<- 6    # desired length of biggest arrow


                                        # Arrow Startpoints

xs<- rep(seq(xrang[1], xrang[2], length.out=res), res)
ys<- rep(seq(yrang[1], yrang[2], length.out=res), each=res)


                                        # Arrow Endpoints

## initial increments along x and y axes
dxs<- dx(xs, ys)
dxs
dys<- dy(xs, ys)
dys
s<- sqrt(dxs^2 + dys^2) # initial arrow lengths

## x and y coordinates for arrow endpoints
xe<- xs + maxLength*dxs/max(s)
ye<- ys + maxLength*dys/max(s)


                                        # Plotting

## creates an empty plot with required plot range
plot(0, 0, xlab=expression(n[1]), ylab=expression(n[2]), xlim=xrang, ylim=yrang)

abline(h=0) # draw x-axis
abline(v=0) # draw y-axis

arrows(xs, ys, xe, ye, length=0.1, angle=12) # plot arrows

