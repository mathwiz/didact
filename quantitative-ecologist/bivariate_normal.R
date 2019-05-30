require(mvtnorm)

mh <- 1.9 # mean tree height
md <- 0.57 # mean tree depth
sh <- 0.22 # sd of height
sd <- 0.08 # sd of depth
ro <- 0.7 # correlation between height and depth
mu <- c(mh, md) # mean vector
cov <- ro*sh*sd # covariance
varcov <- matrix(c(sh^2, cov, cov, sd^2), nrow=2, ncol=2) # variance/covariance matrix

## plotting parameters
steps <- 40
hr <- c(mh - 2*sh, mh + 2*sh) # range of height
dr <- c(md - 2*sd, md + 2*sd) # range of depth
hlab <- seq(hr[1], hr[2], length.out=steps) # height labels
dlab <- seq(dr[1], dr[2], length.out=steps) # depth labels
h <- rep(hlab, steps) # height coordinates
d <- rep(dlab, 1, each=steps) # depth coordinates
pts <- cbind(h, d) # combined coordinates
dns <- dmvnorm(pts, mu, varcov) # list of probability densities
mat <- matrix(dns, steps, steps, byrow=T) # matrix of probability densities

contour(hlab, dlab, mat, xlab="Tree height", ylab="Root depth")

