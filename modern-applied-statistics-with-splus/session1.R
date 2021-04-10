library(MASS)

x <- rnorm(50)
y <- rnorm(50)

h <- chull(x,y)

plot(x,y)

polygon(x[h], y[h], dens=15, angle=15)  # does not work

objects()

x <- rnorm(10000)
y <- rnorm(10000)

truehist(c(x,y+2), nbins=25)


# basic stats

x <- seq(1, 20, 0.5)
x

w <- 1 + x/2
y <- x + w*rnorm(x)
y

dum <- data.frame(x,y,w)
dum

rm(x,y,w)

fm <- lm(y ~ x, data=dum)
summary(fm)

fm1 <- lm(y ~ x, data=dum, weight=1/w^2)
summary(fm1)

lrf <- loess(y ~ x, dum)
summary(lrf)

attach(dum)

plot(x,y)

lines(spline(x, fitted(lrf)))

abline(0, 1, lty=3)

abline(fm)

abline(fm1, lty=4)

plot(fitted(fm), resid(fm), xlab="Fitted Values", ylab="Residuals")

qqnorm(resid(fm))

qqline(resid(fm))

detach()

rm(fm, fm2, lrf, dum)


# Another dataset

hills

library(lattice)

splom(~ hills)

brush(as.matrix(hills))   # does not work

attach(hills)

plot(dist, time)

identify(dist, time, row.names(hills))  # click on plot to see names

abline(lm(time ~ dist))

abline(ltsreg(dist, time), lty=3)

detach()


# Michelson-Morley data

attach(michelson)

search()

plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No.")

fm <- aov(Speed ~ Run + Expt)
summary(fm)

fm0 <- update(fm, . ~ . -Run)
summary(fm0)

anova(fm0, fm)

detach()

rm(fm, fm0)


