                                        # Model data with a sixth degree polynomial
                                        # lm estimates constants of polynomial


                                        # Data
                                        #length in m
s<- c(0, 500, 1000, 1350, 1720, 2050, 2400, 3000, 3300, 3770, 4000, 4500, 5000)
                                        #depth in m
d<- c(100, 500, 980, 1120, 1040, 950, 1050, 1900, 2330, 2880, 2850, 2100, 2200)

dat<- data.frame(s,d)
depth.model<- lm(d ~ s + I(s^2) + I(s^3) + I(s^4) + I(s^5) + I(s^6), data=dat)

summary.lm(depth.model)

plot(s/1000, -d/1000, xlab="Transect length(km)", ylab="Bathymetry(km)")
fl<- seq(0,5000) # regular transect lengths to be used for prediction
lines(fl/1000, -predict(depth.model, newdata=data.frame(s=fl))/1000)

