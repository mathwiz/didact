## Random numbers

x<- rnorm(50)
y<- x*rnorm(50, mean=50, sd=.1)
cor(x,y)


## Graphics

plot(x,y, xlab="rnorm(50)", ylab="rnorm(50, mean=50, sd=.1)")

pdf("Figure1.pdf")
plot(rnorm(100), rnorm(100), col="green")
dev.off()

x<- 1:10
y<- seq(1,10)
f<- outer(x, y, function(x,y) cos(y) / (1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T) # leave previous plot when running this
fa<- (f-t(f))/2
contour(x,y,fa,nlevels=15)

image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)


## Indexing data

A<- matrix(1:16, 4, 4)
dim(A)
A[2,3]
A[c(1,3), c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[,1]
A[1,]
A[-c(1,3),]



## Loading data

library(ISLR)
head(Auto)
Auto<- na.omit(Auto)
dim(Auto)
names(Auto)

plot(Auto$cylinders, Auto$mpg)
Auto$cylinders<- as.factor(Auto$cylinders)
summary(Auto$cylinders)

attach(Auto)
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)

hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)

pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

plot(horsepower, mpg)
identify(horsepower, mpg, name) # does not work

summary(Auto)
summary(mpg)
