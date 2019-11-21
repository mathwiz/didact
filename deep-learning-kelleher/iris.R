data(iris)
fit <- lm(Petal.Width ~ Petal.Length, data=iris)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
