library(ISLR)
library(MASS)

# Simple Regression

fix(Boston)
head(Boston)
names(Boston)

bostonM<- lm(medv ~ lstat, data=Boston)
summary.lm(bostonM)
names(bostonM)
plot(bostonM$fitted.values, bostonM$residuals)
coef(bostonM)
confint(bostonM)
predict(bostonM, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(bostonM, data.frame(lstat=c(5,10,15)), interval="prediction")
plot(Boston$lstat, Boston$medv)
abline(bostonM, lwd=3, col="red")

plot(1:20, 1:20, pch=1:20)
par(mfrow=c(2,2))
plot(Boston$lstat, Boston$medv, col="red")
plot(predict(bostonM), residuals(bostonM))
plot(predict(bostonM), rstudent(bostonM))
plot(hatvalues(bostonM))
which.max(hatvalues(bostonM))


# Multiple Regression
library(car)

bostonMR<- lm(medv ~ lstat + age, data=Boston)
summary.lm(bostonMR)

bostonAll<- lm(medv ~ ., data=Boston)
summary.lm(bostonAll)

vif(bostonAll)
bostonNoAge<- lm(medv ~ . -age, data=Boston)
summary.lm(bostonNoAge)

# Interaction terms
summary(lm(medv ~ lstat * age, data=Boston))

# Non-linear transformations of predictors
model2<- lm(medv ~ lstat + I(lstat^2), data=Boston)
summary.lm(model2)

anova(bostonM, model2)

par(mfrow=c(2,2))
plot(model2)

model3<- lm(medv ~ poly(lstat, 5), data=Boston)
summary.lm(model3)

summary(lm(medv ~ log(rm), data=Boston))


# Qualitative predictors

fix(Carseats)
names(Carseats)

seatsM<- lm(Sales ~ . +Income:Advertising +Price:Age, data=Carseats)
summary.lm(seatsM)

contrasts(Carseats$ShelveLoc)


# Writing functions
LoadLibraries<- function() {
	library(MASS)
	library(ISLR)
	library(car)
	library(pastecs)
	print("done")
}


