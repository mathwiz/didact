library(ISLR)
library(MASS)

fix(Boston)
head(Boston)
names(Boston)

bostonM<- lm(medv ~ lstat, data=Boston)
summary.lm(bostonM)

