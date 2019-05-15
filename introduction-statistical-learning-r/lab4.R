# Logisting regression example

library(ISLR)

attach(Smarket)
names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket[,-9])

plot(Volume)

glm.fit<- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

## predict a value of training data
contrasts(Direction)
glm.probs<- predict(glm.fit, type="response")
glm.probs[1:10]

## create variable with label Up/Down
glm.pred<- rep("Down", 1250)
glm.pred[glm.probs > .5]<- "Up"

table(glm.pred, Direction)
mean(glm.pred==Direction) # note that the predictions are on the training data

## Split data into training and test
train<- Year<2005
train
Smarket.2005<- Smarket[!train,]
dim(Smarket.2005)
Direction.2005<- Direction[!train]

## predict 2005 Direction using train from before 2005
glm.fit2<- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial, subset=train)
glm.probs2<- predict(glm.fit2, Smarket.2005, type="response")

glm.pred<- rep("Down", 252)
glm.pred[glm.probs2 > .5]<- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) 
mean(glm.pred != Direction.2005)


## try predicting with a reduced model
glm.fit2<- glm(Direction ~ Lag1 + Lag2, data=Smarket, family=binomial, subset=train)
glm.probs2<- predict(glm.fit2, Smarket.2005, type="response")

glm.pred<- rep("Down", 252)
glm.pred[glm.probs2 > .5]<- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) 
mean(glm.pred != Direction.2005)



# LDA example

library(MASS)
