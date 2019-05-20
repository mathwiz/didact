# Logistic regression example

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
summary(train)
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

lda.fit<- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

lda.pred<- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class<- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]

sum(lda.pred$posterior[,1] > .9) # none


## Quadratic Discrimminant Analysis

qda.fit<- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

qda.class<- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)


                                        # KNN

library(class)

train.X<- cbind(Lag1, Lag2)[train,]
test.X<- cbind(Lag1, Lag2)[!train,]
train.Direction<- Direction[train]

set.seed(1)
knn.pred<- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83 + 43) / 252

knn.pred<- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
(87 + 48) / 252
mean(knn.pred == Direction.2005)


## KNN with another dataset

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X<- scale(Caravan[,-86]) # exclude categorical column 86
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

test<- 1:1000
train.X<- standardized.X[-test,]
test.X<- standardized.X[test,]
train.Y<- Purchase[-test]
test.Y<- Purchase[test]
set.seed(1)

knn.pred<- knn(train.X, test.X, train.Y, k=5)
mean(test.Y != knn.pred) # error rate of this model
mean(test.Y != "No") # best success rate without predictive model (always guess "No")
table(knn.pred, test.Y)

