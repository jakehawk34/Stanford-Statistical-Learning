# K-Nearest Neighbors

library(class)
library(ISLR)
attach(Smarket)
ls()
train = Smarket$Year < 2005
?knn
xlag = cbind(Lag1, Lag2)
xlag[1:5, ]

# k = 1
knn.pred = knn(xlag[train,], xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred == Direction[!train])

# k = 3
# The results improve slightly compared to k = 1.
knn.pred = knn(xlag[train,], xlag[!train,], Direction[train], k=3)
table(knn.pred, Direction[!train])
mean(knn.pred == Direction[!train])

# Caravan K-Nearest Neighbors
dim(Caravan)
attach(Caravan)
summary(Purchase)
348 / 5822

# Standardize the data
standardized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])

# Split into train and test sets
test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

# The KNN error rate is just under 12%
# Only 6% of customers purchased insurance.
# We could get the error rate down to 6% by predicting "No" every time with no predictor values.
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
mean(test.Y != "No")
table(knn.pred, test.Y)

# We would like to sell insurance to customers who are actually interested.
# k = 3
knn.pred = knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y)
5 / 26

# k = 5
knn.pred = knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y)
4 / 15

