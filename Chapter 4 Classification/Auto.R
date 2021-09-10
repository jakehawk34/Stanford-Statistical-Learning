dim(Auto)
names(Auto)

# Create a binary variable, mpg01
median(Auto$mpg)
Auto$mpg01 = ifelse(Auto$mpg > median(Auto$mpg), 1, 0)

# Explore the data graphically
boxplot(Auto$horsepower ~ Auto$mpg01)
boxplot(Auto$weight ~ Auto$mpg01)
boxplot(Auto$acceleration ~ Auto$mpg01)
boxplot(Auto$cylinders ~ Auto$mpg01)
hist(Auto$mpg01)

# Split the data into training and test sets
library(caTools)
spl = sample.split(Auto$mpg01, SplitRatio = 0.5)

train = subset(Auto, spl == TRUE)
test = subset(Auto, spl == FALSE)

# LDA
lda.fit = lda(mpg01 ~ horsepower + weight + acceleration,
              data = train)
plot(lda.fit)

lda.pred = predict(lda.fit, test)
lda.class = lda.pred$class
table(lda.class, test$mpg01)
mean(lda.class == test$mpg01)

# QDA
qda.fit = qda(mpg01 ~ horsepower + weight + acceleration,
              data = train)
qda.fit

qda.pred = predict(qda.fit, test)
qda.class = qda.pred$class
table(qda.class, test$mpg01)
mean(qda.class == test$mpg01)

# Logistic regression
glm.fit = glm(mpg01 ~ horsepower + weight + acceleration,
              data = train, family = binomial)
glm.fit

glm.prob = predict(glm.fit, test, type = "response")

table(glm.prob > 0.5, test$mpg01)
(88 + 86) / 196

# Naive Bayes
nb.fit = naiveBayes(mpg01 ~ horsepower + weight + acceleration,
                    data = train)
nb.fit
nb.class = predict(nb.fit, test)
table(nb.class, test$mpg01)
mean(nb.class == test$mpg01)

# K-Nearest Neighbors (KNN) k = 1
predictors.train = cbind(train$horsepower, train$weight, train$acceleration)
predictors.test = cbind(test$horsepower, test$weight, test$acceleration)
knn.pred = knn(predictors.train, predictors.test, train$mpg01, k = 1)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)

# k = 3
knn.pred = knn(predictors.train, predictors.test, train$mpg01, k = 3)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)

# k = 5
knn.pred = knn(predictors.train, predictors.test, train$mpg01, k = 5)
table(knn.pred, test$mpg01)
mean(knn.pred == test$mpg01)
