library(ISLR2)
summary(Weekly)
names(Weekly)
pairs(Weekly)
attach(Weekly)

# Graphical summaries
boxplot(Weekly$Lag1 ~ Weekly$Direction)
plot(Weekly$Volume, Weekly$Today)

# Logistic regression
glm.fit = glm(Direction ~ Lag1 + Lag2+ Lag3 + Lag4 + Lag5 + Volume, 
              data = Weekly, family = binomial)
summary(glm.fit)

# Confusion matrix and accuracy
glm.prob = predict(glm.fit, type = "response")
glm.prob[1:5]
glm.pred = ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction)
(54 + 557) / dim(Weekly)[1]
mean(glm.pred == Direction)

# Create a training set with data up to 2008
# Fit a logistic regression model
train = (Year < 2009)
Weekly.2009 = Weekly[!train, ]
dim(Weekly.2009)
Direction.2009 = Direction[!train]

glm.fit2 = glm(Direction ~ Lag2, data = Weekly, 
              family = binomial, subset = train)

glm.prob2 = predict(glm.fit2, Weekly.2009, type = "response")
glm.pred2 = ifelse(glm.prob2 > 0.5, "Up", "Down")
table(glm.pred2, Direction.2009)
mean(glm.pred2 == Direction.2009)

# Linear Discriminant Analysis (LDA)
lda.fit = lda(Direction ~ Lag2, data = Weekly,
              subset = train)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit, Weekly.2009)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2009)
mean(lda.class == Direction.2009)

# Quadratic Discriminant Analysis (QDA)
qda.fit = qda(Direction ~ Lag2, data = Weekly,
              subset = train)
qda.fit
qda.class = predict(qda.fit, Weekly.2009)$class
table(qda.class, Direction.2009)
mean(qda.class == Direction.2009)

# K-Nearest Neighbors (KNN)
train.X = Weekly$Lag2
test.X = Weekly.2009$Lag2
train.Direction = Weekly$Direction

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)

# Naive Bayes
library(e1071)
nb.fit = naiveBayes(Direction ~ Lag2, data = Weekly,
                    subset = train)

nb.fit
# Mean for Lag2
mean(Lag2[train][Direction[train] == "Down"])
# Standard Deviation for Lag2
sd(Lag2[train][Direction[train] == "Down"])

nb.class = predict(nb.fit, Weekly.2009)
table(nb.class, Direction.2009)
mean(nb.class == Direction.2009)
nb.preds = predict(nb.fit, Weekly.2009, type = "raw")
nb.preds[1:5, ]
