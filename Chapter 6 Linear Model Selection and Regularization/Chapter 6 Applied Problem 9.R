# Chapter 6 Applied Problem 9

library(ISLR)

# Split the data into a training and test set
dim(College)
set.seed(1)
names(College)
?sample
split = sample(seq(777), 500, replace = FALSE)
train = College[split, ]
test = College[-split,]
test.Apps = College$Apps[-split]

# Fit a linear model on the training set
lm.fit = lm(Apps ~ ., data = train)
summary(lm.fit)

lm.pred = predict(lm.fit, newdata = test)
summary(lm.pred)

SSE_test = sum((lm.pred - test$Apps)^2)
SST_test = sum((mean(train$Apps) - test$Apps)^2)
RMSE_test = sqrt(SSE_test / nrow(test))

R2_test = 1 - SSE_test / SST_test

