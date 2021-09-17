# Chapter 6 Applied Problem 9

library(ISLR)
library(glmnet)
library(pls)

# Split the data into a training and test set
dim(College)
set.seed(11)
names(College)
sum(is.na(College))

# Normalize
?apply
College[, -1] = apply(College[, -1], 2, scale)

train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]


# Fit a linear model on the training set and evaluate the test set performance.
lm.fit = lm(Apps ~ ., data = College.train)
summary(lm.fit)

lm.pred = predict(lm.fit, newdata = College.test)
mean((College.test[, "Apps"] - lm.pred)^2)

# Fit a ridge regression model on the training set,
# with lambda chosen by cross-validation.
summary(College)
train.mat = model.matrix(Apps ~ . -1, data = College.train)
test.mat = model.matrix(Apps ~ . -1, data = College.test)
grid = 10 ^ seq(4, -2, length = 100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"],
                      alpha = 0, lambda = grid, thresh = 1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best

ridge.pred = predict(mod.ridge, newx = test.mat, s = lambda.best)
mean((College.test[, "Apps"] - ridge.pred)^2)


# # Fit a lasso model on the training set,
# with lambda chosen by cross-validation.
mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"],
                      alpha = 1, lambda = grid, thresh = 1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best

lasso.pred = predict(mod.lasso, newx = test.mat, s = lambda.best)
mean((College.test[, "Apps"] - lasso.pred)^2)

mod.lasso = glmnet(model.matrix(Apps ~ . -1, data = College), 
                   College[, "Apps"], alpha = 1)
predict(mod.lasso, s = lambda.best, type = "coefficients")

# Compare the R2 values for the OLS, Ridge Regression, and Lasso models
test.avg = mean(College.test[, "Apps"])

lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) / 
  mean((College.test[, "Apps"] - test.avg)^2)

ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) / 
  mean((College.test[, "Apps"] - test.avg)^2)

lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) / 
  mean((College.test[, "Apps"] - test.avg)^2)

barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2), 
        col = "red", names.arg = c("OLS", "Ridge", "Lasso"),
        main = "Test R-Squared")
# All models predict the number of college applications with excellent accuracy.