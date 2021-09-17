# Chapter 6 Applied Problem 11
library(ISLR)
library(MASS)
library(leaps)

# Predict the per capita crime rate in the Boston data set
set.seed(1)
summary(Boston)
str(Boston)

# Best Subset Selection
regfit.best = regsubsets(crim ~ ., data = Boston, nvmax = 13)
regfit.summary = summary(regfit.best)

par(mfrow = c(2, 2))

plot(regfit.summary$rss, xlab = "Number of Variables", ylab = "RSS")
which.min(regfit.summary$rss)
points(13, regfit.summary$rss[13], pch = 19, col = "red")

plot(regfit.summary$bic, xlab = "Number of Variables", ylab = "BIC")
which.min(regfit.summary$bic)
points(3, regfit.summary$bic[3], pch = 19, col = "red")

plot(regfit.summary$cp, xlab = "Number of Variables", ylab = "Cp")
which.min(regfit.summary$cp)
points(8, regfit.summary$cp[8], pch = 19, col = "red")

plot(regfit.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2")
regfit.summary$adjr2
which.max(regfit.summary$adjr2)
points(9, regfit.summary$adjr2[9], pch = 19, col = "red")

par(mfrow = c(1,1))
plot(regfit.best, scale = "Cp")

# Create a training set and a validation set
dim(Boston)
train.size = dim(Boston)[1] / 2
train = sample(1:dim(Boston)[1], train.size)
test = -train
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]

regfit.best = regsubsets(crim ~ ., data = Boston.train, nvmax = 13)

val.errors = rep(NA, 13)
x.test = model.matrix(crim~., data = Boston.test) # notice the - index!
for(i in 1:13){
  coefi = coef(regfit.best, id=i)
  pred = x.test[,names(coefi)]%*%coefi
  val.errors[i] = mean((Boston.test$crim-pred)^2)
}


plot(sqrt(val.errors), ylab="Root MSE", ylim = c(5, 12), pch=19, type="b")
points(sqrt(regfit.best$rss[-1]/180), col="blue", pch=19, type="b")
legend("topright", legend=c("Training", "Validation"), col=c("blue", "black"), pch=19)

predict.regsubsets = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  mat[, names(coefi)]%*%coefi
}

set.seed(11)
folds = sample(rep(1:10, length = nrow(Boston)))
folds
table(folds)
cv.errors = matrix(NA, 10, 13)
for (k in 1:10){
  best.fit = regsubsets(crim~., 
                        data = Boston[folds != k,], 
                        nvmax=13)
  for (i in 1:13){
    pred = predict(best.fit, Boston[folds == k,], id=i)
    cv.errors[k, i] = mean((Boston$crim[folds == k] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")

# Ridge Regression
train.mat = model.matrix(crim ~ . -1, data = Boston.train)
test.mat = model.matrix(crim ~ . -1, data = Boston.test)
grid = 10 ^ seq(4, -2, length = 100)
mod.ridge = cv.glmnet(train.mat, Boston.train[, "crim"],
                      alpha = 0, lambda = grid, thresh = 1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best
plot(mod.ridge)

ridge.pred = predict(mod.ridge, newx = test.mat, s = lambda.best)
mean((Boston.test$crim - ridge.pred)^2)
coef(mod.ridge, s = lambda.best)

# Lasso
mod.lasso = cv.glmnet(train.mat, Boston.train$crim,
                      alpha = 1, lambda = grid, thresh = 1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best

lasso.pred = predict(mod.lasso, newx = test.mat, s = lambda.best)
mean((Boston.test$crim - lasso.pred)^2)
coef(mod.lasso, s = lambda.best)

mod.lasso = glmnet(model.matrix(crim ~ . -1, data = Boston), 
                   Boston$crim, alpha = 1)
predict(mod.lasso, s = lambda.best, type = "coefficients")

