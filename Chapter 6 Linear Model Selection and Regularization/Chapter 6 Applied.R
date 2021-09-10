# Chapter 6 Applied Problems

?rnorm
# Predictor X and noise vector
set.seed(1)
X = rnorm(100)
noise = rnorm(100)

# Response vector Y with constants
Y = 3 + 2*X - 3*X^2 + 0.3*X^3 + noise

# Perform best subset selection
library(leaps)
data.full = data.frame("y" = Y, "x" = X)
regfit.best = regsubsets(y ~ poly(x, 10, raw=TRUE), data = data.full, nvmax = 10)
best.summary = summary(regfit.best)

# Find the model with best Cp, BIC, and adjusted R2
which.min(best.summary$cp)
which.min(best.summary$bic)
which.max(best.summary$adjr2)

# Create plots
par(mfrow = c(2,2))

# Cp
plot(best.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
points(3, best.summary$cp[3], pch=19, col="red")

# BIC
plot(best.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
points(3, best.summary$bic[3], pch=19, col="red")

# Adjusted R2
plot(best.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")
points(3, best.summary$adjr2[3], pch=19, col="red")

#RSS
plot(best.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "b")
points(3, best.summary$rss[3], pch=19, col="red")

# Best subset selects the X, X^2, and X^7 variables.
coef(regfit.best, 3)

# Perform forward stepwise selection
regfit.fwd = regsubsets(y ~ poly(x, 10, raw=TRUE), data = data.full, nvmax = 10, method = "forward")
fwd.summary = summary(regfit.fwd)
fwd.summary

# Find the model with best Cp, BIC, and adjusted R2
which.min(fwd.summary$cp)
which.min(fwd.summary$bic)
which.max(fwd.summary$adjr2)

# Perform backward stepwise selection
regfit.bwd = regsubsets(y ~ poly(x, 10, raw=TRUE), data = data.full, nvmax = 10, method = "backward")
bwd.summary = summary(regfit.bwd)
bwd.summary

# Find the model with best Cp, BIC, and adjusted R2
which.min(bwd.summary$cp)
which.min(bwd.summary$bic)
which.max(bwd.summary$adjr2)

# Create plots
par(mfrow = c(3,2))

# Cp
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
points(3, fwd.summary$cp[3], pch=19, col="red")
plot(bwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
points(3, bwd.summary$cp[3], pch=19, col="red")

# BIC
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
points(3, fwd.summary$bic[3], pch=19, col="red")
plot(bwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
points(3, bwd.summary$bic[3], pch=19, col="red")

# Adjusted R2
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")
points(3, fwd.summary$adjr2[3], pch=19, col="red")
plot(bwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")
points(3, bwd.summary$adjr2[3], pch=19, col="red")

# Forward stepwise selects the X, X^2, and X^7 variables.
coef(regfit.fwd, 3)

# Backward stepwise selects the X, X^2, and X^9 variables.
coef(regfit.bwd, 3)

# Fit a lasso model to the data
par(mfrow = c(1,1))
library(glmnet)
x = model.matrix(Y ~ ., data = data.full)
y = Y
fit.lasso = glmnet(x, y)
plot(fit.lasso, xvar = "lambda", label=TRUE)
cv.lasso = cv.glmnet(x, y)
plot(cv.lasso)
coef(cv.lasso)


