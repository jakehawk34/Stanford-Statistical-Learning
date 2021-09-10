# Bootstrap

# First, we must create a function that computes the statistic of interest
# Second, we use the boot() function to perform the bootstrap by repeatedly
# sampling observations from the data set with replacement.

# Create a function, alpha.fn(), which takes as input the (X,Y) data as well as 
# a vector indicating which observations should be used to estimate alpha.
alpha.fn = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

# The following command tells R to estimate alpha using all 100 observations from Portfolio
alpha.fn(Portfolio, 1:100)

# This next command uses the sample() function to randomly select 100 observations from 
# the range 1 to 100, with replacement. This is equivalent to constructing a new bootstrap data set.
set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# Below, we produce R = 1,000 estimates for alpha.
boot.out = boot(Portfolio, alpha.fn, R = 1000)
plot(boot.out)
?boot

# Estimating the Accuracy of a Linear Regression Model
boot.fn = function(data, index)
  coef(lm(mpg ~ horsepower, data = data, subset = index))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

# Use the boot() function to compute the standard errors of 1,000 bootstrap estimates
# for the intercept and slope terms.
boot(Auto, boot.fn, R = 1000)

# Standard formulas can be used to compute the SEs for the regression coefficients in a linear model
summary(lm(mpg ~ horsepower, data = Auto))$coef

# The bootstrap estimates are somewhat different from the estimates for standard error found with the standard formulas.
# This is not surprising.
# The standard formulas depened on the unknown parameter for noise variance.
# The standard formulas also assume that that the x are fixed and all variability
# comes from the variation in errors.
# The bootstrap approach does not make any of these assumptions.
# Therefore, it is likely giving a more accurate estimate of the standard errors of the slope and intercept terms.

# Below, we compute the bootstrap SE estimates and the standard linear regression estimates
# that result from fitting the quadratic model to the data.
boot.fn = function(data, index)
  coef(lm(mpg ~ horsepower + I(horsepower^2),
          data = data, subset = index))

set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef


