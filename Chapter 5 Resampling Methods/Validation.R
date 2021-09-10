# Validation

library(ISLR2)
plot(mpg ~ horsepower, data = Auto)
?sample
set.seed(1)
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# Use the poly() function to estimate the test eror for the quadratic
# and cubic regressions
lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto,
             subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto,
             subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# Choose a different training set to get different errors on the validation set
set.seed(2)
train = sample(x = 392, size = 196)
lm.fit = lm(mpg ~ horsepower, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)

# Quadratic
lm.fit2 = lm(mpg ~ poly(horsepower, 2), subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

# Cubic
lm.fit3 = lm(mpg ~ poly(horsepower, 3), subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# The quadratic model performs better than the linear model.
# The cubic model does not show a significant improvement compared to the quadratic.