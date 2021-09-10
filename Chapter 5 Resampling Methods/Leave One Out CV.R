# Leave-One-Out Cross Validation (LOOCV)

# The glm() function performs linear regression when 
# the family = "binomial" argument is left out
glm.fit = glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

lm.fit = lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)

# glm() function can be used with cv.glm()
library(boot)
glm.fit = glm(mpg ~ horsepower, data = Auto)
?cv.glm
cv.err = cv.glm(data = Auto, glm.fit)

# The numbers in the delta vector contain the cross-validation (CV) results
cv.err$delta

# Use a for loop with the for() function to iteratively fit 
# polynomial regressions for polynomials of order i=1 to i=10,
# compute the associated CV error, and store it in the ith elemt of the cv.error vector.
cv.error = rep(0, 10)
for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(data = Auto, glm.fit)$delta[1]
}

# There is a sharp drop in the estimated test MSE between the linear and quadratic fits.
# Then, there is clear improvement in any higher-order polynomials after that.
cv.error
plot(1:10, cv.error, type = "b")

# Write a function to create the formula from 5.2
loocv = function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit) / (1 - h))^2)
}

# Now, try it out with the glm fit
loocv(glm.fit)

for (i in 1:10) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = loocv(glm.fit)
}

plot(1:10, cv.error, type = "o")





