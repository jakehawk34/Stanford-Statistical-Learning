# K-Fold Cross Validation

# The cv.glm() function can be used to implement k-fold CV
set.seed(17)
cv.error.10 = rep(0, 10)

for (i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

# There is a sharp drop in the estimated test MSE between the linear and quadratic fits.
# Then, there is clear improvement in any higher-order polynomials after that.
cv.error.10
