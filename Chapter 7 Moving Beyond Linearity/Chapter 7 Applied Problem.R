# Chapter 7 Applied Problems

# Question 6

# Perform polynomial regression to predict wage using age
library(ISLR)
library(boot)
attach(Wage)
set.seed(1)
degree <- 10
cv.errs <- rep(NA, degree)
for (i in 1:degree) {
  fit <- glm(wage ~ poly(age, i), data = Wage)
  cv.errs[i] <- cv.glm(Wage, fit)$delta[1]
}

plot(1:degree, cv.errs, xlab = "Degree", ylab = "Test MSE", type="l")
deg.min = which.min(cv.errs)
points(deg.min, cv.errs[deg.min], col = 'red', cex = 2, pch = 19)

# The cross validation method suggests that a 9 degree polynomial has the minimum error.
# However, the 3-degree polynomial appears to be sufficient based on the earlier ANOVA test.

fit1 = lm(wage ~ age, data = Wage)
fit2 = lm(wage ~ poly(age, 2), data = Wage)
fit3 = lm(wage ~ poly(age, 3), data = Wage)
fit4 = lm(wage ~ poly(age, 4), data = Wage)
fit5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit1, fit2, fit3, fit4, fit5)

# Plot of the 3-degree polynomial
agelims = range(age)
age.grid = seq(from=agelims[1], to=agelims[2])
preds = predict(fit3, newdata = list(age = age.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(age, wage, col = "darkgrey")
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, col = "blue", lty=2)

# Fit a step function and use cross-validation to find the optimal number of cuts
cv.errs = rep(NA, degree)
?cut()
for (i in 2:degree){
  Wage$age.cut = cut(Wage$age, i)
  fit = glm(wage ~ age.cut, data = Wage)
  cv.errs[i] = cv.glm(Wage, fit)$delta[1]
}

plot(2:degree, cv.errs[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")
cuts.min = which.min(cv.errs)
points(cuts.min, cv.errs[cuts.min], col = "red", pch = 19, cex = 2)

# The optimal number of cuts is 8. The test MSE drops dramatically to a low point 8;
# it briefly increases at 9 cuts and then decreases slightly at 10 cuts.

# Predict with 8 cuts
plot(wage ~ age, data = Wage, col = "darkgrey")
fit = glm(wage ~ cut(age, 8), data = Wage)
preds = predict(fit, list(age = age.grid))
lines(age.grid, preds, lwd = 2, col = "red")

# Basic example of the cut() function
ex = cut(c(1, 3, 5, 7, 9), 3)
ex
length(ex)
class(ex[2])

# Question 7

# Explore the relationships between wage and other predictor variables in the `Wage`
# data set. Use non-linear techniques to fit flexible models to the data. Create plots
# with the fits created.

set.seed(1)
names(Wage)
summary(Wage$region)
summary(Wage$maritl)
summary(Wage$health_ins)

par(mfrow = c(1, 3))
plot(maritl, wage)
plot(health_ins, wage)

library(gam)
fita = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
fitb = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
fitc = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
fitd = gam(wage ~ lo(year, span = 0.7) + s(age, 5) + education + maritl + jobclass, data = Wage)
anova(fita, fitb, fitc, fitd)

# Model 4 fits the best.
par(mfrow = c(2, 3))
plot(fitd, se = TRUE, col = "blue")

# Question 8

# Fit some of the non-linear models to the `Auto` data set
attach(Auto)
names(Auto)
summary(Auto)
str(Auto)

# Explore the relation of polynomials for various predictors on mpg in the `Auto` data set
pairs(Auto)
fit <- lm(mpg ~ poly(cylinders, 2) + poly(displacement, 5) + poly(horsepower, 5)
          + poly(weight, 5), data = Auto)
summary(fit)

# Strong linear relation between weight and mpg;
# Weak linear relation between displacement and mpg;
# Strong quadratic relation between horsepower and mpg;
# Moderate relation between acceleration and mpg;
# No significant relationship between cylinders and mpg or 

# Test the degree with an ANOVA test
anv1 <- gam(mpg ~ displacement + horsepower + weight, data = Auto)
anv2 <- gam(mpg ~ displacement + s(horsepower, 2) + weight, data = Auto)
anv3 <- gam(mpg ~ s(displacement, 5) + s(horsepower, 5) + s(weight, 5), data = Auto)
anova(anv1, anv2, anv3, test = 'F')

# The fifth degree polynomials for displacement and horsepower are significant.
summary(anv3)

par(mfrow = c(1,3))
plot.Gam(anv3, se = TRUE, col = 'red')

set.seed(1)
cv.errors = rep(NA, 7)
for (i in 1:7){
  fit = glm(mpg ~ poly(weight, i), data = Auto)
  cv.errors[i] = cv.glm(Auto, fit)$delta[1]
}

plot(1:7, cv.errors, xlab = "Degree", ylab = "Test MSE", main = "mpg vs. weight")
deg.min = which.min(cv.errors)
points(deg.min, cv.errors[deg.min], col = "red", pch = 19, cex = 1)

# A second-degree polynomial provides the minimum test MSE for predicting mpg with weight.

# Compare the fits for different degree polynomials between mpg and weight with an ANOVA test.
fit1 = lm(mpg ~ weight, data = Auto)
fit2 = lm(mpg ~ poly(weight, 2), data = Auto)
fit3 = lm(mpg ~ poly(weight, 3), data = Auto)
fit4 = lm(mpg ~ poly(weight, 4), data = Auto)
anova(fit1, fit2, fit3, fit4)

# Plot of the 3-degree polynomial
fit = lm(mpg ~ poly(weight, 2), data = Auto)
weightlims = range(weight)
weight.grid = seq(from=weightlims[1], to=weightlims[2])
preds = predict(fit, newdata = list(weight = weight.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(weight, mpg, col = "darkgrey")
lines(weight.grid, preds$fit, lwd = 2, col = "blue")
matlines(weight.grid, se.bands, col = "blue", lty=2)

# Explore the relationships between other predictor variables and mpg
plot(acceleration, mpg)
plot(horsepower, mpg)
plot(cylinders, mpg)

fit = glm(I(mpg>30) ~ poly(horsepower, 3), data = Auto, family = binomial)
summary(fit)
hplims = range(horsepower)
hp.grid = seq(from=hplims[1], to=hplims[2])
preds = predict(fit, list(horsepower = hp.grid), se=TRUE)
se.bands = preds$fit + cbind(fit=0, lower=-2*preds$se, upper=+2*preds$se)
se.bands[1:5, ]
prob.bands = exp(se.bands) / (1 - exp(se.bands))
matplot(hp.grid, prob.bands, col = "blue", lwd = c(2,1,1), lty=c(1,2,2), type="l", ylim=c(0,0.1))
points(jitter(horsepower), I(mpg>30)/10, pch="l", cex=0.5)

# Create a plot using splines to predict mpg with acceleration
require(splines)
fit = lm(mpg ~ bs(acceleration, knots=c(12,18,24)), data = Auto)
acceleration.lims = range(acceleration)
acceleration.grid = seq(from=acceleration.lims[1], to=acceleration.lims[2])
plot(acceleration, mpg, col = "darkgrey")
lines(acceleration.grid, predict(fit, list(acceleration = acceleration.grid)), col = "darkgreen", lwd=2)
abline(v=c(12,18,24), lty=2, col = "darkgreen")
summary(fit)

# Generalized Additive Models for the `Auto` data set
require(gam)
gam1 = gam(mpg ~ s(weight, df=4) + s(acceleration, df=4) + cylinders, data = Auto)
par(mfrow=c(1,3))
plot(gam1, se=TRUE)
gam2 = gam(mpg ~ s(weight, df=4) + s(acceleration, df=4) + cylinders + horsepower, data = Auto)

# Check if we need a nonlinear term for horsepower with a Chi-Square test.
anova(gam1, gam2, test = "Chisq")

# Based on the significance of the p-value, a nonlinear term is needed for horsepower.


# Question 9

# This question uses the variables dis (the weighted mean of distances
# to five Boston employment centers) and nox (nitrogen oxides concentration in parts per 10 million) 
# from the Boston data.

# a) Use the poly() function to fit a cubic polynomial regression to
# predict nox using dis. Report the regression output, and plot
# the resulting data and polynomial fits.
library(ISLR2)
library(splines)
attach(Boston)
names(Boston)
set.seed(1)

# One way to plot
fit1 = lm(nox ~ poly(dis, 3), data = Boston)
summary(fit1)
dislims = range(dis)
dis.grid = seq(from=dislims[1], to=dislims[2])
preds = predict(fit1, newdata = list(dis = dis.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se, preds$fit - 2*preds$se)
plot(dis, nox, col = "darkgrey")
lines(dis.grid, preds$fit, col = "red", lwd = 2)
matlines(dis.grid, se.bands, col = "red", lty = 2)

# Another way to plot
plot(dis, nox, col = "darkgrey")
points(Boston$dis, fit1$fitted.values, col = "red")


# b) Plot the polynomial fits for a range of different polynomial
# degrees (say, from 1 to 10), and report the associated residual sum of squares.
degree = 10
rss = rep(NA, degree)

for (i in 1:degree){
  fit = lm(nox ~ poly(dis, i), data = Boston)
  rss[i] = sum(fit$residuals ^ 2)
}
plot(1:degree, rss, xlab = "Degree", ylab = "RSS")

# c) Perform cross-validation or another approach to select the optimal degree for the polynomial,
# and explain your results.

mse = rep(NA, degree)

for (i in 1:degree){
  fit = glm(nox ~ poly(dis, i), data = Boston)
  mse[i] = cv.glm(Boston, fit, K = 10)$delta[1]
}

plot(1:degree, mse, xlab = "Degree", ylab = "Test MSE", type = "l")
points(which.min(mse), mse[which.min(mse)], col = "red", pch = 19, cex = 1)
# Cross-validation selects the fourth degree polynomial with the minimum amount of test MSE.

# d) Use the bs() function to fit a regression spline to predict nox using dis. 
# Report the output for the fit using four degrees of freedom. How did you choose the knots? 
# Plot the resulting fit.
spline.fit = lm(nox ~ bs(dis, df = 4), data = Boston)
attr(bs(Boston$dis, df = 4), "knots")

preds = predict(spline.fit, newdata = list(dis = dis.grid), se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
plot(dis, nox, col = "darkgrey")
lines(dis.grid, preds$fit, col = "red", lwd = 2)
matlines(dis.grid, se.bands, col = "red", lty = 2)

# Now fit a regression spline for a range of degrees of freedom, and
# plot the resulting fits and report the resulting RSS. 
# Describe the results obtained.
res = c()
df.range = 3:16
for (dof in df.range) {
  fit = lm(nox ~ bs(dis, df = dof), data = Boston)
  res = c(res, sum(fit$residuals ^ 2))
}
plot(df.range, res, type = 'l', xlab = 'degree of freedom', ylab = 'RSS')

# 10 degrees of freedom is good enough based on the plot.

# f) Perform cross-validation or another approach in order to select 
# the best degrees of freedom for a regression spline on this data.
# Describe your results.

res = c()
for (dof in df.range) {
  fit = glm(nox ~ bs(dis, df = dof), data = Boston)
  testMSE = cv.glm(Boston, fit, K = 10)$delta[1]
  res = c(res, testMSE)
}

plot(df.range, res, type = 'l', xlab = 'Degree of Freedom', ylab = 'Test MSE')
points(which.min(res) + 2, res[which.min(res)], col = "red", pch = 19, cex = 1)

# Question 10

# a) Split the data into a training set and a test set.
attach(College)
names(College)
library(caTools)

set.seed(1)
spl = sample.split(College$Outstate, SplitRatio = 0.5)

train = subset(College, spl == TRUE)
test = subset(College, spl == FALSE)

# Using out-of-state tuition as the response and the other variables as the predictors,
# perform forward stepwise selection on the training set in order to identify a satisfactory model 
# that uses just a subset of the predictors.
library(leaps)
regfit.fwd = regsubsets(Outstate ~ ., data = train, nvmax = 17, method = "forward")
regsummary = summary(regfit.fwd)

which.min(regsummary$cp)
which.min(regsummary$bic)
which.min(regsummary$rss)
which.max(regsummary$adjr2)

par(mfrow = c(2, 2))
plot(regsummary$cp, xlab = "Number of Variables", ylab = "Cp", type = "b")
points(10, regsummary$cp[10], col = "red", pch = 19)
plot(regsummary$bic, xlab = "Number of Variables", ylab = "BIC", type = "b")
points(6, regsummary$bic[6], col = "red", pch = 19)
plot(regsummary$rss, xlab = "Number of Variables", ylab = "RSS", type = "b")
points(17, regsummary$rss[17], col = "red", pch = 19)
plot(regsummary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", type = "b")
points(11, regsummary$adjr2[11], col = "red", pch = 19)

# The model with 14 predictor variables has the minimum Cp and the maximum adjusted R^2
coef(regfit.fwd, 10)
coef(regfit.fwd, 6)

# b) Fit a GAM on the training data, using out-of-state tuition as
# the response and the features selected in the previous step as
# the predictors. Plot the results, and explain your findings.

gam.mod = gam(Outstate ~ Private + s(Room.Board, 5) + s(Terminal, 5)
              + s(perc.alumni, 5) + s(Expend, 5) + s(Grad.Rate, 5), data = train)
par(mfrow = c(2, 3))
plot(gam.mod, se = TRUE, col = "orange")

# Possible linear relation between Outstate and Room.Board, perc.alumni.
# Non-linear relation between Outstate and Grad.Rate and Expend.

# c) Evaluate the model obtained on the test set, and explain the results obtained.
set.seed(1)
pred = predict(gam.mod, newdata = test)
RSS = sum((test$Outstate - pred)^2)
TSS = sum((test$Outstate - mean(test$Outstate))^2)
R2 = 1 - RSS / TSS
R2

# d) For which variables, if any, is there evidence of a non-linear relationship with the response?
summary(gam.mod)

# “Anova for Nonparametric Effects” shows Expend has strong non-linear relationshop with the Outstate.
