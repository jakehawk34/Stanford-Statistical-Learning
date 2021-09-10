library(ISLR)

set.seed(4)
glm.fit = glm(default ~ income + balance, data = Default,
              family = binomial)

# Chapter 5: Question 5
# Split of 50/50
spl = sample.split(Default$default, SplitRatio = 0.5)
train = subset(Default, spl == TRUE)
test = subset(Default, spl == FALSE)

glm.fit1 = glm(default ~ income + balance, data = train,
               family = binomial)
glm.pred1 = predict(glm.fit1, newdata = test, type = "response")
table(test$default, glm.pred1 > 0.5)

1 - (4806 + 53) / dim(test)[1]

#Split of 70/30
spl = sample.split(Default$default, SplitRatio = 0.7)
train = subset(Default, spl == TRUE)
test = subset(Default, spl == FALSE)

glm.fit2 = glm(default ~ income + balance, data = train,
               family = binomial)
glm.pred2 = predict(glm.fit2, newdata = test, type = "response")
table(test$default, glm.pred2 > 0.5)

1 - (2885 + 37) / dim(test)[1]

#Split of 90/10
spl = sample.split(Default$default, SplitRatio = 0.9)
train = subset(Default, spl == TRUE)
test = subset(Default, spl == FALSE)

glm.fit3 = glm(default ~ income + balance, data = train,
               family = binomial)
glm.pred3 = predict(glm.fit3, newdata = test, type = "response")
table(test$default, glm.pred3 > 0.5)

1 - (965 + 12) / dim(test)[1]

# The test error rate on the validation set decreased
# as the fraction of observations in the training set increased.

# Now, consider a logistic regression model with a dummy variable for student
spl = sample.split(Default$default, SplitRatio = 0.5)
train = subset(Default, spl == TRUE)
test = subset(Default, spl == FALSE)
glm.student = glm(default ~ ., data = train, family = binomial)
summary(glm.student)

student.pred = predict(glm.student, newdata = test, type = "response")
table(test$default, student.pred > 0.5)

1 - (4810 + 49) / dim(test)[1]
# Including the dummy variable for student did not improve the test error rate
# comapred to the results we achieved with balance and income as the only predictors.

# Chapter 5: Question 6
# Compute estimates for the standard errors of the income and balance coefficients with two different methods.
set.seed(4)
summary(glm(default ~ income + balance, data = Default, family = binomial))

# Write a function, boot.fn()
boot.fn = function(data, index)
  coef(glm(default ~ income + balance, data = data[index,], family = binomial))

# Use the boot() function together with boot.fn() to estimate the SEs of the logistic regression coefficients
boot(Default, boot.fn, R = 200)

# The SEs estimated by the bootstrap are nearly the same as the calculated SEs from the logistic regression model.

# Chapter 5: Question 7
dim(Weekly)

# Fit a logistic regression model to predict Direction with Lag1 and Lag2
log.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)

# Fit a logistic regression model omitting the first observation of the Weekly data set
loocv.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-(1),], family = binomial)

# Compare the prediction for the first observation with the true value
ifelse(predict(loocv.fit, newdata = Weekly[1, ], type = "response") > 0.5, "Up", "Down")
Weekly$Direction[1]
# The LOOCV logistic model incorrectly classified the Direction of the first observation.

# Write a for loop from i = 1 to i = n, where n is the number of observations in the data set,
count = rep(0, dim(Weekly)[1])
for (i in 1:dim(Weekly)[1]) {
  fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-(i), ], family = binomial)
  prediction = predict(fit, newdata = Weekly[i, ], type = "response") > 0.5
  true = Weekly[i, ]$Direction == "Up"
  if (prediction != true)
    count[i] = 1
}
# Determine whether or not an error was made in predicting
# the direction for the ith observation. If an error was made,
# then indicate this as a 1, and otherwise indicate it as a 0.
sum(count)

# Obtain the LOOCV estimate for the test error.
mean(count)
