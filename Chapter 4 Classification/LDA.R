# Linear Discriminant Analysis (LDA)

library(MASS)
train = Smarket$Year < 2005
Smarket.2005 = Smarket[!train, ]
Direction.2005 = Direction[!train]
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket,
              subset = train)
lda.fit
plot(lda.fit)


lda.pred = predict(lda.fit, Smarket.2005)

# class contains LDA's predictions about the movement of the market
# posterior is a matrix whose kth column contains the posterior probability
# that the corresponding observation belongs to the kth class
# x contains the linear discriminants
names(lda.pred)

# The LDA and logistic regression predictions are almost identical
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

# Output corresponds to the probability that the market will decrease
sum(lda.pred$posterior[, 1] >= 0.5)
sum(lda.pred$posterior[, 1] < 0.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]

