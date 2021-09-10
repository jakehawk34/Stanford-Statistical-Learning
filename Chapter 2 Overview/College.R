college = College
View(college)

rownames(college) = college[, 1]
View(college)

summary(college)
pairs(college[, 1:10])
boxplot(college$Outstate ~ college$Private)

Elite <- rep ("No", nrow (college))
Elite[college$Top10perc > 50] <- " Yes "
Elite <- as.factor(Elite)
college <-data.frame(college , Elite)

summary(college$Elite)
boxplot(college$Outstate ~ college$Elite)

par(mfrow = c(2, 2))
hist(college$PhD)
hist(college$Grad.Rate)
hist(college$S.F.Ratio) 
hist(college$Expend)
