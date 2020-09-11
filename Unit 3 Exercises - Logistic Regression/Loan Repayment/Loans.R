

## Predicting 3-year loan repayment

setwd("Unit 3 Exercises - Logistic Regression/Loan Repayment")
pacman::p_load(tidyverse, caTools, ROCR, mice)

#read data
loans = read.csv("loans.csv")
str(loans)
summary(loans)


table(loans$not.fully.paid)
1533/(1533+8045) #16% not fully paid back


#find missing values
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing) #62 missing rows

table(missing$not.fully.paid) #12 missing fully paid loans
12/62 #19% of missing rows were not fully paid, similar to overall 16% rate


#imputing missing values
#imputing all missing EXCEPT not.fully.paid
#we predicted the missing values using the other independent variables per observation
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#split data into train and test sets
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split==TRUE)
test = subset(loans, split==FALSE)

#create model
mod = glm(not.fully.paid~., data=train, family="binomial")
summary(mod)

#predicted probabilities
test$predicted.risk = predict(mod, newdata=test, type="response")

#confusion matrix
table(test$not.fully.paid, test$predicted.risk>0.5)

2401/2873 #0.8357 accuracy of model
2413/2873 #0.8398 accuracy of baseline


#computing AUC
pred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(pred, "auc")@y.values) #0.672


#making bivariate model
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
cor(train$int.rate, train$fico)


pred.bivariate = predict(bivariate, newdata=test, type="response")
summary(pred.bivariate) #max predicted probability of loan delinquency is 0.4266
#no loans flagged above 0.5 cutoff

#auc of bivariate
prediction.bivariate = prediction(pred.bivariate, test$not.fully.paid)
as.numeric(performance(prediction.bivariate, "auc")@y.values)
#AUC is 0.624


#profit of $1 invested
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)


highInterest = subset(test, int.rate >= 0.15)
summary(highInterest$profit)
table(highInterest$not.fully.paid)


cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)






