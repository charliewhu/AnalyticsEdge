
# predicting parole violators

setwd("Unit 3 Exercises - Logistic Regression/Parole Violators")
pacman::p_load(tidyverse, caTools, ROCR)

parole = read.csv("parole.csv")


str(parole)
summary(parole)

table(parole$violator) #78 parole violators

#converting to factors
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
summary(parole$state)
summary(parole$crime)

#split data
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split==TRUE)
test = subset(parole, split==FALSE)

#create model
mod = glm(violator ~ ., data=train, family=binomial)
summary(mod) #race, state4, multiple.offenses are significant

#apply predictions
pred = predict(mod, newdata=test, type="response")
summary(pred) #max predicted probability of violation == 0.907


#evaluation on test set using threshold of 0.5
table(test$violator, pred > 0.5)

(167+12)/202 #accuracy of 0.886
12/(12+11) #sensitivity of 0.522
167/(167+12) #specificity of 0.933

table(test$violator)
179/202 #baseline model accuracy is 0.886


# more weight assigned to False Negatives
# parole board wouldn't want to release someone
# who violates parole.
# Decreasing threshold decreases false negatives.

# this model is of value as it has 11 false negatives 
# versus 23 false negatives in baseline model


pred2 = prediction(pred, test$violator)
as.numeric(performance(pred2, "auc")@y.values) #AUC is 0.894












