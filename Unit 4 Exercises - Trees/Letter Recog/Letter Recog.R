
## Letter recognition using data
## multiclass classification problem

scipen=999
setwd("Unit 4 Exercises - Trees/Letter Recog")
pacman::p_load(tidyverse, caTools, ROCR, rpart, randomForest, caret, e1071, rattle)

letters = read.csv("letters_ABPR.csv")

#Predicting whether letter is B or not
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

table(train$isB) #baseline method predicts 'not B' every time

table(test$isB) 
1175/(1175+383) #baseline accuracy on test set is 0.754

# Classification tree to predict B or not B
# remove letter first as this is our response variable
CARTb = rpart(isB ~ . - letter, data=train, method="class")

# make predictions on test set
predictions = predict(CARTb, newdata=test, type="class")
table(test$isB, predictions)
(1118+340)/nrow(test) #0.9358 accuracy

## random forest model
set.seed(1000)
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB, predictions)
(1163+374)/nrow(test) #0.9865 accuracy

## CART on all letters

letters$letter = as.factor(letters$letter) #convert to factor
set.seed(2000)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train2 = subset(letters, spl == TRUE)
test2 = subset(letters, spl == FALSE)
table(train2$letter) # baseline is to predict P for all letters
table(test2$letter) 
401/nrow(test) #0.257 accuracy for baseline of predicting always P

CARTletter = rpart(letter ~ . - isB, data=train2, method="class")
predictLetter = predict(CARTletter, newdata=test2, type="class")
table(test2$letter, predictLetter)
(348+318+363+340)/nrow(test2) #0.87869 accuracy

## Random forest on all letters

RF = randomForest(letter ~ . - isB, data=train2)
predictRF = predict(RF, newdata=test2)
table(test2$letter, predictRF)
(390+379+393+366)/nrow(test2) #0.9807 accuracy

# huge gain in accuracy for random forest vs tree




