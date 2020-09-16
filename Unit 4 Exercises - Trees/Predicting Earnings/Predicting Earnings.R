
## Predicitng earnings in US from Census Data

#load packages and read data
scipen=999
setwd("Unit 4 Exercises - Trees/Predicting Earnings")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle)

census = read.csv("census.csv")

## Logistic Regression Model
## predicitng if earnings over 50k
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

logMod = glm( over50k ~ . , family="binomial", data = train)
summary(logMod)

predictTest = predict(logMod, newdata=test, type="response")
table(test$over50k, predictTest>= 0.5)
(9051+1888)/nrow(test) #accuracy of 0.855

table(test$over50k)
9713/nrow(test) #baseline accuracy is 0.759 (always predicting <=50k)

#AUC
ROCpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCpred, "auc")@y.values) #0.9061


## CART model
censustree = rpart(over50k ~., method="class", data=train)
fancyRpartPlot(censustree) # 4 splits

#predictions using tree
predictTest = predict(censustree, newdata = test, type = "class")
table(test$over50k, predictTest)
(9243+1596)/nrow(test) #0.8474
#less accurate than logistic regression but much easier to interpret

#view ROC curve and AUC for model
predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, test$over50k)
perf = performance(ROCRpred, "tpr", "fpr")
plot(perf)
as.numeric(performance(ROCRpred, "auc")@y.values) #0.847


## Random Forest Model
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
censusrf = randomForest(over50k ~ . , data = trainSmall)
predictTest = predict(censusrf, newdata=test)
table(test$over50k, predictTest)
(9614+1050)/nrow(test) #0.8337 accuracy

#viewing number of time variable selected for a split
vu = varUsed(censusrf, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))

#impurity plot
varImpPlot(censusrf)


## Cross Validation

#k=10 folds, testing cp values from 0.002 to 0.1 in 0.002 increments
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
#optimal is cp=0.002

model = rpart(over50k~., data=train, method="class", cp=0.002)
predictTest = predict(model, newdata=test, type="class")

table(test$over50k, predictTest)
(9178+1838)/nrow(test) #accuracy 0.8612

fancyRpartPlot(model)

# now 18 splits on the tree for a 1% increase in accuracy!
# probably not worth the decrease in interpretability 












