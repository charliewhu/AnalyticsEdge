

## Understanding why people vote
## use both logistic regression and classification trees to analyze voter intention

##initialise wd and libraries
scipen=999
setwd("Unit 4 Exercises - Trees/Voting")
pacman::p_load(tidyverse, caTools, ROCR, rpart, randomForest, caret, e1071, rattle)

##read and summarise data
gerber = read.csv("gerber.csv")
summary(gerber)
table(gerber$voting)
108696/(108696+235388) # 31% voted


tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)
# neighbours had the largest percentage of voters


##logistic regression model
log1 = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family=binomial)
summary(log1) # all variables are significant

predictLog = predict(log1, type="response")

table(gerber$voting, predictLog > 0.3)
(134513+51966)/(134513+100875+56730+51966) #accuracy of 54% at 0.3 threshold

table(gerber$voting, predictLog > 0.5)
(235388+0)/(235388+108696) #accuracy of 68% at 0.5 threshold

#AUC
ROCRpred = prediction(predictLog, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values) #0.53
#weak predictive model


## tree
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
fancyRpartPlot(CARTmodel) #only have root node

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
fancyRpartPlot(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
fancyRpartPlot(CARTmodel3)

# focusing on control group
CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
CARTsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
fancyRpartPlot(CARTcontrol, digits=6)
fancyRpartPlot(CARTsex, digits=6)

# from second tree, we see not being in control group affects men and women the same
# women is 0.290456 vs 0.334176
# men is 0.302795 vs 0.345818
# differences of 0.04372 vs 0.04302


## logistic regression
log2 = glm(voting ~ control + sex, data=gerber, family=binomial)
summary(log2)
# coef for sex is negative reflecting women being less likely to vote


# create df with all combos of control and sex (2x2)
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(log2, newdata=Possibilities, type="response")
# same perspective as bottom rung on CARTsex tree


# adding term for combination of control and sex
log3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(log3)
# woman AND control causes chances of voting to decrease, show by negative coef of sex:control
# same as tree


predict(log3, newdata=Possibilities, type="response")
# now an almost zero difference from CARTsex tree bottom rung






