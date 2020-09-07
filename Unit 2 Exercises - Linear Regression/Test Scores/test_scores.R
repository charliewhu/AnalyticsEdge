
setwd('Unit 2 Exercises - Linear Regression/Test Scores')
pisaTrain = read.csv("pisa2009train.csv") 
pisaTest = read.csv("pisa2009test.csv")

nrow(pisaTrain)
nrow(pisaTest)
#3663 students in training set
#1570 in test set

tapply(pisaTrain$readingScore, pisaTrain$male, mean) 
#avg score of 483.5 for males, 512.9 for females


summary(pisaTrain)
#most categories have at least one NA

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

nrow(pisaTrain) #2414 observations left
nrow(pisaTest) #990 observations left


#raceeth is an unordered factor with at least 3 levels
#grade is an ordered factor with at least 3 levels


## we must refactor our ethnicities as binary variables. 
## because white is the most common, we use this as the reference level

#make raceeth a factor
pisaTrain$raceeth = as.factor(pisaTrain$raceeth)
pisaTest$raceeth = as.factor(pisaTest$raceeth)

#relevel the factor with white as reference level
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")


LinReg = lm(readingScore ~ ., data=pisaTrain)
summary(LinReg)
#r2 is 0.3251


SSE = sum(LinReg$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE #73.36555

sqrt(mean(LinReg$residuals^2)) #alternative calculation


#Consider two students A and B. They have all variable values the same, 
#except that student A is in grade 11 and student B is in grade 9. 
#What is the predicted reading score of student A minus the predicted reading score of student B?

#coefficient for grade is 29.54
#difference of 2 grades means reading score differs by
29.54*2


## prediciton
predTest = predict(LinReg, newdata = pisaTest)
summary(predTest)

SSE = sum((predTest-pisaTest$readingScore)^2)
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2))

SSE #5762082
RMSE #76.29079


# predicted test score using training model
baseline = mean(pisaTrain$readingScore)
baseline #517.96
SST = sum((baseline-pisaTest$readingScore)^2)
SST #7802354

testr2 = 1 - SSE/SST
testr2 #0.2614944




