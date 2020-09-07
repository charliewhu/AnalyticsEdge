library(tidyverse)

data = read.csv("Unit 2 Exercises - Linear Regression/Climate Change/climate_change.csv")

str(data)
summary(data)

#split data
train = subset(data, data$Year<=2006)
test = subset(data, data$Year>2006)


#first model using all variables
model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(model1)

#r2 is 0.7509
#CH4 and N2O not significant

cor(train)
#N2O highly correlated with CO2, CH4, CFC.12
#CFC.11 highly correlated with CH4 and CFC.12


#model with focus on N2O
model2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data=train)
summary(model2)

#N2O coefficient is 0.02532 compared to -0.01653 in the full model
#r2 is 0.7261

#creating step model
step_model = step(model1)
summary(step_model)
#r2 is 0.7508 by removing CH4


##testing on unseen data
model_predict = predict(step_model, newdata = test)
SSE= sum((model_predict - test$Temp)^2) # sum of squared errors
SST= sum((mean(train$Temp) - test$Temp)^2) # total sum of squares
R2 = 1-SSE/SST 
R2 #0.6286



