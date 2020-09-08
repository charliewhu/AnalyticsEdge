
library(tidyverse)
setwd('Unit 2 Exercises - Linear Regression/Flu')
FluTrain = read.csv("FluTrain.csv")


subset(FluTrain, ILI == max(ILI))
# week 18-24 oct 2009 has highest number of ILI visits

subset(FluTrain, Queries == max(Queries))
# same week has msot queries


ggplot(FluTrain, aes(x=ILI)) +
  geom_histogram(bins=10)
# right skew, mostly small numbers of ILI visits


qplot(FluTrain$Queries, FluTrain$ILI)
#positive relationship, ILI visits increase exponentially


qplot(FluTrain$Queries, log(FluTrain$ILI))
#positive linear relationship


## we can make a linear model in the form log(ILI) = m(Queries) + c
## the coefficient m will be positive



model1 = lm(log(ILI) ~ Queries, data=FluTrain)
summary(model1)
#r2 is 0.709


corr = cor(FluTrain$Queries, log(FluTrain$ILI))
corr^2
#r2 is equal to the correlation of the variables squared


FluTest = read.csv("FluTest.csv")

#prediction model needs exponent
PredTest1 = exp(predict(model1, newdata=FluTest))


# need estimate for percentage of ILI visits in week 11th march 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17") #find rownumber of the week
PredTest1[11] #2.187378


# relative error
# (Observed - Estimate)/Observed

(FluTest$ILI[11] - PredTest1[11]) / FluTest$ILI[11]
#0.04623827


SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE #0.7490645

sqrt(mean((PredTest1-FluTest$ILI)^2)) #alternative calc for RMSE


### creating time series model

pacman::p_load(zoo)
library(zoo)


#creating lagged variable as the data usually takes 2 weeks to load
ILILag2 = stats::lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

summary(ILILag2)
# 2 NAs

qplot(log(FluTrain$ILILag2), log(FluTrain$ILI))
# very strong positive relationship


FluTrend2 = lm(log(ILI)~Queries+log(ILILag2), data=FluTrain)
summary(FluTrend2)
#r2 is 0.9063


testILILag2 = stats::lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(testILILag2)
summary(FluTest$ILILag2)
# 2 NAs again of course

nrow(FluTrain)
# filling training NAs with the last two values from test set
FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

# test set predictions
PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

SSE = sum((PredTest2-FluTest$ILI)^2)
SSE #4.500877

#RMSE
sqrt(mean((PredTest2-FluTest$ILI)^2)) #0.294

# 2nd model has lower RMSE and is a better model
# successful ARIMA model built









