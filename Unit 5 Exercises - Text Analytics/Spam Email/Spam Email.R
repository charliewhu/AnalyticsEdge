

## Building and evaluating a spam filter using
## dataset from "Spam Filtering with Naive Bayes -- Which Naive Bayes?"
## The Dataset has two fields: text and spam,
## text is the email text, spam is a binary variable.

##load packages and read data
scipen=999
setwd("Unit 5 Exercises - Text Analytics/Spam Email")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle, tm, SnowballC)

emails = read.csv("emails.csv", stringsAsFactors=FALSE)

str(emails) #5728 emails
table(emails$spam) #1368 are spam

emails$text[1]
emails$text[1000]
#all text starts with the word 'subject'

max(nchar(emails$text)) #43,952 characters
min(nchar(emails$text)) #13 characters
which.min(nchar(emails$text)) #row 1992


## Preparing the corpus

corpus = VCorpus(VectorSource(emails$text))
corpus = corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stemDocument)

dtm = DocumentTermMatrix(corpus)
dtm #28687 terms

#only non sparse terms
spdtm = removeSparseTerms(dtm, 0.95)
spdtm #330 terms


emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse)) #enron is the most common term

#add spam column back in
emailsSparse$spam = emails$spam



## Model building

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)
spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, spl==TRUE)
test = subset(emailsSparse, spl==FALSE)

#logistic regression
spamLog = glm(spam ~ ., data=train, family = "binomial")
summary(spamLog) #no variables significant at p=0.05 level

#CART
spamCART = rpart(spam ~ ., data=train, method="class")
fancyRpartPlot(spamCART) #'enron' and 'vinc' appear, highly personal to this dataset
# this will affect generlisability of the model

#Random Forest
spamRF = randomForest(spam~., data=train)



## Training set predictions 
predTrainLog = predict(spamLog, type="response")
predTrainCART = predict(spamCART)[,2]
predTrainRF = predict(spamRF, type="prob")[,2]

#accuracy of logistic regression
table(train$spam, predTrainLog>0.5)
(3052+954)/nrow(train) #0.999 accuracy on training set

#AUC of logistic regression
predictionTrainLog = prediction(predTrainLog, train$spam)
as.numeric(performance(predictionTrainLog, "auc")@y.values) #0.99999


#accuracy of CART
table(train$spam, predTrainCART > 0.5)
(2885+894)/nrow(train) #0.942 on training set

#AUC of CART
predictionTrainCART = prediction(predTrainCART, train$spam)
as.numeric(performance(predictionTrainCART, "auc")@y.values) #0.9696


#accuracy of Random Forest
table(train$spam, predTrainRF > 0.5)
(3016+919)/nrow(train) #0.98

#AUC of RF
predictionTrainRF = prediction(predTrainRF, train$spam)
as.numeric(performance(predictionTrainRF, "auc")@y.values) #0.998



## Test set predictions
predTestLog = predict(spamLog, newdata=test, type="response")
predTestCART = predict(spamCART, newdata=test)[,2]
predTestRF = predict(spamRF, newdata=test, type="prob")[,2]

#Logistic 
table(test$spam, predTestLog > 0.5)
(1257+376)/nrow(test) #0.9505 accuracy

predictionTestLog = prediction(predTestLog, test$spam)
as.numeric(performance(predictionTestLog, "auc")@y.values) #0.9627 AUC


#CART
table(test$spam, predTestCART > 0.5)
(1228+386)/nrow(test) #0.9394 accuracy

predictionTestCART = prediction(predTestCART, test$spam)
as.numeric(performance(predictionTestCART, "auc")@y.values) #0.9631 AUC


#RandomForest
table(test$spam, predTestRF > 0.5)
(1291+385)/nrow(test) #0.9756 accuracy

predictionTestRF = prediction(predTestRF, test$spam)
as.numeric(performance(predictionTestRF, "auc")@y.values) #0.9976 AUC


## Logistic regression shows a lot of overfitting. 
## CART and RF has similar accuracy/AUC on training and test
## LogReg fell off significantly as it was overfit to the training set
















