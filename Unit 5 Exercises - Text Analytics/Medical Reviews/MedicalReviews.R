
## Retrieving reviews automatically, predicting whether a review is a clinical trial

##load packages and read data
scipen=999
setwd("Unit 5 Exercises - Text Analytics/Medical Reviews")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle, tm, SnowballC)

trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
summary(trials)

summary(nchar(trials$abstract)) #longest abstract is 3708 characters
table(nchar(trials$abstract) == 0) #112 search results with no abstract

which.min(nchar(trials$title)) #shortest title is 1258th
trials$title[1258]


## preprocessing
corpusTitle = VCorpus(VectorSource(trials$title)) #convert title to corpus
corpusTitle = tm_map(corpusTitle, content_transformer(tolower)) #convert to lowercase
corpusTitle = tm_map(corpusTitle, removePunctuation) #remove punctuation
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english")) #remove english stopwords
corpusTitle = tm_map(corpusTitle, stemDocument) #stem words
dtmTitle = DocumentTermMatrix(corpusTitle) #create document term matrix
dtmTitle = removeSparseTerms(dtmTitle, 0.95) #Limit to terms which appear in at least 5% of documents
dtmTitle = as.data.frame(as.matrix(dtmTitle)) #convert to dataframe

#repeat for trials$abstract
corpusAbstract = VCorpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower)) #convert to lowercase
corpusAbstract = tm_map(corpusAbstract, removePunctuation) #remove punctuation
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english")) #remove english stopwords
corpusAbstract = tm_map(corpusAbstract, stemDocument) #stem words
dtmAbstract = DocumentTermMatrix(corpusAbstract) #create document term matrix
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95) #Limit to terms which appear in at least 5% of documents
dtmAbstract = as.data.frame(as.matrix(dtmAbstract)) #convert to dataframe

which.max(colSums(dtmAbstract)) #patient is the most common word stem across all abstracts

#Ensure colnames aren't the same after combining
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract) #combine Title and Abstract into single df
dtm$trial = trials$trial #adding the dependent variable
ncol(dtm) #367 columns

## building model
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, spl==TRUE)
test = subset(dtm, spl==FALSE)

table(train$trial)
730/nrow(train) #0.56 accuracy

trialCART = rpart(trial ~ ., data=train, method="class")
fancyRpartPlot(trialCART)

#training set predictions
predTrain = predict(trialCART)[,2]
summary(predTrain) #0.87 is the max predicted probability for any result
table(train$trial, predTrain >= 0.5)
(631+441)/nrow(train) #training set accuracy of 0.823
441/(441+131) #0.771 sensitivity
631/(631+99) #0.864 specificity

#test set predictions
predTest = predict(trialCART, newdata=test)[,2]
table(test$trial, predTest >= 0.5)
(261+161)/nrow(test) #0.758 accuracy

#generate AUC
pred = ROCR::prediction(predTest, test$trial)
as.numeric(performance(pred,"auc")@y.values) #0.837


















