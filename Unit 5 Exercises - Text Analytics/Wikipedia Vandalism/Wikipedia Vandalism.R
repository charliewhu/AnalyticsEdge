
## Detecting vandalism of wikipedia pages

#load packages and read data
scipen=999
setwd("Unit 5 Exercises - Text Analytics/Wikipedia Vandalism")
pacman::p_load(tidyverse, caTools, ROCR, rpart, rpart.plot, randomForest, caret, e1071, rattle, tm, SnowballC)

wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal) #1815 cases of vandalism

#create document term matrix from Added column
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded #6675 terms

#filter out sparse terms 
sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded #now 166 terms

#prepend sparseAdded with A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

#repeat for removed
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved) #162 words

#combine all words
wikiWords = cbind(wordsAdded, wordsRemoved) 
wikiWords$Vandal = wiki$Vandal

#create train and test splits
set.seed(123)
spl = sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain = subset(wikiWords, spl==TRUE)
wikiTest = subset(wikiWords, spl==FALSE)

table(wikiTest$Vandal)
618/nrow(wikiTest) #0.531 baseline accuracy

## Build CART model
wikiCART = rpart(Vandal ~ ., data=wikiTrain, method="class")
testPredictCART = predict(wikiCART, newdata=wikiTest, type="class")
table(wikiTest$Vandal, testPredictCART)
(618+12)/nrow(wikiTest) #0.5417 accuracy

fancyRpartPlot(wikiCART)
# bag of words not very predictive vs baseline


#checking to see if people are adding links to webpages
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)


#CART with addition of new variable
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
wikiCART2 = rpart(Vandal ~ ., data=wikiTrain2, method="class")
testPredictCART2 = predict(wikiCART2, newdata=wikiTest2, type="class")
table(wikiTest2$Vandal, testPredictCART2)
(609+57)/nrow(wikiTest2) #accuracy of 0.573


#seeing iff number of words added is predictive in itself
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded) #4.05 avg

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vandal ~ ., data=wikiTrain3, method="class")
testPredictCART3 = predict(wikiCART3, newdata=wikiTest3, type="class")
table(wikiTest3$Vandal, testPredictCART3)
(514+248)/nrow(wikiTest3) #accuracy 0.655


#using non textual data
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vandal ~ ., data=wikiTrain4, method="class")
predictTestCART4 = predict(wikiCART4, newdata=wikiTest4, type="class")
table(wikiTest4$Vandal, predictTestCART4)
(595+241)/nrow(wikiTest4) #accuracy 0.7188

fancyRpartPlot(wikiCART4) 

#3 splits with significant accuracy improvements 










