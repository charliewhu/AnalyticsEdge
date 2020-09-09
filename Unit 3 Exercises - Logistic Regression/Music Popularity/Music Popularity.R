
setwd("Unit 3 Exercises - Logistic Regression/Music Popularity")
pacman::p_load(tidyverse, caTools, ROCR)

songs = read.csv("songs.csv")
table(songs$year) #373 songs in 2010


MichaelJackson = subset(songs, artistname=="Michael Jackson")
nrow(MichaelJackson) #18 songs by MJ
MichaelJackson[c("songtitle", "Top10")] # 5 top 10s


table(songs$timesignature) #only 0,1,3,4,5,7 min songs
#4 mins is the most common discrete time signature
which.max(songs$tempo) #highest tempo song is row 6206
songs$songtitle[6206] #Wanna be startin something


SongsTrain = subset(songs, songs$year <=2009) #splitting data
SongsTest = subset(songs, songs$year > 2009)

nrow(songs) == nrow(SongsTest) + nrow(SongsTrain) #check to see split was correct


##remove vars before modelling
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]


## we want to predict whether a song with be Top10
## create model and show summary
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1) #AIC is 4827.2

cor(SongsTrain$loudness, SongsTrain$energy) # multicollinearity issue with these variables
#correlation is >0.7

# new model with loudness removed
SongsLog2 = glm(Top10 ~ . -loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
#energy coefficient has changed to positive


# new model with energy removed
SongsLog3 = glm(Top10 ~ . -energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
#loudness still has positive coefficient

#predictions using SongsLog3
testPredict = predict(SongsLog3, newdata=SongsTest, type="response")

table(SongsTest$Top10, testPredict >= 0.45) #confusion matrix
(309+19)/(309+5+40+19) #accuracy is 0.879


table(SongsTest$Top10)
314/(314+59) #baseline is 0.842


table(SongsTest$Top10, testPredict >= 0.45) #19 true postives and 5 false positives at 0.45 threshold


19/(19+40) #sensitivity or True Positive Rate is 0.322
309/(309+5) #specificity or False Positive Rate is 0.984

#our model has very high specificity
#detects less than half of the Top10 songs
#but we are very confident when it predicts one







