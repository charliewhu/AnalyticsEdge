
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


##create model and show summary
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)

summary(SongsLog1) #AIC is 4827.2
