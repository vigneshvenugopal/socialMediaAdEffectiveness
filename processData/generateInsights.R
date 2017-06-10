library(tm)
library(qdap)
library(openNLP)
library(ggplot2)
library(wordcloud)
library(syuzhet)
library(pander)
library(topicmodels)
library(igraph)
library(tidytext)
library(dplyr)
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/cleanSocialMediaText.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/plotTextMetrics.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/posTagging.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/sentimentAnalysis.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/topicModelling.R")

###################
# Loading the data
###################

twitterData <- read.csv("RivaahBridesByTanishq_twitter_Tweets.csv")
tweets <- as.data.frame(twitterData$text)
tweets$source<-1
colnames(tweets)[1] <- "text"
facebookData <- read.csv("RivaahBridesByTanishq_Facebook_Comments.csv")
fbComments <- as.data.frame(facebookData$comment_message)
fbComments$source <- 2
colnames(fbComments)[1] <- "text"
youTubeData <- read.csv("RivaahBridesByTanishq_YouTube_Video_Comments.csv")
youTubeComments <- as.data.frame(youTubeData$text)
youTubeComments$source<-3
colnames(youTubeComments)[1] <- "text"
completeData <- rbind(tweets, fbComments, youTubeComments)


##############################
# Setting up the input params
##############################

# Setup the below mentioned params for each datasource (i.e., completeData, Facebook, Twitter & YouTube),
# And execute the data processing code below available below
inputData <- fbComments$text
myStopWords <- c(stopwords("SMART"),"http","https","bitly","sticke","www","pictwittercom","bhi","pictwitter","com","shavetjain","youtube","pakistan","zeevekadlm","ghaywan","singh","sharma","such","sticker")
wordFrequency <- 10
correlationValue <- 0.1
associationWord <- "emotional"
associationValue <- 0.1
wordCloudMaxWords <- 100
wordCloudMinFreq <- 7
corrWord <- 'beautiful'

#####################################
# Process data and generate insights
#####################################

# Cleaning the Data
corpusList <- cleanSocialMediaText(inputData, myStopWords)
myTmCorpus <- corpusList[[1]]
myCorpus <- corpusList[[2]]

# POS Tagging
posDataReturnList <- posTagging(myTmCorpus, myCorpus)
posTaggedData <- posDataReturnList[[1]]
myWordList <- posDataReturnList[[2]]
myTmCorpusWordList <- posDataReturnList[[3]]
myVCorpus <- posDataReturnList[[4]]

# Plotting metrics for the text
plotReturnList <- plotTextMetrics(myTmCorpusWordList, myVCorpus, wordFrequency, correlationValue, associationWord, associationValue, wordCloudMaxWords, wordCloudMinFreq, corrWord)
plotReturnList[[1]]
plotReturnList[[2]]

# Sentiment Analysis
sentimentAnalysis(myWordList)

# Topics Modelling
topicModelling(posTaggedData, myTmCorpusWordList)