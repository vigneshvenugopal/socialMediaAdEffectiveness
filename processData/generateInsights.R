# required pakacges

library(tm)
library(qdap)
library(openNLP)
library(ggplot2)
library(wordcloud)
library(syuzhet)
library(pander)
library(topicmodels)
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/cleanSocialMediaText.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/plotWordMetrics.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/posTagging.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/sentimentAnalysis.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/topicModelling.R")

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
finalData <- rbind(tweets, fbComments, youTubeComments)

myStopWords<- c(stopwords("SMART"),"http","https","bitly","sticke","www","pictwittercom","bhi","pictwitter","com","shavetjain","youtube","pakistan","zeevekadlm","ghaywan","singh","sharma","such","sticker")
wordFrequency = 10
correlationValue = 0.1
associationWord = "emotional"
associationValue = 0.13
wordCloudMaxWords = 100

myCorpus <- cleanSocialMediaText(finalData$text, myStopWords)

returnList <- posTagging(myCorpus)
posTaggedData <- returnList[[1]]
myWordList <- returnList[[2]]
myCorpusWordList <- returnList[[3]]

plotWordMetrics(myCorpus, myCorpusWordList, wordFrequency, correlationValue, associationWord, associationValue, wordCloudMaxWords)

sentimentAnalysis(myWordList)

topicModelling(posTaggedData, myCorpusWordList)

