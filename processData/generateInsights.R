# required pakacges

library(tm)
library(qdap)
library(openNLP)
library(ggplot2)
library(wordcloud)
library(syuzhet)
library(pander)
library(topicmodels)

# library(twitteR)
# library(plyr)
# library(RColorBrewer)
# library(Rstem)
# library(readr)
# library("SnowballC")
# library("RCurl")
# library(rvest)
# library(dplyr)
# library("textcat")
# library(stringr)
# library(openNLP)
# library(NLP)


source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/cleanSocialMediaText.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/plotWordMetrics.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/posTagging.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/sentimentAnalysis.R")
source("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/functions/topicModelling.R")

Twitter_Data <- read.csv("RivaahBridesByTanishq_Twitter_Tweets.csv")
Twitter<-as.data.frame(Twitter_Data$text)
Twitter$Source<-1
colnames(Twitter)[1]<-"Text"
Facebook_Data <- read.csv("RivaahBridesByTanishq_Facebook_Comments.csv")
FB<-as.data.frame(Facebook_Data$comment_message)
FB$Source<-2
colnames(FB)[1]<-"Text"
YouTube_Data <- read.csv("RivaahBridesByTanishq_YouTube_Video_Comments.csv")
YouTube<-as.data.frame(YouTube_Data$text)
YouTube$Source<-3
colnames(YouTube)[1]<-"Text"
Finaldata<-rbind(Twitter,FB,YouTube)

myStopWords<- c(stopwords("SMART"),"http","https","bitly","sticke","www","pictwittercom","bhi","pictwitter","com","shavetjain","youtube","pakistan","zeevekadlm","ghaywan","singh","sharma","such","sticker")
wordFrequency = 10
correlationValue = 0.1
associationWord = "emotional"
associationValue = 0.13
wordCloudMaxWords = 100

myCorpus <- cleanSocialMediaText(Finaldata$Text, myStopWords)

returnList <- posTagging(myCorpus)
posTaggedData <- returnList[[1]]
myWordList <- returnList[[2]]
myCorpusWordList <- returnList[[3]]

plotWordMetrics(myCorpus, myCorpusWordList, wordFrequency, correlationValue, associationWord, associationValue, wordCloudMaxWords)

sentimentAnalysis(myWordList)

topicModelling(posTaggedData, myWordList)

