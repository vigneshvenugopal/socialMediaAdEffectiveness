# required pakacges
library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(Rstem)
library(readr)
library(tm)
library("SnowballC")
library("RCurl")
library(rvest)
library(dplyr)
library("textcat")
library(stringr)
library(openNLP)
library(NLP)
library(syuzhet)
library("pander")
library(qdap)

setwd("C:\\Users\\Vicky\\Google Drive\\Capstone\\Data\\New")
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
associationWord = "rivaahbridesbytanishq"
associationValue = 0.13

myCorpus <- cleanSocialMediaText(Finaldata$Text, myStopWords)
posTagData = 0
list[posTagData, myWordList] <- posTagging(myCorpus)

plotWordMetrics(myCorpus, myWordList, wordFrequency, correlationValue, associationWord, associationValue)

sentimentAnalysis(myWordList)


