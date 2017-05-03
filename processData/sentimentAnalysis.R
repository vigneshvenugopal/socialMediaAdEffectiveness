# install.packages("devtools")
# require(devtools)
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# install.packages("plyr")
# install.packages("tm")
# install.packages("sentiment")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("RCurl")
# install.packages("rvest")
# install.packages("textcat")
# install.packages("stringr")

# ##Libraries
library(Rstem)
library(readr)
library(tm)
library(sentiment)
library("plyr")
library("ggplot2")
library("RColorBrewer")
library("wordcloud")
library("tm")
library("SnowballC")
library("RCurl")
library(rvest)
library(dplyr)
library("textcat")
library(stringr)

#setwd("C:/Users/Vicky/Google Drive/Capstone/Data")
Twitter_Data <- read_csv("BrotherlyLove_Twitter_Tweets.csv")
Twitter<-as.data.frame(Twitter_Data$text)
Twitter$Source<-1
colnames(Twitter)[1]<-"Text"
Facebook_Data <- read_csv("BrotherlyLove_Facebook_Comments.csv")
FB<-as.data.frame(Facebook_Data$comment_message)
FB$Source<-2
colnames(FB)[1]<-"Text"
YouTube_Data <- read_csv("BrotherlyLove_YouTube_Video_Comments.csv")
YouTube<-as.data.frame(YouTube_Data$text)
YouTube$Source<-3
colnames(YouTube)[1]<-"Text"
Finaldata<-rbind(Twitter,FB,YouTube)
Comments<-as.character(Finaldata$Text)
#convert string to vector of words
dat2 <- unlist(strsplit(Comments, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
pos= readLines("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/opinion_lexicon_English/positive-words.txt")
neg= readLines("https://raw.githubusercontent.com/vigneshvenugopal/socialMediaAdEffectiveness/master/opinion_lexicon_English/negative-words.txt")

# Wrapper function for sentiment analysis

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits
                   sentence = gsub('\\d+', '', sentence)
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                     
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   sentence = sapply(sentence, tryTolower)
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df = data.frame(score=scores)
  return(scores.df)
}
score = score.sentiment(dat4, pos, neg, .progress='text')
Sentiment_data<-cbind(dat4,score)
Sentiment_data$score<-as.numeric(Sentiment_data$score)
hist(Sentiment_data$score,xlab=" ",main="Sentiment Analysis",
     border="black",col="skyblue")
