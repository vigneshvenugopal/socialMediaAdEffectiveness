# required pakacges
library(twitteR)
#library(sentiment)
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

#######################################
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
Comments<-as.character(Finaldata$Text)
#convert string to vector of words
dat2 <- unlist(strsplit(Comments, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dat4)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#POS tagging 
tagPOS <-  function(text.var, PTA, ...) {
  
  if (is.na(text.var)) return(list(POStagged = NA, POStags = NA))
  
  s <- as.String(text.var)
  
  ## Set up the POS annotator if missing (for parallel)
  if (missing(PTA)) {
    PTA <- Maxent_POS_Tag_Annotator()
  }
  
  ## Need sentence and word token annotations.
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, PTA, a2)
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, "[[", "POS"))
  
  ## Extract token/POS pairs (all of them): easy.
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
dat5<-tagPOS(some_txt)
Adjectives<-strsplit(dat5$POStagged,' ')[[1]][which(dat5$POStags == 'JJ')]
some_txt<-sapply(strsplit(Adjectives, "/"), "[", 1)
s_v <- get_sentences(some_txt)
nrc_data <- get_nrc_sentiment(s_v)
Sentiment<-pander::pandoc.table(nrc_data, split.table = Inf)
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
barplot(
  sort(colSums(prop.table(nrc_data[,9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Polarity in Sample text", xlab="Percentage"
)

#FaceBook
Finaldata<-rbind(Twitter,FB,YouTube)
Finaldata<-Finaldata%>%filter(Source == 2)
Comments<-as.character(Finaldata$Text)
#convert string to vector of words
dat2 <- unlist(strsplit(Comments, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dat4)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#POS tagging 
tagPOS <-  function(text.var, PTA, ...) {
  
  if (is.na(text.var)) return(list(POStagged = NA, POStags = NA))
  
  s <- as.String(text.var)
  
  ## Set up the POS annotator if missing (for parallel)
  if (missing(PTA)) {
    PTA <- Maxent_POS_Tag_Annotator()
  }
  
  ## Need sentence and word token annotations.
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, PTA, a2)
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, "[[", "POS"))
  
  ## Extract token/POS pairs (all of them): easy.
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
dat5<-tagPOS(some_txt)
Adjectives<-strsplit(dat5$POStagged,' ')[[1]][which(dat5$POStags == 'JJ')]
some_txt<-sapply(strsplit(Adjectives, "/"), "[", 1)
s_v <- get_sentences(some_txt)
nrc_data <- get_nrc_sentiment(s_v)
Sentiment<-pander::pandoc.table(nrc_data, split.table = Inf)
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
barplot(
  sort(colSums(prop.table(nrc_data[,9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Polarity in Sample text", xlab="Percentage"
)

#Twitter
Finaldata<-rbind(Twitter,FB,YouTube)
Finaldata<-Finaldata%>%filter(Source == 1)
Comments<-as.character(Finaldata$Text)
#convert string to vector of words
dat2 <- unlist(strsplit(Comments, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dat4)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#POS tagging 
tagPOS <-  function(text.var, PTA, ...) {
  
  if (is.na(text.var)) return(list(POStagged = NA, POStags = NA))
  
  s <- as.String(text.var)
  
  ## Set up the POS annotator if missing (for parallel)
  if (missing(PTA)) {
    PTA <- Maxent_POS_Tag_Annotator()
  }
  
  ## Need sentence and word token annotations.
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, PTA, a2)
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, "[[", "POS"))
  
  ## Extract token/POS pairs (all of them): easy.
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
dat5<-tagPOS(some_txt)
Adjectives<-strsplit(dat5$POStagged,' ')[[1]][which(dat5$POStags == 'JJ')]
some_txt<-sapply(strsplit(Adjectives, "/"), "[", 1)
s_v <- get_sentences(some_txt)
nrc_data <- get_nrc_sentiment(s_v)
Sentiment<-pander::pandoc.table(nrc_data, split.table = Inf)
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
barplot(
  sort(colSums(prop.table(nrc_data[,9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Polarity in Sample text", xlab="Percentage"
)

#YouTube
Finaldata<-rbind(Twitter,FB,YouTube)
Finaldata<-Finaldata%>%filter(Source == 3)
Comments<-as.character(Finaldata$Text)
#convert string to vector of words
dat2 <- unlist(strsplit(Comments, split=", "))
# find indices of words with non-ASCII characters
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
# subset original vector of words to exclude words with non-ASCII char
dat4 <- dat2[-dat3]
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", dat4)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
# define "tolower error handling" function 
try.error = function(x)
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
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)
# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#POS tagging 
tagPOS <-  function(text.var, PTA, ...) {
  
  if (is.na(text.var)) return(list(POStagged = NA, POStags = NA))
  
  s <- as.String(text.var)
  
  ## Set up the POS annotator if missing (for parallel)
  if (missing(PTA)) {
    PTA <- Maxent_POS_Tag_Annotator()
  }
  
  ## Need sentence and word token annotations.
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, PTA, a2)
  
  ## Determine the distribution of POS tags for word tokens.
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, "[[", "POS"))
  
  ## Extract token/POS pairs (all of them): easy.
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}
dat5<-tagPOS(some_txt)
Adjectives<-strsplit(dat5$POStagged,' ')[[1]][which(dat5$POStags == 'JJ')]
some_txt<-sapply(strsplit(Adjectives, "/"), "[", 1)
s_v <- get_sentences(some_txt)
nrc_data <- get_nrc_sentiment(s_v)
Sentiment<-pander::pandoc.table(nrc_data, split.table = Inf)
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
barplot(
  sort(colSums(prop.table(nrc_data[,9:10]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Polarity in Sample text", xlab="Percentage"
)

