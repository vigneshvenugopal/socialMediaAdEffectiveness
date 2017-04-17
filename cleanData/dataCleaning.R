# install.packages("devtools")
# require(devtools)
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# install.packages("plyr")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("RCurl")
# install.packages("rvest")
# install.packages("textcat")
# 
# ##Libraries
# library(Rstem)
# library(readr)
# library(sentiment)
# library("plyr")
# library("ggplot2")
# library("RColorBrewer")
# library("wordcloud")
# library("tm")
# library("SnowballC")
# library("RCurl")
# library(rvest)
# library(dplyr)
# library("textcat")
# 
# setwd("C:\\Users\\Vicky\\Google Drive\\Capstone\\Data\\")
GalaxyS8_tweets <- read_csv("TouchOfCare_Twitter_Tweets.csv")
Twitter<-as.data.frame(GalaxyS8_tweets$text)
Twitter$Source<-1
colnames(Twitter)[1]<-"Text"
GalaxyS8_facebook_comments <- read_csv("TouchOfCare_Facebook_Comments.csv")
FB<-as.data.frame(GalaxyS8_facebook_comments$comment_message)
FB$Source<-2
colnames(FB)[1]<-"Text"
GalaxyS8_youtube <- read_csv("TouchOfCare_YouTube_Video_Comments.csv")
YouTube<-as.data.frame(GalaxyS8_youtube$text)
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
# convert vector back to a string
Comments <- paste(dat4, collapse = ", ")
corpus_fb <- Corpus(VectorSource(Comments))
my_function<-content_transformer(function(x,pattern)gsub(pattern," ",x))
Cleaned_Corpus_fb<-tm_map(corpus_fb,my_function,"/")
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,my_function,"@")
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,my_function,"\\|")
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,my_function,"[[STICKER]]")
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,content_transformer(tolower))
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,removeWords,c(stopwords("SMART"),"http","https","bitly","sticke","www","pictwittercom","bhi","pictwitter","com","shavetjain","youtube","pakistan","zeevekadlm"))
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,removeWords,c("TouchOfCare", "vicks", "youtu", "video", "vicksindia"))
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,removePunctuation)
Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,stripWhitespace)
#Cleaned_Corpus_fb<-tm_map(Cleaned_Corpus_fb,stemDocument)
my_tdm<-TermDocumentMatrix(Cleaned_Corpus_fb)
m<-as.matrix(my_tdm)
words<-sort(rowSums(m),decreasing = TRUE)
data_samsun<-data.frame(word = names(words),freq = words)
#used for dettol only data_samsun<-data_samsun%>%filter(freq > 1)
data_samsun<-data_samsun%>%filter(freq >15)
unique(data_samsun$word)
word_clound_Samsung<-wordcloud(words = data_samsun$word,freq = data_samsun$freq,min.freq = 3,max.words = 100,random.order = FALSE,rot.per = 0.35,colors = brewer.pal(8,"Dark2"))

