cleanSocialMediaText <- function(text, stopWords){
  
    text  <- as.character(genX(text, " <", ">"))
    nonASCI<- grep("text", iconv(text, "latin1", "ASCII", sub="text"))
    text<- text[-nonASCI]
    corpus<- Corpus(VectorSource(text))
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
    corpus <- tm_map(corpus, content_transformer(removeURL))
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
    corpus <- tm_map(corpus, content_transformer(removeNumPunct))
    corpus<- tm_map(corpus, removeWords, stopWords) 
    removeSingle <- function(x) gsub(" . ", " ", x)   
    corpus <- tm_map(corpus, content_transformer(removeSingle))
    corpus<- tm_map(corpus, stripWhitespace) 
    my_function<-content_transformer(function(x,pattern)gsub(pattern," ",x))
    tmCorpus<-tm_map(corpus,my_function,"[[STICKER]]")
    tmCorpus<-tm_map(corpus,content_transformer(tolower))
    
    return(list(tmCorpus, corpus))
}