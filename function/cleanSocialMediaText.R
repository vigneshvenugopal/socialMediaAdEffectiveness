cleanSocialMediaText <- function(text, myStopWords){
    text  <- as.character(genX(text, " <", ">"))
    nonASCI<- grep("text", iconv(text, "latin1", "ASCII", sub="text"))
    text<- text[-nonASCI]
    myCorpus<- Corpus(VectorSource(text))
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  
    myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)   
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
    myCorpus<- tm_map(myCorpus,removeWords , myStopWords) 
    removeSingle <- function(x) gsub(" . ", " ", x)   
    myCorpus <- tm_map(myCorpus, content_transformer(removeSingle))
    myCorpus<- tm_map(myCorpus, stripWhitespace) 
    myCorpusCopy<- myCorpus
    my_function<-content_transformer(function(x,pattern)gsub(pattern," ",x))
    myCorpus<-tm_map(myCorpus,my_function,"[[STICKER]]")
    myCorpus<-tm_map(myCorpus,content_transformer(tolower))
    
    return(myCorpus)
}