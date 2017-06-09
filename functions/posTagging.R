posTagging <- function(tmCorpus, corpus){
    
    tagPOS <- function(corpus, PTA, ...) {
    
        if (is.na(corpus))
            return(list(POStagged = NA, POStags = NA))
        
        textString <- as.String(corpus)
        
        if (missing(PTA)) {
            PTA <- Maxent_POS_Tag_Annotator()
        }
        
        word_token_annotator <- Maxent_Word_Token_Annotator()
        annotedText <- Annotation(1L, "sentence", 1L, nchar(textString))
        annotedText <- NLP::annotate(textString, word_token_annotator, annotedText)
        annotedText <- NLP::annotate(textString, PTA, annotedText)
        annotedText <- annotedText[annotedText$type == "word"]
        posTags <- unlist(lapply(annotedText$features, "[[", "POS"))
        posTagged <- paste(sprintf("%s/%s", textString[annotedText], posTags), collapse = " ")
        list(posTagged = posTagged, posTags = posTags)
    }
    
    posTaggedData <- tagPOS(tmCorpus)    
    adjectives <- strsplit(posTaggedData$posTagged,' ')[[1]][which(posTaggedData$posTags == 'JJ')]
    wordList <- sapply(strsplit(adjectives, "/"), "[", 1)
    tmCorpusWordList <- Corpus(VectorSource(wordList))
    vCorpus <- VCorpus(VectorSource(corpus))

return(list(posTaggedData, wordList, tmCorpusWordList, vCorpus))
    
}