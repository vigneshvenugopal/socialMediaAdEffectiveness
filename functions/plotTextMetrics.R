plotTextMetrics <- function(tmCorpusWordList, vCorpus, wordFrequency, correlationValue, associationWord, associationValue, wordCloudMaxWords, wordCloudMinFreq, corrWord){
    
    tdm <- TermDocumentMatrix(tmCorpusWordList, control= list(wordLengths= c(1, Inf)))
    freq.terms <- findFreqTerms(tdm, lowfreq = wordFrequency)
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > wordFrequency)
    df <- data.frame(term = names(term.freq), freq= term.freq)
    word.freq <- sort(rowSums(as.matrix(tdm)), decreasing= F)
    wordCorr <- apply_as_df(vCorpus, word_cor, word = corrWord, r = correlationValue)
    assoTdm <- TermDocumentMatrix(vCorpus, control= list(wordLengths= c(1, Inf)))
    associationData <- findAssocs(assoTdm, associationWord, associationValue)
    associationDataDF <- data.frame(associationData)
    colnames(associationDataDF) <- ('association')
    assoWords <- row.names(associationDataDF)
    associationDataDF <- data.frame(cbind(assoWords, associationDataDF$association))[1:10,]
    colnames(associationDataDF) <- c('word', 'association')
    associationGraph <- graph.data.frame(associationDataDF, directed = TRUE)
    
    wordcloud(words = names(word.freq), freq = word.freq, min.freq = wordCloudMinFreq, random.order = F, colors = brewer.pal(8, "Dark2"), max.words = wordCloudMaxWords)
    plot(associationGraph, main = associationWord)
    wordFreqPlot <- ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity") + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts"))
    wordCorrPlot <- plot(wordCorr)
    
    return(list(wordFreqPlot, wordCorrPlot))
}