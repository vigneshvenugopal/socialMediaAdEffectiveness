plotWordMetrics <- function(corpus, corpusWordList, wordFrequency, correlationValue, associationWord, associationValue, wordCloudMaxWords){
    
    tdm <- TermDocumentMatrix(corpusWordList, control= list(wordLengths= c(1, Inf)))
    freq.terms <- findFreqTerms(tdm, lowfreq = wordFrequency)
    term.freq <- rowSums(as.matrix(tdm))
    term.freq <- subset(term.freq, term.freq > wordFrequency)
    df <- data.frame(term = names(term.freq), freq= term.freq)
    word.freq <- sort(rowSums(as.matrix(tdm)), decreasing= F)
    
    myCorpus <- VCorpus(VectorSource(corpus))
    wordCorr <- apply_as_df(myCorpus, word_cor, word = associationWord, r = correlationValue)
    
    findAssocs(tdm, associationWord, associationValue)
    
    ggplot(df, aes(reorder(term, freq),freq)) + theme_bw() + geom_bar(stat = "identity")  
    + coord_flip() +labs(list(title="Term Frequency Chart", x="Terms", y="Term Counts"))
    
    wordcloud(words = names(word.freq), freq = word.freq, min.freq = 7, random.order = F, colors = brewer.pal(8, "Dark2"), max.words = wordCloudMaxWords)
    
    plot(wordCorr)
    
    qheat(vect2df(wordCorr[[1]], "word", "cor"), values = TRUE, high="Blue",
                     digits = 2, order.by ="cor", plot = FALSE) + coord_flip()
    
}