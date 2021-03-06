topicModelling <- function(posTaggedData, corpusWordList){
  
  nn <- strsplit(posTaggedData$posTagged,' ')[[1]][which(posTaggedData$posTags == 'NN')]
  nn <- sapply(strsplit(nn, "/"), "[", 1)
  nns <- strsplit(posTaggedData$posTagged,' ')[[1]][which(posTaggedData$posTags == 'NNS')]
  nns <- sapply(strsplit(nns, "/"), "[", 1)
  nnp <- strsplit(posTaggedData$posTagged,' ')[[1]][which(posTaggedData$posTags == 'NNP')]
  nnp <- sapply(strsplit(nnp, "/"), "[", 1)
  
  wordsToRemove <- append(nn, nnp)
  wordsToRemove <- append(wordsToRemove, nns)
  wordsToRemove <- unlist(wordsToRemove)
  
  try(corpusWordList <- tm_map(corpusWordList, removeWords, wordsToRemove))
  tdmLda <- TermDocumentMatrix(corpusWordList, control= list(wordLengths= c(1, Inf)))
  dtm.control <- list(tolower= F,removePunctuation=F,removeNumbers= F,
                     stemming= F, minWordLength = 3,weighting= weightTf,stopwords=F)
  
  dtm <- as.DocumentTermMatrix(tdmLda, control = dtm.control)
  
  rowTotals <- apply(dtm , 1, sum)
  
  nullDocs <- dtm[rowTotals==0, ]
  dtm   <- dtm[rowTotals> 0, ]
  
  if (length(nullDocs$dimnames$Docs) > 0) {
    tweets.df <- tweets.df[-as.numeric(nullDocs$dimnames$Docs),]
  }
  
  lda <- LDA(dtm, k = 5)
  term <- terms(lda, 7)
  (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
  
  
  best.model <- lapply(seq(2,50, by=2), function(k){LDA(dtm[1:10,], k)})
  gc();
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  best.model.logLik.df <- data.frame(topics=c(seq(2,50, by=2)), LL=as.numeric(as.matrix(best.model.logLik)))
  k=best.model.logLik.df[which.max(best.model.logLik.df$LL),1];
  cat("Best topic number is k=",k);
  k = 5
  flush.console();
  lda.model = LDA(dtm, k,method='GIBBS')
  
  write.csv(terms(lda.model,7), file = "terms_Gibbs.csv");
  
  lda.model = LDA(dtm, k,method='VEM')
  write.csv(terms(lda.model,7), file = "terms_Vem.csv");
  
  ap_topics <- tidy(lda.model, matrix = "beta")
  ap_topics
  
  ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
  
  ap_top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip()
}
