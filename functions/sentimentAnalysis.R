sentimentAnalysis <- function(wordList){
  
  sentenceVector <- get_sentences(wordList)
  nrcData <- get_nrc_sentiment(sentenceVector)
  sentiment <- pander::pandoc.table(nrcData, split.table = Inf)
  
  barplot(
    sort(colSums(prop.table(nrcData[, 1:8]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions in Data", xlab="Percentage"
  )
  
  barplot(
    sort(colSums(prop.table(nrcData[,9:10]))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Polarity in Data", xlab="Percentage"
  )
  
}
