analyzeWord <- function(word){
  filepath = paste("texts/",word, sep="")
  cname <- file.path("d:", filepath)
  print(cname)
  docs <- Corpus(DirSource(cname))
  
  ## Preprocessing      
  docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*    
  docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*    
  docs <- tm_map(docs, tolower)   # *Converting to lowercase:*   
  
  docs <- tm_map(docs, PlainTextDocument)   
  
  ### Stage the Data      
  dtm <- DocumentTermMatrix(docs)   
  tdm <- TermDocumentMatrix(docs)   
  
  ### Explore your data      
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq, decreasing = TRUE)
  
  frequentWords <- freq[head(ord,21)]
  fileName <- paste("./top 20 in/", word, ".txt",sep="")
  write.table(frequentWords,fileName)
  
  corr <- findAssocs(dtm, word, corlimit=0)
  fileName <- paste("./pearson correlation/wordCorrelations in ", word, ".txt",sep="")
  write.table(corr,fileName)
}