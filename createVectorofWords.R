createVectorOfWords <- function(matchingWord){
  tw <- readLines("sampletext.txt")
  tw <- tolower(tw)
  splittw<-unlist(strsplit(tw[1]," "))
  
  ## get the 20 or less words from the left side of the matching text
  getLeft <- function(matchIndex){
    s <- NULL
    ## if less then 20
    if (matchIndex < 20){
      i <- 1
    } else {
      i <- matchIndex - 20 
    }
    
    while(i < matchIndex){
      s <- c(s, splittw[i])
      i <- i+1
    }
    s <- paste(s,collapse=" ")
  }
  
  ## get the 20 or less words from the right side of the matching text
  getRight <- function(matchIndex){
    i <- matchIndex + 1
    s <- NULL
    maxI <- length(splittw)
    
    if(matchIndex + 20 > maxI){
      end <- maxI
    } else {
      end <- matchIndex + 20
    }
    
    while(i <= end){
      s <- c(s, splittw[i])
      i <- i+1
    }
    s <- paste(s,collapse=" ")
  }
  
  
  ## Main
  wordVector <- NULL
  lmr <- NULL
  i <- 1
  lastI <- length(splittw)
  
  ## create directory
  folder <- paste("./",matchingWord,sep="")
  dir.create(folder)
  
  while (i <= lastI){
    if (splittw[i]==matchingWord){

      ## get left side
      leftWords <- getLeft(i)
      
      ## get right side
      rigthWords <- getRight(i)
      
      # concatenate to one string
      lmr <- c(leftWords, splittw[i], rigthWords)
      lmr <- paste(lmr, collapse = " ")
      wordVector <- append(wordVector,lmr)
      
      #save file
      fileName <- paste("./",matchingWord,"/",i,".txt",sep="")
      write.table(lmr, fileName, col.names = F, row.names = F)
    }
    
    i <- i + 1
    
  }
}