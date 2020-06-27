rankhospital <- function(state, outcome, rank) {
  
  #read data in from source
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  new_data <- as.data.frame(cbind(data[,2],     #hospital
                                  data[,7],     #state
                                  data[,11],    #Heart Attack
                                  data[,17],    #Heart Failure
                                  data[,23]),   #Pneumonia
                                  stringsAsFactors = FALSE)
  
  #attach old colnames to new subsetted data
  colnames(new_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  
  #change the class of the outcome columns to numeric so they can be sorted later into ascending order
  new_data$`heart attack` <- as.numeric(as.character(new_data$`heart attack`))
  new_data$`heart failure` <- as.numeric(as.character(new_data$`heart failure`))
  new_data$pneumonia <- as.numeric(as.character(new_data$pneumonia))
  
  
  
  #Check for invalid entries
  if(!any(state == new_data$state)){
    
    stop(print("Invalid State: Enter a valid US State"))
    
  }
  
  else if((!any(outcome == c("heart failure", "heart attack", "pneumonia")))){
    
    stop(print("Invalid Outcome: Enter a valid outcome."))
    
  }
  
  else {
    
    #subset new_data by the input state and return vector of rownumbers for valid state entry
    byState <- which(new_data[, "state"] == state)
    
    #create new data frame with just the selected states
    byStatedf <- new_data[byState, ]
    
    #generate list of all results by that outcome in a numbered list
    byOutcome <- as.numeric(byStatedf[, eval(outcome)])
    byOutcome <- sort(byOutcome, decreasing = FALSE)
    
    
    if (rank == "best") {
      
      #grab the best (minimum) mortality rate by subsetting byStatedf
      finalBest <- byStatedf[, "hospital"][which.min(byStatedf[, outcome])]
      
      return(finalBest)
      
    }
    
    else if (rank == "worst") {
      #grab the worst (maximum) mortality rate by subsetting byStatedf
      finalWorst <- byStatedf[, "hospital"][which.max(byStatedf[, outcome])]
      
      return(finalWorst)
      
    }
    
    else if (is.numeric(rank)) {
      
      #order the byState.df appropriately first by outcome then by alphebetical order
      byStatedf1 <- byStatedf[order(byStatedf[, outcome], byStatedf[, "hospital"]), ]
      
      #subset the original dataframe to grab the names of the hospitals matching the rank value given
      finalRank <- byStatedf1[, "hospital"][rank]
      
      return(finalRank)
      
    }
    
    else {
      
      stop(print("Enter a Valid Rank"))
    }
    
    
    
  }
  
}
