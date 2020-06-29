rankall <- function(outcome, num) {
  
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
  
  #split the dataframe up by state. List of dataframes for each state
  byState <- split(new_data, new_data$state)
  
  #order the data frames according to the outcome and then alphabetically by hospital
  orderState <- lapply(byState, function(x) x[order(x[, outcome], x[, "hospital"]), ])
  


  #Check for invalid entries
  if ((!any(outcome == c("heart failure", "heart attack", "pneumonia")))){
    
    stop(print("Invalid Outcome: Enter a valid outcome."))
    
  }
  
  else if (is.numeric(num)) {
    
    #grab the hospital data by the given numeric rank
    byRank <- lapply(orderState, function(x) x[, "hospital"][num])
    
    #convert results to data.frame and properly label the columns
    finalRank <- as.data.frame(t(as.data.frame(byRank)))
    finalRank$state <- row.names(finalRank)
    colnames(finalRank) <- c("hospital", "state")
    
    return(finalRank)
    
  }
  
  else if (!is.numeric(num)) {
    
    if (num == "best") {
      
      #generate list of best hospital per state with outcome
      bestRank <- lapply(byState, function(x) x[, "hospital"][which.min(x[, outcome])])
      
      #convert results to data.frame and properly label the columns
      bestRank <- as.data.frame(t(as.data.frame(bestRank)))
      bestRank$state <- row.names(bestRank)
      colnames(bestRank) <- c("hospital", "state")
      
      return(bestRank)
      
    }
    
    else if (num == "worst") {
      
      #generate list of best hospital per state with outcome
      worstRank <- lapply(byState, function(x) x[, "hospital"][which.max(x[, outcome])])
      
      #convert results to data.frame and properly label the columns
      worstRank <- as.data.frame(t(as.data.frame(worstRank)))
      worstRank$state <- row.names(worstRank)
      colnames(worstRank) <- c("hospital", "state")
      
      return(worstRank)
      
    }
    
    else {
      
      stop("Invalid entry for num: Please enter 'best', 'worst' or a numeric rank")
    
      }
    
    
  }
  
  
  
  
}
