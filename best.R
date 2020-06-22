best <- function(state, outcome) {
  
  #read data in
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  #subset read data frame into a new data.frame we can more easily manipulate
  new_data <- as.data.frame(cbind(data[, 2],    #hospital
                                  data[, 7],    #state
                                  data[, 11],   #HA
                                  data[, 17],   #HF
                                  data[, 23]),  #Pneumonia
                                  stringsAsFactors = FALSE)
  
  colnames(new_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  #Check for invalid entries
  if(!any(state == new_data$state)){
    
    stop("Invalid State: Enter a valid US State")
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
    
    #grab the min value for that outcome and remove the NA's
    min_value_per_state <- min(byOutcome, na.rm = TRUE)
    
    #grab the hospital/hospitals with that min value
    final <- byStatedf[, "hospital"][which(byOutcome == min_value_per_state)]
    
    #return an in order list of these final subset's
    theAnswer <- final[order(final)]
  
  }

return(theAnswer)
}