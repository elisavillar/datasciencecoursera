############# Programming Assignment 3 #####################

###### Plot the 30-day mortality rates for heart attack ####

# Load packages

library(dplyr)

  
  best <- function(state, outcome) {
    
    ## Read outcome data
    
    outcome_data <- read.csv("outcome-of-care-measures.csv")
    
    ## Put in lower case outcome 
    
    outcome <- tolower(outcome)
    
    ## Put in capital letters the State
    
    state <- toupper(state)
    
    ## Check that state and outcome are valid
    
    if (!state %in% unique(outcome_data[["State"]])) {
      
      stop("Invalid State", call. = TRUE)
      
    } 
    
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      
      stop("Invalid Outcome", call. = TRUE)
      
    } 
    
    ## Filter by state
    
    outcome_data <- outcome_data[grep(state, outcome_data$State), ]
    
    
    ## Getting the column for the selected outcome
    
    col <- if (outcome == "heart attack") {
      11
    } else if (outcome == "heart failure") {
      17
    } else {
      23
    }
    
    ## Getting the lowest rate in the State and outcome selected
    
    outcome_data <- select(outcome_data, 2, all_of(col))
    
    colnames(outcome_data) <- c("Hospital", "Outcome")
    
    suppressWarnings(outcome_data$Outcome <- as.numeric(as.character(outcome_data$Outcome)))
    
    outcome_data <- outcome_data[order(outcome_data$Outcome), ]
    
    ## Return hospital name in that state with lowest 30-day death rate
    
    return(outcome_data[[1, 1]])
    
  }

####### Test ############

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

