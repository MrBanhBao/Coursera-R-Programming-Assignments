rankall <- function(outcome, num = "best") {
      ##read outcome data
      worstFlag <- FALSE #TODO: ugly
      data <- read.csv("data/outcome-of-care-measures.csv")
      outcome <- formatOutcome(outcome)
      
      ## Check that state and outcome are valid
      if(!outcome %in% colnames(data))
            stop("There are no records to this Outcome")
      
      #cast factor value to numeric
      data[, outcome] <- as.numeric(as.character(data[, outcome]))
      
      ##create output data frame
      outputDF <- data.frame(hostpital = character(), state = character())
      
      ## For each state, find the hospital of the given rank
      states <- levels(data$State)
      for(state in states) {
            data.state <- data[data$State == state, ]
            order.outcast <- data.state[order(data.state[, outcome], data.state$Hospital.Name), ]
            
            #Filter Data with NAs as Outcome
            good <- !is.na(order.outcast[, outcome])
            order.outcast.filtered <- order.outcast[good, ]
            
            #check 
            if(num == "best") {
                  num <- 1    
            } else if(num == "worst") {
                  num <- nrow(order.outcast.filtered)
                  worstFlag <- TRUE
            }
            
            hospitalName <- as.character(order.outcast.filtered[num, ]$Hospital.Name)
            newRow <- data.frame(hospital=hospitalName, state=state)
            
            outputDF <- rbind(outputDF, newRow)
            
            if(worstFlag) num = "worst"
      }
      
      ## Return a data frame with the hospital names and the
      outputDF
      ## (abbreviated) state name
}

formatOutcome <- function(outcome) {
      words <- capitalize(unlist(strsplit(outcome, split=" ")))
      outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(words, collapse="."), sep="")
      outcome
}

capitalize <- function(word) {
      paste(toupper(substr(word,1, 1)), substr(word, 2, nchar(word)), sep="")
}