best <- function(state, outcome) {
      ##read outcome data
      data <- read.csv("data/outcome-of-care-measures.csv")
      outcome <- formatOutcome(outcome)
      
      ##check that state and outcome are valid
      if(!state %in% data$State)
            stop("Given State does not exist in Data.")
      if(!outcome %in% colnames(data))
            stop("There are no records to this Outcome")
      
      ## Return hospital name in that state with lowest 30-day death
      data[, outcome] <- as.numeric(as.character(data[, outcome])) #cast factor value to numeric
      data.state <- data[data$State == state, ]
      
      order.outcast <- data.state[order(data.state[, outcome]), ]
      name <- as.character(order.outcast[1, ]$Hospital.Name)

      ## rate
      name
}

formatOutcome <- function(outcome) {
      words <- capitalize(unlist(strsplit(outcome, split=" ")))
      outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", paste(words, collapse="."), sep="")
      outcome
}

capitalize <- function(word) {
      paste(toupper(substr(word,1, 1)), substr(word, 2, nchar(word)), sep="")
}