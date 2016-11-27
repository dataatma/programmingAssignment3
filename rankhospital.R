
rankhospital <- function(state, outcome, num = "best") {

  ## Read outcome data from the flat file located in the data directory
  outcomes <- read.csv(file='data/assig3/outcome-of-care-measures.csv', colClasses = 'character', na.strings = c("NA", "Not Available"))

  ## Check that state and outcome are valid
  ourOutcomes<-outcomes[,c(2,7,11,17,23)]
  names(ourOutcomes)<-c("Hospital.Name", "State", "Heart.Attack", "Heart.Failure", "Pheumonia")
  ourOutcomes[, c(3,4,5)] <- sapply(ourOutcomes[, c(3,4,5)], as.numeric)

  ## Check that state and outcome are valid

  outColIndex<-0

  if(!any(ourOutcomes$State == state)){
   stop('invalid state')
  }

  if(outcome == 'heart attack') {
    outColIndex <- 3
  }
  else if(outcome == 'heart failure') {
    outColIndex <- 4
  }
  else if(outcome == 'pneumonia') {
    outColIndex <- 5
  }
  else {
    stop('invalid outcome')
  }

  suppressWarnings(num2<-as.numeric(num))

  if (num != "best" & num != "worst" & is.na(num2)) {
    stop('invalid num provided. Valid Arguments are best, worst or a integer nth rank')
  }

  ## Return hospital name in that state with the given rank
  byState <- ourOutcomes[ourOutcomes$State == state, ]
  byStateByOutcome<-byState[,c(1,outColIndex)]

  ranked <- character()
  if (num == "best") {
    ranked<- byStateByOutcome[order(byStateByOutcome[,2], byStateByOutcome[,1]),][1,]
  } else if (num == "worst"){
    ranked<- byStateByOutcome[order(byStateByOutcome[,2], byStateByOutcome[,1],decreasing=TRUE),][1,]
  }
  else {
    if (length(byStateByOutcome$Hospital.Name) < num2) {
      ranked<-NA
    }
    else {
      ranked<- byStateByOutcome[order(byStateByOutcome[,2], byStateByOutcome[,1]),][num2,]
    }
  }
  ranked
}
