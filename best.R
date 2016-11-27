best <- function(state, outcome) {

  ## Read outcome data from the flat file located in the data directory
  outcomes <- read.csv(file='data/assig3/outcome-of-care-measures.csv', colClasses = 'character', na.strings = c("NA", "Not Available"))

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

  ## Return hospital name in that state with lowest 30-day death rate
  byState <- ourOutcomes[ourOutcomes$State == state, ]

  byStateByOutcome<-byState[,c(1,outColIndex)]

  best<-byStateByOutcome[byStateByOutcome[,2]==min(byStateByOutcome[,2], na.rm=TRUE), ]
  best<-best[!is.na(best[, 2]), ]

  bestHospital<-best[best$Hospital.Name == min(best$Hospital.Name), ]

  bestHospital
}
