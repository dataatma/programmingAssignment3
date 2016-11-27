
rankall <- function(outcome, num = "best") {

  ## Read outcome data from the flat file located in the data directory
  outcomes <- read.csv(file='data/assig3/outcome-of-care-measures.csv', colClasses = 'character', na.strings = c("NA", "Not Available"))

  ## Check that outcome and num are valid
  outColIndex<-0

  if(outcome == 'heart attack') {
    outColIndex <- 11
  }
  else if(outcome == 'heart failure') {
    outColIndex <- 17
  }
  else if(outcome == 'pneumonia') {
    outColIndex <- 23
  }
  else {
    stop('invalid outcome')
  }

  suppressWarnings(num2<-as.numeric(num))

  if (num != "best" & num != "worst" & is.na(num2)) {
    stop('invalid num provided. Valid Arguments are best, worst or a integer nth rank')
  }

  ourOutcomes<-outcomes[,c(2,7,outColIndex)]
  names(ourOutcomes)<-c("Hospital.Name", "State", "Mortality")
  ourOutcomes[, 3] <- sapply(ourOutcomes[, 3], as.numeric)


  ## For each state, find the hospital of the given rank

  byState<-split(ourOutcomes, ourOutcomes$State)

  ranked <- lapply(byState, function(dataInState, num, num2) {
                curState <- min(dataInState$State)
                r1 <-  data.frame(NA,curState,NA)
                names(r1) <- c("Hospital.Name", "State", "Mortality")

                if (num == "best") {
                  r1 <- data.frame(dataInState[order(dataInState[,"Mortality"], dataInState[,"Hospital.Name"]),][1,])
                } else if (num == "worst"){
                  r1 <- data.frame(dataInState[order(dataInState[,"Mortality"], dataInState[,"Hospital.Name"],decreasing=TRUE),][1,])
                }
                else {
                  if (length(dataInState$Hospital.Name) >= num2) {
                    r1 <- data.frame(dataInState[order(dataInState[,"Mortality"], dataInState[,"Hospital.Name"]),][num2,])
                  }
                }
                r1
              }, num, num2
            )
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(Reduce(rbind,ranked))
}
