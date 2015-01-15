best <- function(state, outcome) {

    ## Return hospital name in that state with lowest 30-day death rate
    
    outcomeData <- read.csv('outcome-of-care-measures.csv', na = 'Not Available')
    states <- levels(outcomeData$State)
    
    if (sum(state == states) == 0) {
        stop("invalid state")
    }
    
    if (outcome == "heart attack") {
        column <- 11
    }
    else if (outcome == "heart failure") {
        column <- 17
    }
    else if (outcome == "pneumonia") {
        column <- 23
    }
    else {
        stop("invalid outcome")
    }
    
    outcomeSubset <- subset(outcomeData, State == state, select = c(2, column))
    o <- order(outcomeSubset[,2], na.last = NA )
    outcomeSorted <- outcomeSubset[o,]
    first <- as.character(outcomeSorted[1,1]) 
    first
    
}