rankhospital <- function(state, outcome, num = 'best') {
    
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
    o <- order(outcomeSubset[,2], outcomeSubset[,1], na.last = NA)
    outcomeSorted <- outcomeSubset[o,]
    numHospitals <- length(outcomeSorted[,1])
    
    if (num == 'best') {
        hospital <- as.character(outcomeSorted[1,1])
    }
    else if (num == 'worst') {
        hospital <- as.character(outcomeSorted[numHospitals,1])
    }
    else if (num > numHospitals) {
        hospital <- NA
    }
    else {
        hospital <- as.character(outcomeSorted[num,1])
    }
    
    hospital
    
}