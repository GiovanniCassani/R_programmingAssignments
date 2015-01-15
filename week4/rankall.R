rankall <- function(outcome, num = 'best') {
    
    outcomeData <- read.csv('outcome-of-care-measures.csv', na = 'Not Available')
    state <- levels(outcomeData$State)
    hospital <- character()
    
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
    
    for (i in seq_along(state)) {
        outcomeSubset <- subset(outcomeData, State == state[i], select = c(2, column))
        o <- order(outcomeSubset[,2], outcomeSubset[,1], na.last = NA)
        outcomeSorted <- outcomeSubset[o,]
        numHospitals <- length(outcomeSorted[,1])
    
        if (num == 'best') {
            hospital[i] <- as.character(outcomeSorted[1,1])
        }
        else if (num == 'worst') {
            hospital[i] <- as.character(outcomeSorted[numHospitals,1])
        }
        else if (num > numHospitals) {
            hospital[i] <- NA
        }
        else {
            hospital[i] <- as.character(outcomeSorted[num,1])
        }
    }
    
    out <- as.data.frame(cbind(hospital, state))
    out
    
}