pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    nfiles = length(id)
    files <- list.files(directory)
    sums <- numeric(length = nfiles)
    els <- numeric(length = nfiles)
    
    for (i in seq_along(id)) {
        file <- paste(directory, files[id[i]], sep = '/')
        dataSet <- read.csv(file)
        sums[i] <- sum(na.omit(dataSet[,pollutant]))
        els[i] <- length(na.omit(dataSet[,pollutant]))
    }
    
    tot_els <- sum(els)
    tot_sums <- sum(sums)
    
    mean <- tot_sums / tot_els
    mean
}