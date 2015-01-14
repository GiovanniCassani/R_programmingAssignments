complete <- function(directory, id = 1:332) {
    
    nfiles = length(id)
    files <- list.files(directory)
    nobs <- numeric(length = nfiles)
    
    for (i in seq_along(id)) {
        file <- paste(directory, files[id[i]], sep = '/')
        dataSet <- read.csv(file)
        nobs[i] <- nrow(na.omit(dataSet))
    }
    
    out = as.data.frame(cbind(id, nobs))
    out
}