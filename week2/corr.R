corr <- function(directory, threshold = 0) {
 
    files <- list.files(directory)
    corrs <- numeric(0)
    idx = 1
    
    for (i in seq_along(files)) {
        file <- paste(directory, files[i], sep = '/')
        dataSet <- read.csv(file)
        rows <- na.omit(dataSet)

        if(nrow(rows) >= threshold) {
            corrs[idx] <- cor(rows$nitrate, rows$sulfate)
            idx = idx + 1
        }
    }
    
    corrs
    
}