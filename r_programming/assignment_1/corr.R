corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    correlations <- numeric()
    for (monitor in list.files(directory)) {
        source_file <- paste(directory, monitor, sep='/')
        this_monitor <- read.csv(source_file, header=TRUE)
        
        bool_test <- !is.na(this_monitor$nitrate) & !is.na(this_monitor$sulfate)
        complete <- this_monitor[bool_test, ]

        if (length(complete$Date) > threshold) {
            new_correlation <- cor(complete$sulfate, complete$nitrate)
            correlations <- c(correlations, new_correlation)
        }
    }
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    correlations
}
