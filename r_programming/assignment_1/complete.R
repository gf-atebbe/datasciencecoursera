complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    data <- data.frame()
    for (monitor in id) {
        source_file <- paste(directory, '/', sprintf("%03d", monitor) , '.csv', sep='')
        this_monitor <- read.csv(source_file, header=TRUE)
        
        bool_test <- !is.na(this_monitor$nitrate) & !is.na(this_monitor$sulfate)
        counter <- length(this_monitor[bool_test, 1])
        temp_data <- rbind(data, c(monitor, counter))
        data <- temp_data
    }
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    colnames(data) <- c('id', 'nobs')
    data
}
