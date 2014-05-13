pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    data <- data.frame()
    for (monitor in id) {
        source_file <- paste(directory, '/', sprintf("%03d", monitor) , '.csv', sep='')
        this_monitor <- read.csv(source_file, header=TRUE)
        temp_data <- rbind(data, this_monitor)
        data <- temp_data
    }
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    non_na <- data[!is.na(data[[pollutant]]),]

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    mean(non_na[[pollutant]])
}

a <- pollutantmean("specdata", "sulfate", 1:10)
print(a)