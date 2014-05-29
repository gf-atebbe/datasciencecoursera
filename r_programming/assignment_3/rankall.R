rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcomes = c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% outcomes) {
        stop("invalid outcome")
    } else {
        if (outcome == "heart attack") {
            col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if (outcome == "heart failure") {
            col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
            col_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
    }
    
    ## For each state, find the hospital of the given rank
    data[[col_name]] <- as.numeric(data[[col_name]])
    states <- unique(data$State)
    m <- matrix(0, ncol = 2, nrow = length(states))
    returned = data.frame(m)
    colnames(returned) <- c("hospital", "state")
    returned$state <- states[order(states)]

    for (state in states) {
        data_state <- data[data$State == state,]
        data_min <- data_state[!is.na(data_state[[col_name]]), ]
        data_sort <- data_min[order(data_min[[col_name]]),]
        
        if (num == "best") {
            index = 1
        } else if (num == "worst") {
            index = length(data_sort$Hospital.Name)
        } else {
            index = num
        }
        
        final_rows <- data_sort[data_sort[[col_name]] == data_sort[[col_name]][index], ]
        final_sorted <- final_rows[order(final_rows$Hospital.Name),]
        final <- final_sorted[1, "Hospital.Name"]
        returned[returned$state == state, "hospital"] <- final_sorted[1, "Hospital.Name"]
    }

    ## Return a data frame with the hospital names and the 
    ## (abbreviated) state name
    returned
}
