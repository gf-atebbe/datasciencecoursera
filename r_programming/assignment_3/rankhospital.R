rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if (!state %in% data$State) {
        stop("invalid state")
    }
    
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
    
    data[[col_name]] <- as.numeric(data[[col_name]])
    data_state <- data[data$State == state,]
    
    ## Order by col_name
    data_min <- data_state[!is.na(data_state[[col_name]]), ]
    data_sort <- data_min[order(data_min[[col_name]]),]
    
    if (num == "best") {
        index = 1
    } else if (num == "worst") {
        index = length(data_sort$Hospital.Name)
    } else {
        index = num
    }

    ## Return hospital name in that state with the given rank 30-day death rate
    if (index > length(data_sort$Hospital.Name)) {
        final <- NA
    } else {
        final_rows <- data_sort[data_sort[[col_name]] == data_sort[[col_name]][index], ]
        final_sorted <- final_rows[order(final_rows$Hospital.Name),]
        final <- final_sorted[1, "Hospital.Name"]
    }
    
    final
}
