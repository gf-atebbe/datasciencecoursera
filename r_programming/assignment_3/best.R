best <- function(state, outcome) {
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
    
    data_state <- data[data$State == state,]
    data_staten <- as.numeric(data_state[[col_name]])
    min_val <- min(data_staten[!is.na(data_staten)])
    data_min <- data_state[data_state[[col_name]] == min_val, ]

    ## Return hospital name in that state with lowest 30-day death rate
    if(length(data_min$Hospital.Name) == 1) {
        final <- data_min$Hospital.Name
    } else {
        final <- data_min[order(data_min$Hospital.Name)[1], ]$Hospital.Name
    }
    
    final
}
