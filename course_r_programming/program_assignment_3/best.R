best <- function(state, outcome){
    ## Read outcome data
    data.hospital.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

    ## Create value map that relates outcomes "heart attack", "heart failure", and "pneumonia" to the corresponding
    ## column number and column name in the data.hospital.outcome data.frame
    
    #[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    #[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    #[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    outcome_value_map <- list("heart attack" = c(col = 11, name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"),
                              "heart failure" = c(col = 17, name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"),
                              "pneumonia" = c(col = 23, name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))

    ## Check that state and outcome are valid
    if(!(toupper(state) %in% toupper(data.hospital.outcome$State))){
        stop(paste("Invalid state", paste("'", state, "'", sep = ""), sep = " "))
    }
    
    if(!(toupper(outcome) %in% toupper(names(outcome_value_map)))){
        stop(paste("Invalid outcome", paste("'", outcome, "'", ".", sep = ""), "Must be 'heart attack', 'heart failure', or 'pneumonia'", sep = " "))
    }
    
    #capture columnt number and column name that correspond to target outcome
    outcome.selected.col <- outcome_value_map[[outcome]]["col"]
    outcome.selected.name <- outcome_value_map[[outcome]]["name"]
    
    #select data for the target state
    #selecct only "Hospital Name" and target outcome column
    data.hospital.outcome.state <- data.hospital.outcome[data.hospital.outcome$State == toupper(state), c("Hospital.Name", outcome.selected.name)]
    
    #drop cases where the target outcome column is NA
    data.hospital.outcome.state <- data.hospital.outcome.state[complete.cases(data.hospital.outcome.state), ]

    #convert target outcome column to numeric
    data.hospital.outcome.state[outcome.selected.name] <- as.numeric(data.hospital.outcome.state[[outcome.selected.name]])
    
    #find the min (the best) of the target outcome column
    outcome.bset.value <- min(data.hospital.outcome.state[outcome.selected.name])
    
    #capture all records where the target outcome column equals to the best target outcome value
    data.hospital.outcome.final <- data.hospital.outcome.state[as.double(data.hospital.outcome.state[[outcome.selected.name]]) == as.double(c(outcome.bset.value)), ]
    
    #sort by Hospital Name and return the first record
    head(data.hospital.outcome.final[order(data.hospital.outcome.final$Hospital.Name), c("Hospital.Name")], 1)
}