rankhospital <- function(state, outcome, num){
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
    
    if(!(length(num) == 1)){
        stop("Invalid num lenght. num length must be equal 1")
    }
    else if(is.character(num)){
        if(!(tolower(num) %in% c("best", "worst"))){
                stop(paste("Invalid num", paste("'", num, "'", ".", sep = ""), "Must be 'best', 'worst', or integer", sep = " "))
            }
    }
    else if(is.numeric(num)){
        if(!(num == as.integer(num))){
            stop(paste("Invalid num", paste("'", num, "'", ".", sep = ""), "Must be 'best', 'worst', or integer", sep = " "))
        }
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
    
    #reorder data.hospital.outcome.state by target outcome column, then by Hostpital Name
    data.hospital.outcome.state <- data.hospital.outcome.state[order(data.hospital.outcome.state[outcome.selected.name], data.hospital.outcome.state$Hospital.Name), ]
    
    #Assign rank and store in a new column Rank
    data.hospital.outcome.state$Rank <- rank(data.hospital.outcome.state[outcome.selected.name], ties.method = "first")
    

    #if num is "best" set num value to the min rank
    if(tolower(num) == "best"){
        num <- min(data.hospital.outcome.state$Rank)
    }
    #if num is "worst" set num value to the max rank
    else if (tolower(num) == "worst"){
        num <- max(data.hospital.outcome.state$Rank)
    }

    #capture all hospitals with Rank == num
    ret_value <- data.hospital.outcome.state[data.hospital.outcome.state$Rank == num, "Hospital.Name"]
    
    if(!length(ret_value)){
        NA
    }
    else{
        ret_value
    }

}