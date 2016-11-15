rankall <- function(outcome, num = "best"){
    ## Read outcome data
    data.hospital.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## Distinct list of states (to be used later)
    data.state <- unique(data.hospital.outcome["State"])
    
    #add Hospital Name place holder for future use
    data.state$Hospital.Name <- NA

    
    ## Create value map that relates outcomes "heart attack", "heart failure", and "pneumonia" to the corresponding
    ## column number and column name in the data.hospital.outcome data.frame
    
    #[11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    #[17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    #[23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    outcome_value_map <- list("heart attack" = c(col = 11, name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"),
                              "heart failure" = c(col = 17, name = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"),
                              "pneumonia" = c(col = 23, name = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
    
    ## Check that state and outcome are valid
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
    
    #limit data frame data.hospital.outcome columns to Hostpital Name, State, and target outcome column
    data.hospital.outcome <- data.hospital.outcome[, c("Hospital.Name", "State", outcome.selected.name)]
    
    #convert target outcome column to numeric
    data.hospital.outcome[outcome.selected.name] <- as.numeric(data.hospital.outcome[[outcome.selected.name]])
    
    #resort by state, target outcome column, and state name (for tie resolution) in ascending order
    data.hospital.outcome <- data.hospital.outcome[order(data.hospital.outcome$State, data.hospital.outcome[outcome.selected.name], data.hospital.outcome$Hospital.Name), ]
    
    #rank hospitals withing each State group
    data.hospital.outcome$Rank <- unlist(tapply(data.hospital.outcome[[outcome.selected.name]], data.hospital.outcome$State, function(x){rank(x, ties.method = "first")}))
    
    #cpature records without NA
    data.hospital.outcome.na.rm <- data.hospital.outcome[!is.na(data.hospital.outcome[outcome.selected.name]), ]

    #calculate worst rank for reach state
    #returns named records
    data.state.worst <- as.data.frame(tapply(data.hospital.outcome.na.rm$Rank, data.hospital.outcome.na.rm$State, max))

    #convert record names to data frame column
    data.state.worst <- cbind(data.state.worst, rownames(data.state.worst))
    
    #assign meaningful names to the columns
    #and clear record names
    names(data.state.worst) <- c("Rank.Worst", "State")
    rownames(data.state.worst) <- c(NULL, NULL)
    
    #populate Ranks.Worst column in data.hospital.outcome by looking up values in data.state.worst
    #it will be used lates when returning "worst" hospitals
    data.hospital.outcome$Rank.Worst <- data.state.worst[match(data.hospital.outcome$State, data.state.worst$State), "Rank.Worst"]

    
    #if num is "best" set num value to the min rank
    if(tolower(num) == "best"){
        data.hospital.outcome.final <- data.hospital.outcome[data.hospital.outcome$Rank == 1, c("Hospital.Name", "State")]
    }
    #if num is "worst" set num value to the max rank
    else if (tolower(num) == "worst"){
        data.hospital.outcome.final <- data.hospital.outcome[data.hospital.outcome$Rank == data.hospital.outcome$Rank.Worst, c("Hospital.Name", "State")]
    }
    else{
        #hospitals with rank num
        data.hospital.outcome.final <- data.hospital.outcome[data.hospital.outcome$Rank == num, c("Hospital.Name", "State")]
    }
    
    
    #append States with NA data
    data.hospital.outcome.final <- rbind(data.hospital.outcome.final, data.state[!(data.state$State %in% data.hospital.outcome.final$State), c("Hospital.Name", "State")])
    
    #assign new names
    names(data.hospital.outcome.final) <- c("hospital", "state")
    
    #return sorted by State
    data.hospital.outcome.final[order(data.hospital.outcome.final$state), ]
}