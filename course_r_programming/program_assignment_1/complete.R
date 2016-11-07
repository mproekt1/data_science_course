source("filelib.R")

complete <- function(directory, id = 1:332){

    #use combine_data function to get a single data.frame that combines data for all ids in id range
    #this will avoid multople reading of the files. All data is collected in a single pass
    #filter removeNA is set TRUE for both sulfate and nitrate to ensure return of complete cases
    complete_data <- combined_data(directory, id, c(sulfate = TRUE, nitrate = TRUE))

    #apply NROW function to ID column groupped by ID using anonymous function that wraps arounf NROW
    aggregated_data <- aggregate(complete_data$ID, by = list(complete_data$ID), FUN = function(x){NROW(x)})
    
    #rename columns for final output
    names(aggregated_data) <- c("id", "nobs")
    
    #apply the same id order as specified in id argument 
    aggregated_data <- aggregated_data[match(id, aggregated_data$id), ]
    
    #match function return NA is values in aggregated_data didn't match (no completed cases)
    #remove unmatched records and return
    aggregated_data[!(is.na(aggregated_data$id)), ]
}
