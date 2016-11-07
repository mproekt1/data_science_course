source("filelib.R")

complete <- function(directory, id = 1:332){

    #use combine_data function to get a single data.frame that combines data for all ids in id range
    #this will avoid multople reading of the files. All data is collected in a single pass
    #filter removeNA is set TRUE for both sulfate and nitrate to ensure return of complete cases
    complete_data <- combined_data(directory, id, c(sulfate = TRUE, nitrate = TRUE))
    
    #append "count" coulumn set to value 1 to be used with aggregate function
    #the "count" will be calculates as sum(count)
    complete_data <- cbind(complete_data, data.frame(count = 1))
    
    #apply sum to "count" column groupped by ID
    aggregated_data <- aggregate(complete_data[c("count")], by = list(complete_data$ID), FUN = sum)
    
    #rename columns for final output
    names(aggregated_data) <- c("id", "nobs")
    
    #return value is the same id order as specified in idargument 
    aggregated_data[match(id, aggregated_data$id), ]
}
