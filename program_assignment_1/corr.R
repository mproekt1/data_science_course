source("filelib.R")
source("complete.R")

corr <- function(directory, threshold = 0){

    #initialize return value as empty numeric vector
    corr_data <- numeric()
    
    #use output of complete function to obtain collection of ids for files with completed cases 
    files_completed <- complete(directory)
    
    #apply threshold filter and capture ids
    files_passed_threshold <- files_completed[files_completed[, "nobs"] > threshold, ]
    
    #use combine_data function to get a single data.frame that combines data for all ids that passed threshold
    #this will avoid multople reading of the files. All data is collected in a single pass
    files_passed_threshold_data <- combined_data("specdata", files_passed_threshold[["id"]], c(sulfate = TRUE, nitrate = TRUE))
    
    #loop through the ids of files that passef the threshold test
    for(current_id in files_passed_threshold$id){
        #filter the combined data for the current id
        #and capture in current_data
        current_data <- files_passed_threshold_data[files_passed_threshold_data$ID == current_id, ]
        
        #capture sulfate and nitrate reading in separate vectors
        current_sulfate <- current_data$sulfate
        current_nitrate <- current_data$nitrate
        
        #apply cor function and capture output in current_cor
        current_cor <- cor(current_sulfate, current_nitrate)
        
        #add (append) current_cor value to the return vector corr_data
        corr_data <- append(corr_data, current_cor)
    }
    
    #assign names to the return vector for better readablity
    #the names are assigned from the same data.frame that was used for the loo
    #to ensure that the order os names matches the order of values
    names(corr_data) <- files_passed_threshold$id
    
    #return value
    corr_data
}