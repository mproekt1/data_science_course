source("filelib.R")

pollutantmean <- function(directory, pollutant, id = 1:332){
    #initialize filter variable removeNA to TRUE for pullutant
    removeNA <- c(TRUE) #set member to TRUE
    names(removeNA) <- c(pollutant) #assign member name that corresponds to pollutant (must be same as column in current_data)
    
    #use combine_data function to get a single data.frame that combines data for all ids in id range
    #this will avoid multople reading of the files. All data is collected in a single pass
    all_data <- combined_data(directory, id, removeNA)
    
    #return mean of pollutant
    mean(all_data[, pollutant])
    
    #opted from uding mean's na.rm paramteter, beacuse I wanted to play around with accessing data.frame members by variable name
}
