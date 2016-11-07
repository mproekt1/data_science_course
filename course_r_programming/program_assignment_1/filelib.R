read_file_to_data_frame <- function(directory, id, extension = "csv"){
    #directory - path to file directory relative to R working directory
    #id - numeric representation of file name. The file name is a 3 character string
    #     padded with 0s (001, 023, 332) 
    #extension - defaulted to "csv"
    
    
    #################################################################
    #
    # Argument validation:
    # 
    # directory - character vector with one member
    # id - numeric or character with one member and no more the 3 characters long
    # extension - character vector with one member
    #
    #################################################################
    if(!(class(directory) %in% "character" & length(directory) == 1)){stop("directory must be character vector with one member")}
    if(!(class(id) %in% c("numeric", "integer", "character") & length(id) == 1)){stop("id must be numeric, integer or character with one member")} else if(!(nchar(as.character(id)) <= 3)){stop("id must be no more than 3 characters long")}
    if(!(class(extension) %in% "character" & length(extension) == 1)){stop("extension must be character vector with one member")}
    #################################################################
    
    
    
    #################################################################
    #
    # Build and validate file path
    #
    #################################################################
    #id could be passed as both numeric or string
    #if any other ata type, then throw an error
    id_string <- if(class(id) %in% c("numeric", "integer")){as.character(id)} else if(class(id) == "character"){id} else( stop("id must be of 'numeric', 'integer' or 'character' data type"))
    
    #build file name form id
    file_name_padding <- paste(rep("0", 3 - nchar(id_string)), collapse = "")
    file_name <- paste(file_name_padding, id_string, ".", extension, sep = "")
    
    #build the file path
    file_path <- file.path(getwd(), directory, file_name)
    
    #if provided directory and id do not produce falid file path, return NULL
    if (!file.exists(file_path)){
        stop(paste("file path '", file_path, "' does not exists", sep = ""))
    }
    #################################################################
    
    
    #################################################################
    #
    # Create and return data frame
    #
    #################################################################
    #read file into data frame
    file_data <- read.csv(file = file_path, header = TRUE)
    
    
    #append the file path to the end of the data frame as file_path column
    file_data <- cbind(file_data, data.frame(file_path = rep(file_path, nrow(file_data))) )
    
    
    #return data frame to the caller
    file_data
    #################################################################
}

clean_data <- function(data, removeNA){
    #data - data frame that contains polution data
    #removeNA - named vecor where members name correspons to column in data data frame.
    #           If named member is TRUE, then records with NA values in the corresponding 
    #           column will be ignored.
    
    #################################################################
    #
    # Argument validation:
    # 
    # data - data.frame
    # removeNA - logical vector with one or more named members
    #            all names of members must be valid column names in data data frame
    #################################################################
    if(!(class(data) %in% "data.frame")){stop("data must be data.frame")}
    if(!is.null(removeNA)){if(!(class(removeNA) %in% "logical")){stop("removeNA must be logical")} else if(!(length(names(removeNA))) > 0){stop("members of removeNA must be named")}}
    if(!(all(names(removeNA) %in% names(data)))){stop("names of removeNA must be valid column names in data data.frame")}
    #################################################################
    
    #################################################################
    #
    # Apply filters removeNA
    #
    #################################################################
    #loop throuhg all members of removeNA vector
    for(col_filter in names(removeNA)){
        #capture member name and value
        removeNA_col_name <- col_filter
        removeNA_col_value <- removeNA[col_filter]
        
        #if member value is true then !is.NA will be applied to data.frame column with correspoding name
        if(removeNA_col_value){
            #reassign filtered data
            data <- data[!is.na(data[, removeNA_col_name]),]
            
            #move on to the next removeNA member
        }
    }
    #################################################################
    
    #return clean data.frame
    data
}

combined_data <- function(directory, id, removeNA = NULL){
    all_data <- 0 #initialize all_data
    
    for(current_id in id){
        #get data for id in current iteration
        current_data <- read_file_to_data_frame(directory = directory, id = current_id)
        
        #apply filter to current_data
        if(class(all_data) == "data.frame" & identical(names(all_data), names(current_data))){
            all_data <- rbind(all_data, clean_data(current_data, removeNA))
        }
        else{
            all_data <- clean_data(current_data, removeNA)
        }
    }
    
    all_data
}