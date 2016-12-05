run_analysis <- function(max_rows = -1){
    #install.packages("dplyr")
    #library(dplyr)
    
    #install.packages("tidyr")
    #library(tidyr)
    
    
    
    #***********************************************
    #determine location of data.
    #the data directory "UCI HAR Dataset" must be located
    #in the same directory as the script run_analysis.R
    #the following code determines the current location of run_analysis.R
    #and uses it to construct a path to data
    script_dir <- getSrcDirectory(eval(match.call()[[1]]))

    features.txt.path <- paste0(script_dir, "/UCI HAR Dataset/features.txt")
    activity_labels.txt.path <- paste0(script_dir, "/UCI HAR Dataset/activity_labels.txt")

    X_train.txt.path <- paste0(script_dir, "/UCI HAR Dataset/train/X_train.txt")
    y_train.txt.path <- paste0(script_dir, "/UCI HAR Dataset/train/y_train.txt")
    subject_train.txt.path <- paste0(script_dir, "/UCI HAR Dataset/train/subject_train.txt")
    
    X_test.txt.path <- paste0(script_dir, "/UCI HAR Dataset/test/X_test.txt")
    y_test.txt.path <- paste0(script_dir, "/UCI HAR Dataset/test/y_test.txt")
    subject_test.txt.path <- paste0(script_dir, "/UCI HAR Dataset/test/subject_test.txt")
    
    clean_data.txt.path <- paste0(script_dir, "/clean_data.txt")
    summarised_data.txt.path <- paste0(script_dir, "/summarised_data.txt")
    
    #check if data files exist
    if(file.access(features.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", features.txt.path))
    }
    else if(file.access(activity_labels.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", activity_labels.txt.path))
    }
    else if(file.access(X_train.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", X_train.txt.path))
    }
    else if(file.access(y_train.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", y_train.txt.path))
    }
    else if(file.access(subject_train.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", subject_train.txt.path))
    }
    else if(file.access(X_test.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", X_test.txt.path))
    }
    else if(file.access(y_test.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", y_test.txt.path))
    }
    else if(file.access(subject_test.txt.path, mode = 0) == -1){
        stop(paste("Invalid path:", subject_test.txt.path))
    }
    
    #***********************************************
    
    
    
    #***********************************************
    #feature (measurment) labels
    #will be used as variable names in train and test datasets
    features.txt <- tbl_df(read.table(file = features.txt.path, header = FALSE, col.names = c("featureOrder", "featureName")))
    
    #the reatures dataset contains dupicate labels
    #rid the dataset of duplicates by giving each duplicate a unigue value
    features.txt <- tbl_df(apply(features.txt, 2, make.unique, sep = "-"))
    
    #activity labels for activity IDs in y_train.txt and y_test.txt
    activity_labels.txt <- tbl_df(read.table(file = activity_labels.txt.path, header = FALSE, col.names = c("activityID", "activityName")))
    #***********************************************
    
    
    
    #***********************************************
    #load raw train data from X_train.txt file
    X_train.txt <- tbl_df(read.table(file = X_train.txt.path, header = FALSE, nrows = max_rows))
    
    #load activity IDs for train data in X_train.txt
    y_train.txt <- tbl_df(read.table(file = y_train.txt.path, header = FALSE, col.names = "activityID", nrows = max_rows))
    
    #load subject IDs for X_train.txt
    subject_train.txt <- tbl_df(read.table(file = subject_train.txt.path, header = FALSE, col.names = "subjectID", nrows = max_rows))
    #***********************************************
    
    
    
    #***********************************************
    #test features (measurments) raw data
    X_test.txt <- tbl_df(read.table(file = X_test.txt.path, header = FALSE, nrows = max_rows))
    
    #activity IDs for test data in X_test.txt
    y_test.txt <- tbl_df(read.table(file = y_test.txt.path, header = FALSE, col.names = "activityID", nrows = max_rows))

    #subject IDs for X_test.txt
    subject_test.txt <- tbl_df(read.table(file = subject_test.txt.path, header = FALSE, col.names = "subjectID", nrows = max_rows))
    #***********************************************

    
    
    
    #***********************************************
    #assign variable names from features.txt to the raw train and test datasets
    #i did not use "col.names" parameter of read.table beause it causes variable names to be "normalized"
    #this approach preserves the original format for the feature's name
    names(X_train.txt) <- features.txt$featureName
    names(X_test.txt) <- features.txt$featureName
    #***********************************************


    
    
    #***********************************************
    #link activityIDs in y_train.txt dataset to the corresponding activity labels in activity_labels.txt dataset
    y_train.txt <- inner_join(x = y_train.txt, y = activity_labels.txt)
    #***********************************************
    
    
    
    #***********************************************
    #link activityIDs in y_test.txt dataset to the corresponding activity labels in activity_labels.txt dataset
    y_test.txt <- inner_join(x = y_test.txt, y = activity_labels.txt)
    #***********************************************
    
    
    
    #***********************************************
    #create a single dataset that combines all train data (subjects, activities, and measurments)
    train_data <- cbind(cbind(subject_train.txt, y_train.txt), X_train.txt)
    #***********************************************
    
    
    
    #***********************************************
    #create a single dataset that combines all test data (subjects, activities, and measurments)
    test_data <- cbind(cbind(subject_test.txt, y_test.txt), X_test.txt)
    #***********************************************

    
    
    #***********************************************
    #merge train and test datasets into one
    all_data <- rbind(train_data, test_data)
    #***********************************************

    
    
    #***********************************************
    #select only variables subectID, activityName, all mean and std variables
    #also will ignore variables containing "meanFreq"
    clean_data <- select(all_data, subjectID, activityName, contains("mean()", ignore.case = TRUE), contains("std()", ignore.case = TRUE))
    #***********************************************

    
    
    #***********************************************
    #data set with the average of each variable for each activity and each subject.
    summarised_data <- group_by(clean_data, subjectID, activityName) %>% summarise_all(mean)
    
    #prefix all mean and std variables with "MEAN of "
    names(summarised_data)[!(names(summarised_data) %in% c("subjectID", "activityName"))] <- paste0("MEAN of ", names(summarised_data)[!(names(summarised_data) %in% c("subjectID", "activityName"))])
    #***********************************************

        
    
    #***********************************************
    #display results
    #View(clean_data)
    #View(summarised_data)
    #***********************************************
    
    
    
    #***********************************************
    #save summarised data in file
    write.table(clean_data, file = clean_data.txt.path, row.names = FALSE)
    write.table(summarised_data, file = summarised_data.txt.path, row.names = FALSE)
    #***********************************************
}
