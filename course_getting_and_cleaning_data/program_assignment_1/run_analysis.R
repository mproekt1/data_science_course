run_analysis <- function(max_rows = -1){
    #install.packages("dplyr")
    #library(dplyr)
    
    #install.packages("tidyr")
    #library(tidyr)
    
    
    
    #***********************************************
    #feature (measurment) labels
    #will be used as variable names in train and test datasets
    features.txt <- tbl_df(read.table(file = "UCI HAR Dataset/features.txt", header = FALSE, col.names = c("featureOrder", "featureName")))
    
    #the reatures dataset contains dupicate labels
    #rid the dataset of duplicates by giving each duplicate a unigue value
    features.txt <- tbl_df(apply(features.txt, 2, make.unique, sep = "-"))
    
    #activity labels for activity IDs in y_train.txt and y_test.txt
    activity_labels.txt <- tbl_df(read.table(file = "UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names = c("activityID", "activityName")))
    #***********************************************
    
    
    
    #***********************************************
    #load raw train data from X_train.txt file
    X_train.txt <- tbl_df(read.table(file = "UCI HAR Dataset/train/X_train.txt", header = FALSE, nrows = max_rows))
    
    #load activity IDs for train data in X_train.txt
    y_train.txt <- tbl_df(read.table(file = "UCI HAR Dataset/train/y_train.txt", header = FALSE, col.names = "activityID", nrows = max_rows))
    
    #load subject IDs for X_train.txt
    subject_train.txt <- tbl_df(read.table(file = "UCI HAR Dataset/train/subject_train.txt", header = FALSE, col.names = "subjectID", nrows = max_rows))
    #***********************************************
    
    
    
    #***********************************************
    #test features (measurments) raw data
    X_test.txt <- tbl_df(read.table(file = "UCI HAR Dataset/test/X_test.txt", header = FALSE, nrows = max_rows))
    
    #activity IDs for test data in X_test.txt
    y_test.txt <- tbl_df(read.table(file = "UCI HAR Dataset/test/y_test.txt", header = FALSE, col.names = "activityID", nrows = max_rows))

    #subject IDs for X_test.txt
    subject_test.txt <- tbl_df(read.table(file = "UCI HAR Dataset/test/subject_test.txt", header = FALSE, col.names = "subjectID", nrows = max_rows))
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
    write.table(summarised_data, file = "summarized_data.txt", row.names = FALSE)
    #***********************************************
}
