#Getting and Cleaning Data Course
##(programing assignment)

###Summary
This document describes design and use of **run_analysis()** function which was developed as part of course's programing assignment.

###Steps to run **run_analysis()**
1. Download R script and data from GitHub **mproekt1/data_science_course** repo directory [course_getting_and_cleaning_data/program_assignment_1](https://github.com/mproekt1/data_science_course/tree/master/course_getting_and_cleaning_data/program_assignment_1).
2. It is preffered to use RStudio to run the code.
3. Install and load **dplyr** package.
4. Make sure that data directory **/UCI HAR Dataset** exists in the same directory as the script **run_analysis.R**.
5. Load the script using **source()** function.

###Design notes
####Directory sctructure
The script **run_analysis.R** expects to find data in the same directory. There is a code in the script that determines location of the script file and checks to see if the data directory **/UCI HAR Dataset** is present. If one or more data files are missing, the script will throw an error. The data directory has the following structure and files:

Directory / File | File description
----------------------------|-----------------------
_[current script directory]_/UCI HAR Dataset/**activity_labels.txt**| contains transaction of activities' numeric codes
_[current script directory]_/UCI HAR Dataset/**features.txt**| contains names of the features (vaiables) in test and train data files.
_[current script directory]_/UCI HAR Dataset/train/**X_train.txt**| contains space separated observations from **training** set. Each record represents a single observation. Each column represents a single feature (variable) which corresponds to an item in features.txt.
_[current script directory]_/UCI HAR Dataset/train/**y_train.txt**| contains a list of activity codes from **training** set. Each record represents an activity code that corresponds to an observation in X_train.txt with the same row number.
_[current script directory]_/UCI HAR Dataset/train/**subject_train.txt**| contains a list of subject codes from **training** set. Each record represents a subject code that corresponds to an observation in X_train.txt with the same row number.
_[current script directory]_/UCI HAR Dataset/test/**X_test.txt**| contains space separated observations from **test** set. Each record represents a single observation. Each column represents a single feature (variable) which corresponds to an item in features.txt file.
_[current script directory]_/UCI HAR Dataset/test/**y_test.txt**| contains a list of activity codes from **test** set. Each record represents an activity code that corresponds to an observation in X_test.txt with the same row number.
_[current script directory]_/UCI HAR Dataset/test/**subject_test.txt**| contains a list of subject codes from **test** set. Each record represents a subject code that corresponds to an observation in X_test.txt with the same row number.

####Calling run_analysis() function
The **run_analysis()** has following declaration:  
```
run_analysis <- function(max_rows = -1)
```
where input paramter **max_rows** sets the maximum number of records from the data files to be returned. It's useful to limit the number of returned records during testing. A **positive number** signifies a maximum number of records to be returned from each data file. If the value is **negative** (default -1), then **all** records will be returned. If the value is **0**, then an **error** will be raised (sorry).
