#################################################################################       
# run_analysis.R                                                                #
# Author: Luis Padua                                                            #
# Date: 17-Jun-2017                                                             #
# As part of Project Conclusion of Course Geting and Cleaning Data from Coursera#
#
#################################################################################

library(dplyr)

#loading the names of the columns
rawdataNames <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
rawdataNames <- c("subject", "activity", as.character(rawdataNames[[2]]))
#appling the naming conventions to the rawdataNames array
rawdataNames <- tolower(rawdataNames)
rawdataNames <- sub("\\(\\)", "", rawdataNames)
rawdataNames <- sub(",", "_", rawdataNames)
rawdataNames <- sub("\\(", "-by", rawdataNames) # -by to indicate the variables 
                                                # that were used to calculate the column
rawdataNames <- gsub("\\)", "", rawdataNames)

#there are duplicated names in rawdataNames
# with a For I will add the index channels to the names that are duplicated
duplicateNames <- duplicated(rawdataNames)
for(i in seq_along(rawdataNames)) 
{
        if (duplicateNames[i] == TRUE) 
        {
                rawdataNames[i] <- paste0(rawdataNames[i], "-", as.character(as.integer(i)))
        }
}

#loading the rawdata from Tests datasets
tests <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
#loading the activity and subject of each observation and merging to tests
testsActivity <- read.table("./UCI HAR Dataset/test/Y_test.txt", header = FALSE)
testsSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
tests <- bind_cols(testsSubject, testsActivity, tests)
names(tests) <- rawdataNames
tests <- tbl_df(tests)
#remove used variables
rm(testsActivity)
rm(testsSubject)

#loading the rawdata from Train datasets
training <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
training <- tbl_df(training)
#loading the activity and subject of each observation and merging to training
trainActivity <- read.table("./UCI HAR Dataset/train/Y_train.txt", header = FALSE)
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
training <- bind_cols(trainSubject, trainActivity, training)
names(training) <- rawdataNames
training <- tbl_df(training)
#remove used variables
rm(trainActivity)
rm(trainSubject)

#ITEM 1 - Merges the training and the test sets to create one data set
mergedTestTrain <- bind_rows(tests, training)

#ITEM 2 - Extracts only the measurements on the mean and standard deviation for each measurement
selectedTestTrain <- select(mergedTestTrain, 
                            grep("subject|activity|mean|std", names(mergedTestTrain)))
# remove the angle columns which only use Mean channels as inputs
selectedTestTrain <- select(selectedTestTrain,
                            grep("angle", names(selectedTestTrain), invert = TRUE))

#ITEM 3 - Uses descriptive activity names to name the activities in the data set
selectedTestTrain$activity <- as.factor(selectedTestTrain$activity)
levels(selectedTestTrain$activity) <- c("WALKING", "WALKING_UPSTAIRS", 
                                        "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")

#ITEM 4 - Appropriately labels the data set with descriptive variable names
#variables that starts with t are in Time Domain and so, replace this to more descriptive name
names(selectedTestTrain) <- sub("^t", "timeDomain_", names(selectedTestTrain))
#variables that starts with f are in Frequency Domain and so, replace this to more descriptive name
names(selectedTestTrain) <- sub("^f", "freqDomain_", names(selectedTestTrain))

#ITEM 5 - From the data set in step 4, creates a second, independent tidy data set with the average 
#         of each variable for each activity and each subject
#let's make the subject as factor to help on this task
selectedTestTrain$subject <- as.factor(selectedTestTrain$subject)
#grouping as resquested
bySubAct <- group_by(selectedTestTrain, subject, activity)
#creating the mean of all columns by the groups
finaldata <- summarize_each(bySubAct, funs(mean))

write.table(finaldata, file = "tidy_testtrain_means.txt", row.names = FALSE)