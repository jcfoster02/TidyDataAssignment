#Course 3/Week 4 - Assignment
#
##   run_analysis.R
##
## September, 2016
## Julie Foster
#


## Step 1: Merge the training and the test sets to create one 
##        data set:  "largeData" 


#  Download and read in datasets
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip", mode="wb")
unzip(zipfile="./data/Dataset.zip",exdir="./data")


library(plyr)

# read in and label test files

subj_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
names(subj_test) <- "subject"

y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

# add new column with as "activity" using y_test$V1 to mapvalues from 
# activity_label.txt file
y_test$activity <- mapvalues(y_test$V1, from = c(1,2,3,4,5,6), 
                             to = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING","STANDING", "LAYING"))

# delete original column (y_test$V1)
y_subtest = subset(y_test, select = -c(V1) )

x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")

features <- read.table("./data/UCI HAR Dataset/features.txt")
colnames(x_test) <- as.character(features$V2)

testData <- cbind(subj_test, y_subtest, x_test)

# reading in and labeling train files

subj_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
names(subj_train) <- "subject"

y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

# add new column with as "activity" using y_train$V1 to mapvalues from 
#  activity_label.txt file
y_train$activity <- mapvalues(y_train$V1, from = c(1,2,3,4,5,6), 
                              to = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING","STANDING", "LAYING"))

# delete original column (y_train$V1)
y_subtrain = subset(y_train, select = -c(V1) )

x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")

colnames(x_train) <- as.character(features$V2)

trainData <- cbind(subj_train, y_subtrain, x_train)

largeData <- rbind(testData, trainData)
dim(largeData)


## Step 2: Extract the measurements on the mean and standard 
##         deviation for each measurement.


#  extract mean measurements
meanData <- largeData[ , grep("mean", colnames(largeData), value=TRUE)]

# extract standard deviation measurements.
stdData <- largeData[ , grep("std", colnames(largeData), value=TRUE)]

# merge meanData and stdData -> meanstdData
meanstdData <- cbind(largeData$subject, largeData$activity, meanData, stdData)
  

## Step 3:  Use descriptive activity names to name the activities 
## in the dataset.  (Folded into Step 1,  above).


## Step 4:  Appropriately label the dataset with descriptive variable names.

# rename columns from "largeData"
library(stringr)
names(meanstdData)[1] <- "subject"
names(meanstdData)[2] <- "activity"

# remove () from activity column names
names(meanstdData) <- str_replace(names(meanstdData), "\\(\\)", "")


## Step 5:  From this dataset, create a second, independent
##          tidy dataset with the average of each variable 
##          for each activity and each subject.  


# average by subject for each variable
subj_s <- split(meanstdData, meanstdData$subject)
by_subj <- sapply(subj_s, function(x) colMeans(x[, 3:79]))
colnames(by_subj) <- paste0("Subject_", 1:30)

# average by activity for each variable
act_s <- split(meanstdData, meanstdData$activity)
by_act <- sapply(act_s, function(x) colMeans(x[, 3:79]))

# merge by_act and by_subj to create new "avg_dataset"
avg_dataset <- cbind(by_act, by_subj)
dim(avg_dataset)

# write "avg_dataset" to output file.
write.txt(avg_dataset, file = "avg_dataset.txt", row.name=FALSE)
