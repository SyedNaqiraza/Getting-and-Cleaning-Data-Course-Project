
library(dplyr)

#Download Dataset

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
path_of_data <- "UCI HAR Dataset"
if (!file.exists(path_of_data)) {
  unzip(zipFile)
}



# read training data
t_subjects <- read.table(file.path(path_of_data, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(path_of_data, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(path_of_data, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(path_of_data, "test", "subject_test.txt"))
testValues <- read.table(file.path(path_of_data, "test", "X_test.txt"))
testActivity <- read.table(file.path(path_of_data, "test", "y_test.txt"))

# read features, don't convert text labels to factors
features <- read.table(file.path(path_of_data, "features.txt"), as.is = TRUE)
## note: feature names (in features[, 2]) are not unique
##       e.g. fBodyAcc-bandsEnergy()-1,8

# read activity labels
activities <- read.table(file.path(path_of_data, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")



# concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(t_subjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(t_subjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")



columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# ... and keep data in these columns only
humanActivity <- humanActivity[, columnsToKeep]



# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])



# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand 
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols



# group by 
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output 
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
