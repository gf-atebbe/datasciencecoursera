## You should create one R script called run_analysis.R that does the following. 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names. 
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# setwd('~/Documents/code/datasciencecoursera/getting_and_cleaning_data/')

## Parse the list of features
features <- read.table('UCI HAR Dataset/features.txt', header=FALSE, stringsAsFactors=FALSE)

## Import the training data set
train <- read.table('UCI HAR Dataset/train/X_train.txt', header=FALSE)
train$source <- as.factor('TRAIN')
train$labels <- as.factor(read.table('UCI HAR Dataset/train/Y_train.txt', header=FALSE)$V1)
train$subject <- as.factor(read.table('UCI HAR Dataset/train/subject_train.txt', header=FALSE)$V1)

## Import the test data set
test <- read.table('UCI HAR Dataset/test/X_test.txt', header=FALSE)
test$source <- as.factor('TEST')
test$labels <- as.factor(read.table('UCI HAR Dataset/test/Y_test.txt', header=FALSE)$V1)
test$subject <- as.factor(read.table('UCI HAR Dataset/test/subject_test.txt', header=FALSE)$V1)

## 1. Merge the training and test data sets into a single data frame
merged <- rbind(train, test)

## 2. Extract the mean and standard deviation for each measurement
meanstd <- merged[,which(grepl("mean|std", colnames(merged)))]

## 3. Uses descriptive activity names to name the activities in the data set
activity_name <- read.table('UCI HAR Dataset/activity_labels.txt', header=FALSE)
colnames(activity_name) <- c("labels", "Activity")
merged$Activity <- merge(merged, activity_name, by="labels", sort=FALSE)$Activity

## 4. Appropriately labels the data set with descriptive activity names.
colnames(merged) <- c(features$V2, "Source", "labels", "Subject", "Activity")

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
s <- split(merged, list(merged$Activity, merged$Subject))
tidy <- sapply(s, function(x) colMeans(x[, 1:length(features$V1)], na.rm=TRUE))
write.table(tidy, file='tidy_data_set.txt', sep=',')
