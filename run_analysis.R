# Step1. Merges the training and the test sets to create one data set.
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt") 
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
joinLabel <- rbind(trainLabel, testLabel)
joinSubject <- rbind(trainSubject, testSubject)

# Step2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("./UCI HAR Dataset/features.txt")
mean_std_Indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, mean_std_Indices]
names(joinData) <- gsub("\\(\\)", "", features[mean_std_Indices, 2]) # remove "()"

# Step3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
tidyData <- cbind(joinSubject, joinLabel, joinData)
write.table(cleanedData, "merged_data.txt") 

# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectNum <- length(table(joinSubject)) # 30
activityNum <- dim(activity)[1] # 6
measureNum <- dim(tidyData)[2] # 68
result <- matrix(NA, nrow=subjectNum*activityNum, ncol=measureNum) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectNum) {
  for(j in 1:activityNum) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:measureNum] <- colMeans(cleanedData[bool1&bool2, 3:measureNum])
    row <- row + 1
  }
}
write.table(result, "data_with_means.txt") 
