# This script takes the UCI HAR datasets and merges the training and test sets
# into one dataset containing only mean and standard deviation for each measurement.
# It utilizes descriptive activity names and variable names. Finally, a second, 
# independent tidy data set with the average of each variable for each activity and 
# subject has been created.

# create directory and download UCI HAR dataset
if(!file.exists("./data")) {dir.create("./data")}

filename <- "./data/getdata_dataset.zip"

if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, filename, method = "curl")
}
if (!file.exists("./data/UCI HAR Dataset")) {
    unzip(zipfile = "./data/getdata_dataset.zip", exdir = "./data")
}

# Merging the training and test datasets to create one dataset

# Reading features
features <- read.table("./data/UCI HAR Dataset/features.txt")

# Reading activity labels
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

# Reading in training files
x_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# Reading in testing files
x_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# Assigning column names to data imported above
colnames(activity_labels) = c('activityID', 'activityType')
colnames(subject_train) = "subjectID"
colnames(x_train) = features[,2]
colnames(y_train) = "activityID"
colnames(subject_test) = "subjectID"
colnames(x_test) = features[,2]
colnames(y_test) = "activityID"

# Creating merged training and testing data sets
trainingData <- cbind(y_train, subject_train, x_train)
testData <- cbind(y_test, subject_test, x_test)

# Combine training and testing into a final data set
finalData <- rbind(trainingData, testData)

# Create a vector for the column names from finalData to use to select appropriate
# columns for mean and standard deviation.
colNames <- colnames(finalData)

# Extract only mean and standard deviation for each measurement
# Create logical vector
logicalVector <- (grepl("activity..",colNames) |
                      grepl("subject..",colNames) |
                      grepl(".*mean.*",colNames) |
                      grepl(".*std.*",colNames)
                  )

# Subset finalData based on logical vector to get desired columns
finalData <- finalData[logicalVector==TRUE]

# Descriptive activity names

# Merging to get activity names
finalData <- merge(finalData, activity_labels, by = 'activityID', all.x = TRUE)

# Updating colNames vector
colNames <- colnames(finalData)

# Descriptive variable names

for (i in 1:length(colNames)) {
    colNames[i] = gsub("\\()", "", colNames[i])
    colNames[i] = gsub("-std$", "StdDev", colNames[i])
    colNames[i] = gsub("-mean", "Mean", colNames[i])
    colNames[i] = gsub("^(t)", "time", colNames[i])
    colNames[i] = gsub("^(f)", "freq", colNames[i])
    colNames[i] = gsub("([Gg]ravity)", "Gravity", colNames[i])
    colNames[i] = gsub("([Bb]ody)", "Body", colNames[i])
    colNames[i] = gsub("[Gg]yro", "Gyro", colNames[i])
    colNames[i] = gsub("AccMag", "AccMagnitude", colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", colNames[i])
    colNames[i] = gsub("JerkMag", "JerkMagnitude", colNames[i])
    colNames[i] = gsub("GyroMag", "GyroMagnitude", colNames[i])
}

# Updating column names vector
colNames <- colnames(finalData)

# Create tidy data set with average of each variable for each activity and subject.

finalDataNoType <- finalData[,names(finalData) != 'activityType']
tidyData <- aggregate(finalDataNoType[,names(finalDataNoType) != c('activityID', 'subjectID')],
                      by = list(activityID=finalDataNoType$activityID,
                                subjectID=finalDataNoType$subjectID), mean)

tidyData <- merge(tidyData, activity_labels, by='activityID', all.x=TRUE)

write.table(tidyData, "./data/Get Clean Data Project/tidy.txt", row.names = FALSE)
