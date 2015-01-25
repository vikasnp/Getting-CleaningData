# Data Source https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

############## 1. Merge training and test set ##############
temp1 <- read.table("train/X_train.txt")
temp2 <- read.table("test/X_test.txt")
X <- rbind(temp1, temp2)

temp1 <- read.table("train/subject_train.txt")
temp2 <- read.table("test/subject_test.txt")
S <- rbind(temp1, temp2)

temp1 <- read.table("train/y_train.txt")
temp2 <- read.table("test/y_test.txt")
Y <- rbind(temp1, temp2)

############## 2. Extracts only measurement on mean and s.d. for each measurement.##############

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

############## 3. Use descriptive activity names to name the activities in the data set.##############

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

############## 4.Label the data set with descriptive activity names.##############

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

############## 5. Create second independent tidy data set with the average of each variable for each activity and for each subject.##############

uniqueSubjects = unique(S)[,1]
numOfSubjects = length(unique(S)[,1])
numOfActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numOfSubjects*numOfActivities), ]

row = 1
for (s in 1:numOfSubjects) {
    for (a in 1:numOfActivities) {
        result[row, 1] = uniqueSubjects[s]
        result[row, 2] = activities[a, 2]
        tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
        result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
        row = row+1
    }
}
write.table(result, "data_set_with_the_averages.txt",row.name=FALSE)