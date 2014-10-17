#1. Merges the training and the test sets to create one data set.

setwd("~/Documents/Data Science/Getting and Cleaning Data/UCI HAR Dataset")

# read in train data
features = read.table('./features.txt')
activityType = read.table('./activity_labels.txt')
subjectTrain = read.table('./train/subject_train.txt')
xTrain = read.table('./train/X_train.txt')
yTrain = read.table('./train/Y_train.txt')

# assign column names to train data
colnames(activityType) = c('activityID', 'activityType')
colnames(subjectTrain) = "subjectID"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityID"

# create a train data set 
trainingData = cbind(subjectTrain, yTrain, xTrain)

# read in test data 
subjectTest = read.table('./test/subject_test.txt')
xTest = read.table('./test/X_test.txt')
yTest = read.table('./test/y_test.txt')

# assign column names to test data
colnames(subjectTest) = "subjectID"
colnames(xTest) = features[,2] 
colnames(yTest) = "activityID"

# create a test data set 
testData = cbind(subjectTest, yTest, xTest)

# merge training and test sets to create one data set
data = rbind(trainingData, testData)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

extract = grep("(mean|std)\\(\\)", colnames(data))
data = data[ , c(1:2, extract)]

#3. Uses descriptive activity names to name the activities in the data set
data = merge(data,activityType,by='activityID')
data = data[ ,c(2,1, 69, 3:68)]

#4. Appropriately labels the data set with descriptive variable names. 

# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names

names(data) <- gsub("^t", "Time", names(data)) 
names(data) <- gsub("^f", "Frequency", names(data))
names(data) <- gsub("-mean\\(\\)", "Mean", names(data))
names(data) <- gsub("-std\\(\\)", "StdDev", names(data))
names(data) <- gsub("-", "", names(data))
names(data) <- gsub("BodyBody", "Body", names(data))

#5. From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

library(plyr)

# column means for all except the subject and activity columns
dataColMeans <- function(data){colMeans(data[ ,-c(1:3)])}
tidyMeans <- ddply(.data=data, .(subjectID, activityID, activityType), dataColMeans)
names(tidyMeans)[-c(1:3)] <- paste0("Mean_", names(tidyMeans)[-c(1:3)])

#6. Please upload the tidy data set created in step 5 of the instructions. Please
# upload your data set as a txt file created with write.table() using row.name=FALSE 
# (do not cut and paste a dataset directly into the text box, as this may cause errors
# saving your submission).

write.table(tidyMeans, file="tidydata.txt", row.names=FALSE)

