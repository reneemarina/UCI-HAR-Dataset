setwd("C:/Users/renee.marina/")
library(plyr)
library(data.table)

##Read and Convert Data
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

#Set Training Data
data.train.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
data.train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(data.train.subject, data.train.activity, data.train.x)
names(data.train) <- c(c('subject', 'activity'), features)

#Set Test Data
data.test.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
data.test.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
data.test.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data.test <-  data.frame(data.test.subject, data.test.activity, data.test.x)
names(data.test) <- c(c('subject', 'activity'), features)

#Step 1: Merges the training and the test sets to create one data set.
data.all <- rbind(data.train, data.test)

##Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std <- grep('mean|std', features)
data.sub <- data.all[,c(1,2,mean_std + 2)]

##Step 3: Uses descriptive activity names to name the activities in the data set
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity.labels <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

##Step 4: Appropriately labels the data set with descriptive variable names.
name.new <- names(data.sub)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("BodyBody", "Body", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("^t", "Time", name.new)
name.new <- gsub("^f", "Frequency", name.new)
name.new <- gsub("-", "_", name.new)
name.new <- gsub("[(][)]", "", name.new)
names(data.sub) <- name.new

##Step 5: From the data set in step 4, creates a second, 
##independent tidy data set with the average of each variable for each activity 
##and each subject.
tidy.data <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
write.table(x = tidy.data, file = "tidy_data.txt", row.names = FALSE)