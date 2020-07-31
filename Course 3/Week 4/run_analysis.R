library(dplyr)
setwd("/Users/snehchitalia/Desktop")
#Load all data from folder
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#Merge into single dataset
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merge_Data <- cbind(Subject, Y, X)

#Extracts only the measurements on the mean and standard deviation for each measurement.
Tidy <- Merge_Data %>% select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set
Tidy$code <- activities[Tidy$code, 2]

#Appropriately labels the data set with descriptive variable names.
names(Tidy)[2] = "activity"
names(Tidy)<-gsub("Acc", "Accelerometer", names(Tidy))
names(Tidy)<-gsub("Gyro", "Gyroscope", names(Tidy))
names(Tidy)<-gsub("BodyBody", "Body", names(Tidy))
names(Tidy)<-gsub("Mag", "Magnitude", names(Tidy))
names(Tidy)<-gsub("^t", "Time", names(Tidy))
names(Tidy)<-gsub("mean", "Mean", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("std", "STD", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("freq", "Frequency", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("angle", "Angle", names(Tidy))
names(Tidy)<-gsub("gravity", "Gravity", names(Tidy))
names(Tidy)<-gsub("^f", "Frequency", names(Tidy))

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

Final <- Tidy %>% group_by(subject, activity)

Final<-summarise_all(Final,mean)

write.table(Final, "FinalData.txt", row.name=FALSE)


