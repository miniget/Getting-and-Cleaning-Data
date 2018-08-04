# You should create one R script called run_analysis.R that does the following.
# Q1. Merges the training and the test sets to create one data set.
##set the working directory
setwd("C:\\Users\\asten\\Documents\\Mini\\Data_science_training\\Geting_and_Cleaning_Data\\Week4\\ProjectData")

FileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download file from URL
download.file(FileURL, destfile ="C:\\Users\\asten\\Documents\\Mini\\Data_science_training\\Geting_and_Cleaning_Data\\Week4\\ProjectData\\Dataset.zip", method = "curl")

## Unzip file into a folder
unzip("C:\\Users\\asten\\Documents\\Mini\\Data_science_training\\Geting_and_Cleaning_Data\\Week4\\ProjectData\\Dataset.zip", files = NULL)

## Reading the Supporting Metadata from the text files
FeatureNamesMetadata <- read.table ("features.txt")
ActivityLabels <- read.table ("activity_labels.txt", header = FALSE)

##Reading the Test data from the text files
FeatureTestData<-read.table ("test\\X_test.txt", header = FALSE)
ActivityTestData<-read.table ("test\\y_test.txt", header = FALSE)
SubjectTestData<-read.table ("test\\subject_test.txt", header = FALSE)

##Reading  the Training data from text files
FeatureTrainData<-read.table ("train\\X_train.txt", header = FALSE)
ActivityTrainData<-read.table ("train\\y_train.txt", header = FALSE)
SubjectTrainData<-read.table ("train\\subject_train.txt", header = FALSE)

##Organizing similar data into one file
Subject <- rbind(SubjectTrainData, SubjectTestData)
Activity <- rbind(ActivityTrainData, ActivityTestData)
FeaturesData <- rbind(FeatureTrainData, FeatureTestData)

## Naming the columns of the Feature data
colnames(FeaturesData) <- t(FeatureNamesMetadata[2])
colnames(Activity) <- "Activity"
colnames(Subject) <- "Subject"

## Merging the overall datasets in one
MergedData <- cbind (FeaturesData, Activity, Subject)

## Q2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##-to extract measurements mean and standard deviation
ExtractColWithMeanSTD <- grep (".*Mean.*|.*Std.*", names (MergedData), ignore.case=TRUE)  

## Adjusting the diamension of the datasets to 10299 by 563
ActualColumns <- c(ExtractColWithMeanSTD, 562, 563)

##Extract measurements on the mean and STD
ActualData <- MergedData[,ActualColumns]  

## Q3. Uses descriptive activity names to name the activities in the data set.

ActualData$Activity <- as.character(ActualData$Activity)
for (i in 1:6){
        ActualData$Activity[ActualData$Activity == i] <- as.character(ActivityLabels[i,2])
}                                                 

ActualData$Activity<- as.factor(ActualData$Activity)

##Part 4 - Appropriately labels the data set with descriptive variable names

names (ActualData)
names(ActualData)<-gsub("Acc", "Accelerometer", names(ActualData))
names(ActualData)<-gsub("Gyro", "Gyroscope", names(ActualData))
names(ActualData)<-gsub("BodyBody", "Body", names(ActualData))
names(ActualData)<-gsub("Mag", "Magnitude", names(ActualData))
names(ActualData)<-gsub("^t", "Time", names(ActualData))
names(ActualData)<-gsub("^f", "Frequency", names(ActualData))
names(ActualData)<-gsub("tBody", "TimeBody", names(ActualData))
names(ActualData)<-gsub("-mean()", "Mean", names(ActualData), ignore.case = TRUE)
names(ActualData)<-gsub("-std()", "STD", names(ActualData), ignore.case = TRUE)
names(ActualData)<-gsub("-freq()", "Frequency", names(ActualData), ignore.case = TRUE)
names(ActualData)<-gsub("angle", "Angle", names(ActualData))
names(ActualData)<-gsub("gravity", "Gravity", names(ActualData))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## call library
library (data.table)

ActualData$Subject <- as.factor(ActualData$Subject)
ActualData <- data.table(ActualData)

## to split the data means into subsets based on subject and activity
tidyData <- aggregate(. ~Subject + Activity, ActualData, mean)

## to sort the data based on subject and activity
tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity),]

## to print the data and save the result into a text file
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

