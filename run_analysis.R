library(dplyr)

#Identifying all the data frames and assigning file and columns names
Activs <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("code","activity"))
Feats <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","functions"))
test_Subject <- read.table("UCI HAR Dataset\\test\\subject_test.txt",col.names = "subject")
xtest <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = Feats$functions)
ytest <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "code")
train_Subject <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = "subject")
xtrain <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = Feats$functions)
ytrain <- read.table("UCI HAR Dataset\\train\\y_train.txt", col.names = "code")

#Merging the Training and Tests files into one data set
tx <- rbind(xtrain,xtest)
ty <- rbind(ytrain,ytest)
subjects <- rbind(train_Subject,test_Subject)
MergedData <- cbind(subjects, ty, tx)

#Calculation of the mean and the standard deviation for the variables and naming activities
TidyData <- MergedData %>% select(subject, code, contains("mean"), contains("std"))
TidyData$code <- Activs[TidyData$code, 2]

#Labeling data with proper descriptive names
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#Creating an independent data set containing the average of each variable X activity X subject
CrossDataSet <- TidyData %>% group_by(subject,activity) %>% summarise_all(funs(mean))
write.table(CrossDataSet,"TidyDataOutput.txt", row.names = FALSE)

str(CrossDataSet)
CrossDataSet