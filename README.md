# -SamsungDataAnalysis

library(dplyr)

#Identifying all the data frames and assigning file and columns names
Activs <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("code","activity")) #reads the activities data set
Feats <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","functions")) #reads the features data set
test_Subject <- read.table("UCI HAR Dataset\\test\\subject_test.txt",col.names = "subject") #reads the subject in the test data set, names one column
xtest <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = Feats$functions) #reads the subject in the test data set, names a series of columns
ytest <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "code") ##reads the subject in the test data set, names one column
train_Subject <- read.table("UCI HAR Dataset\\train\\subject_train.txt", col.names = "subject") #reads the subject in the training data set, name one column
xtrain <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = Feats$functions) #reads the subject in the training data set, names a series of columns
ytrain <- read.table("UCI HAR Dataset\\train\\y_train.txt", col.names = "code")  #reads the subject in the training data set, name one column

#Merging the Training and Tests files into one data set
tx <- rbind(xtrain,xtest) #binds rows together x from train & test files
ty <- rbind(ytrain,ytest) #binds rows together y from train & test files
subjects <- rbind(train_Subject,test_Subject) #binds rows together y from train & test 'Subject' files
MergedData <- cbind(subjects, ty, tx) #binds the columns from the three files above into one merged data frame

#Calculation of the mean and the standard deviation for the variables and naming activities
TidyData <- MergedData %>% select(subject, code, contains("mean"), contains("std")) #subsets a data frame containing subjects, codes and its means & standard deviation
TidyData$code <- Activs[TidyData$code, 2]

#Labeling data with proper descriptive names, for each column label
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
