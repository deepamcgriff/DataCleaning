library(dplyr) #Load dplyr package for inner join later
#Set working directory to saved zip file
setwd("H:/Coursera/Getting and Cleaning Data/UCI HAR Dataset")
#Read in training data files
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

#Read in test data files
subject_train <- read.table("train/subject_train.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

#Read in descriptive files
features <- read.table('features.txt')
activityLabels = read.table('activity_labels.txt')

#Attach data labels to training and test datasets
colnames(x_train) <- features[,2] 
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"
colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"
colnames(activityLabels) <- c('activityId','activityType')

#Merge all datasets into one; first combine triaxial data with horizontal bind and then train/test data with 
#vertical bind
all_train <- cbind(y_train, subject_train, x_train)
all_test <- cbind(y_test, subject_test, x_test)
all <- rbind(all_train, all_test)

#search for variables either containing IDs or means/SD and create subset including only these fields
mean_sd <- (grepl("activityId" , names(all)) | grepl("subjectId" , names(all)) 
            | grepl("mean.." , names(all)) | grepl("std.." , names(all))
)
mean_sd_subset <- all[ , mean_sd == TRUE]

#Replace non-descriptive values and labels with descriptive values or labels
desc = inner_join(activityLabels, mean_sd_subset, by="activityId")
desc$activityId <- NULL
names(desc) <- gsub("Acc", "Accelerator", names(desc))
names(desc) <- gsub("Mag", "Magnitude", names(desc))
names(desc) <- gsub("Gyro", "Gyroscope", names(desc))
names(desc) <- gsub("^t", "time", names(desc))
names(desc) <- gsub("^f", "frequency", names(desc))

#Create tidy summary dataset by grouping by subject and activity type
tidymeans <- aggregate(. ~subjectId + activityType, desc, mean)
write.table(tidymeans, "TidyMeans.txt", row.name=FALSE)
