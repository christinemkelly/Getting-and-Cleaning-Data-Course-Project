#This R script works with the data collected from smartphones about subject
#movement (walking, sitting, standing, etc). The script will download, read data and analzye 
#means and standard deviations (std) as well as produce a tidy data set.

#Specifically the R script will:
# 1. Merge the training and test data sets to create one data set.
# 2. Extract processed results for the variables marked as "mean" and "std".
# 3. Replace the activity number in the test and training activity files with the name of 
# the activity.
# 4. Rename the variables in the feature files with descriptive names.
# 5. Create a second, independent tidy data set (based on the data in Step 4 )
# with the average of each variable for each activity and each subject.

#*************************************************************************************

# Begin by getting the data: download, unzip (which will create a local dir and plce the
# files in it. Checking first for the existing directory will allow for the code to be re-run
# without having to do the download repeatedly.

if(dir.exists("./UCI HAR Dataset")) {print("Files already downloaded into directory")} else
    
     {url <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
            destinationfile <- "UCI_HAR_zip"
            download.file(url, destinationfile, method='curl')
            unzip(destinationfile)
      }

#*************************************************************************************

# Determine the content of the download. This portion of the script is provided so that a 
# reviewer could execute it and verify that the correct files were used by the script and none 
# were omitted. The full list of files from the download is given in the CodeBook.

    # files <-list.files("UCI HAR Dataset") 

    # testFiles<- list.files("UCI HAR Dataset/test")

    # trainFiles<- list.files("UCI HAR Dataset/train")
    
    # testInertialSignalsFiles <- list.files("UCI HAR Dataset/test/Inertial Signals")
    
    # trainInertialSignalsFiles <- list.files("UCI HAR Dataset/train/Inertial Signals")
    
    # "total_acc_x_test.txt" "total_acc_y_test.txt" "total_acc_z_test.txt"

#*************************************************************************************

# Read the data files into R;  check for NAs with each read.
# Note that the Inertal Signal files in the download aren't needed. 

library(dplyr)
library(data.table)

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
if(colSums(is.na(subject_test)) !=0) Warning("The file subject_test.txt contains blanks")
        
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt")
if(colSums(is.na(activity_test)) !=0) Warning("The file activity_test.txt contains blanks")

features_test <- read.table("UCI HAR Dataset/test/X_test.txt")

if(all(colSums(is.na(features_test))) !=0) Warning("The file features_test.txt contains blanks")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")

if(colSums(is.na(subject_train)) !=0) Warning("The file subject_train.txt contains blanks")

activity_train <- read.table("UCI HAR Dataset/train/y_train.txt")

if(colSums(is.na(activity_train)) !=0) Warning("The file activity_train.txt contains blanks")

features_train <- read.table("UCI HAR Dataset/train/X_train.txt")

if(all(colSums(is.na(features_train))) !=0) Warning("The file features_train.txt contains blanks")

feature_names <- read.table("UCI HAR Dataset/features.txt")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

#*************************************************************************************
# Step 1 of the assignment: Join the test and train files for subject, activity and 
# feature files:

combined_subject <- rbind(subject_test, subject_train)
combined_activity <- rbind(activity_test, activity_train)
combined_features <- rbind(features_test, features_train)

# The columns in the combined files are marked V1, V2, etc. These need to be changed to
# words for readable. If these 3 files were joined now there would be multiple V1 and V2... 
# headers making it diffiult to name the columns after joining. 

colnames(combined_subject) <- "Participant"
colnames(combined_activity) <- "Activity"

# Step 3 of the assingnment: Use descriptive activity names in the combined_activity file instead 
# of V1, V2....

combined_activity[combined_activity == 1] <- "Walking"
combined_activity[combined_activity == 2] <- "Walking Upstairs"
combined_activity[combined_activity == 3] <- "Walking Downstairs"
combined_activity[combined_activity == 4] <- "Sitting"
combined_activity[combined_activity == 5] <- "Standing"
combined_activity[combined_activity == 6] <- "Laying"

# Step 4 of the assingnment: Replace the variable names in the features files with descriptive names.
# Create a vector of names (feature_names[,2] and apply to the columns in the combined_features 
# file. There are duplicate names which need to be addressed.
 
colnames(combined_features) <- feature_names[,2]  
    duplicated(colnames(combined_features))   
    combined_features <- combined_features[, !duplicated(colnames(combined_features))]

# Improve the readability of the feature names by replacing acc with acceleration, etc.:
    names(combined_features) <- gsub("Acc", "Accelerator", names(combined_features))
    names(combined_features) <- gsub("Mag", "Magnitude", names(combined_features))
    names(combined_features) <- gsub("Gyro", "Gyroscope", names(combined_features))
    names(combined_features) <- gsub("^t", "time", names(combined_features))
    names(combined_features) <- gsub("^f", "frequency", names(combined_features))

#*************************************************************************************
# Step 1 of the assingnment: Now that the individual files have been "prettied up", 
# create a master file with all subject, activity and features and all columns labeled plus 
# words for the activities. This file could be used for future analysis but is not needed
# in this script.

master_df <- cbind(combined_subject, combined_activity, combined_features)

#*************************************************************************************
# Step 2 of the assingnment: pull out the mean and std columns according to the assignment and 
# combine into one df with the subjects and activities:

combined_features_mean <- select(combined_features, contains("Mean", ignore.case=TRUE))
combined_features_std <- select(combined_features, contains("std", ignore.case=TRUE))
mean_and_std_df <- cbind(combined_features_mean, combined_features_std)
full_mean_and_std_df <-cbind(combined_subject, combined_activity, mean_and_std_df)
final_df <- full_mean_and_std_df   #Name simplification for easier use in next steps

#********************************************************************************
#Step 5 of the assignment: Using the file created above create a tidy data set 
# with the average of each variable for each activity each subject:
 
arranged_df <- arrange(final_df, Participant) #puts the subjects in order
grouped_df <- group_by(arranged_df, Participant, Activity) #groups the activities together
tidy_df <- summarise_each(grouped_df, funs(mean))

#********************************************************************************
# Write out the datasets.
setwd("..")
write.table(final_df, file="Mean_Standard_Dev_Results.txt", row.name=FALSE)
write.table(tidy_df, file="Tidy_Results.txt", row.name=FALSE)

print("Here you go....the output of the R-Script is in Mean_Standard_Dev_Results.txt")
print("And one more output for you: the tidy set with means summarized by participant 
      and activity is in Tidy_Results.txt")