## run_analysis.R
# A script created for Coursera Getting and Cleaning Data Assignment
# "Getting and Cleaning Data Course Project"

# The code includes the following 5 steps: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

#############################################################################################
# Load Data

setwd("~/") #change this as required to point to where the data is saved

test <- read.table("./UCI HAR Dataset/test/X_test.txt", sep="") # DF 2947 obs of 561 var (stores the different features and corresponding measurements)
test_activity <- read.table("./UCI HAR Dataset/test/y_test.txt", sep="") # DF 2947 obs of 1 var (IDs the 1:6 predicted activity types)

train <- read.table("./UCI HAR Dataset/train/X_train.txt", sep="") # DF 7352 obs of 561 var (IDs the differnet features)
train_activity <- read.table("./UCI HAR Dataset/train/y_train.txt", sep="") # DF 7352 obs of 1 var (IDs the 1:6 activity types)
train_ID <- read.table("./UCI HAR Dataset/train/subject_train.txt", sep="") # DF 7352 obs of 1 var (IDs the 1:30 subjects)

features <- read.table("./UCI HAR Dataset/features.txt", sep="") #stores the different featueres 

##############################################################################################
## Step 1: Merging all the Training and Test Data sets 

df_train <- cbind(train_ID[,],train_activity[,],train[,]) 
colnames(df_train) <- c("ID","Activity", as.character(features[,2]))

ID = 1:dim(test)[1]
df_test <- cbind(ID[], test_activity[,],test[,])
colnames(df_test) <- c("ID","Activity", as.character(features[,2]))
df_test$ID <- NA 

df_temp <- rbind(df_train,df_test)  # DF 10299 obs of 563 var 
#############################################################################################
## Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Select only columns with "ID", "Activity", "mean()" OR "std()" in heading 
colindex <- grep("mean()|std()", features[,2])
df_temp2 <- df_temp[ ,c(1,2,colindex+2)] #keep only columns that are required DF 10299 obs of 81 var 

#############################################################################################
## Step 3: Use descriptive activity names to name the activities in the data set.

df_temp2$Activity <- ifelse(  df_temp2$Activity == 1, "WALKING",
  ifelse( df_temp2$Activity == 2,  "WALKING_UPSTAIRS",
    ifelse( df_temp2$Activity == 3, "WALKING_DOWNSTAIRS",
      ifelse( df_temp2$Activity == 4, "SITTING",
        ifelse(df_temp2$Activity == 5, "STANDING",
               ifelse(df_temp2$Activity == 6, "LAYING",
                      NA))))))

############################################################################################
## Step 4: Appropriately label the data set with descriptive variable names.

## This is already done in step 1, where the column names for the different variables were 
# renamed as per the features file. 

############################################################################################
## Step 5: From the data set in step 4, create a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

# Use dplyr package 
library(dplyr)

# This line finds the mean of the specified column by group 
DF_Final <- df_temp2 %>%  group_by(ID, Activity) %>%  summarise_each(funs(mean)) 
head(DF_Final[,1:5])
#ID           Activity tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
#(int)              (chr)             (dbl)             (dbl)             (dbl)
#1     1             LAYING         0.2215982      -0.040513953        -0.1132036
#2     1            SITTING         0.2612376      -0.001308288        -0.1045442
#3     1           STANDING         0.2789176      -0.016137590        -0.1106018
#4     1            WALKING         0.2773308      -0.017383819        -0.1111481
#5     1 WALKING_DOWNSTAIRS         0.2891883      -0.009918505        -0.1075662
#6     1   WALKING_UPSTAIRS         0.2554617      -0.023953149        -0.0973020

#######################################################################################################
## Final step save the summarised data frame 
write.csv(DF_Final, "./UCI HAR Dataset/SummaryOutputData.csv")