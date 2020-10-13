library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(lubridate)
library(quantmod)

### STEP 1: MERGES THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET

# download the data, unzip, and load the datafiles

FileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(FileUrl, destfile = "wearable.zip")
unzip("wearable.zip")

# the 30 volunteers - X1 7352 cases
training1 <- tbl_df(read.delim("UCI HAR Dataset/train/subject_train.txt", header = FALSE))
training1 <- rename(training1, ID = V1)
training1$Group <- c("Training")

# the 6 activities - X5 7351 cases
training2 <- tbl_df(read.delim("UCI HAR Dataset/train/y_train.txt", header = FALSE)) 
training2 <- rename(training2, Activity = V1)

# info accelerometer and gyroscope
training3_1 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", header = FALSE))
training3_1 <- rename(training3_1, BodyAccX = V1)
training3_2 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", header = FALSE))
training3_2 <- rename(training3_2, BodyAccY = V1)
training3_3 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", header = FALSE))
training3_3 <- rename(training3_3, BodyAccZ = V1)

training4_1 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/total_acc_x_train.txt", header = FALSE))
training4_1 <- rename(training4_1, TotalAccX = V1)
training4_2 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/total_acc_y_train.txt", header = FALSE))
training4_2 <- rename(training4_2, TotalAccY = V1)
training4_3 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/total_acc_z_train.txt", header = FALSE)) ; training4_3
training4_3 <- rename(training4_3, TotalAccZ = V1)

training5_1 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", header = FALSE)) ; training5_1
training5_1 <- rename(training5_1, BodyGyroX = V1)
training5_2 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", header = FALSE)) ; training5_2
training5_2 <- rename(training5_2, BodyGyroY = V1)
training5_3 <- tbl_df(read.delim("UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", header = FALSE)) ; training5_3
training5_3 <- rename(training5_3, BodyGyroZ = V1)

# the 561 feature vectors - X2 7351 cases
training6 <- tbl_df(read.delim("UCI HAR Dataset/train/X_train.txt", header = FALSE))
training6 <- training6 %>%
      mutate(V1 = gsub(pattern = "  ", replacement = " ", x = V1)) %>%
      mutate(V1 = str_trim(string = V1, side = "left"))
training6 <- tbl_df(as.data.frame(str_split_fixed(string = training6$V1, pattern = " ", n = 561)))
names <- names(training6)
training6[names] <- sapply(training6[names], as.numeric)

# Merges the training sets to create one data set
training <- bind_cols(training1, training2, training3_1, training3_2, training3_3, training4_1, training4_2, training4_3, training5_1, training5_2, training5_3, training6)

# same approach for test
test1 <- tbl_df(read.delim("UCI HAR Dataset/test/subject_test.txt", header = FALSE))
test1 <- rename(test1, ID = V1)
test1$Group <- c("Test") ; test1
test2 <- tbl_df(read.delim("UCI HAR Dataset/test/y_test.txt", header = FALSE)) 
test2 <- rename(test2, Activity = V1)
test3_1 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", header = FALSE))
test3_1 <- rename(test3_1, BodyAccX = V1)
test3_2 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", header = FALSE))
test3_2 <- rename(test3_2, BodyAccY = V1)
test3_3 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", header = FALSE))
test3_3 <- rename(test3_3, BodyAccZ = V1)
test4_1 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/total_acc_x_test.txt", header = FALSE))
test4_1 <- rename(test4_1, TotalAccX = V1)
test4_2 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/total_acc_y_test.txt", header = FALSE))
test4_2 <- rename(test4_2, TotalAccY = V1)
test4_3 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/total_acc_z_test.txt", header = FALSE))
test4_3 <- rename(test4_3, TotalAccZ = V1)
test5_1 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", header = FALSE))
test5_1 <- rename(test5_1, BodyGyroX = V1)
test5_2 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", header = FALSE))
test5_2 <- rename(test5_2, BodyGyroY = V1)
test5_3 <- tbl_df(read.delim("UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", header = FALSE))
test5_3 <- rename(test5_3, BodyGyroZ = V1)
test6 <- tbl_df(read.delim("UCI HAR Dataset/test/X_test.txt", header = FALSE))
test6 <- test6 %>%
      mutate(V1 = gsub(pattern = "  ", replacement = " ", x = V1)) %>%
      mutate(V1 = str_trim(string = V1, side = "left"))
test6 <- tbl_df(as.data.frame(str_split_fixed(string = test6$V1, pattern = " ", n = 561)))
names <- names(test6)
test6[names] <- sapply(test6[names], as.numeric)

# Merges the test sets to create one data set
test <- bind_cols(test1, test2, test3_1, test3_2, test3_3, test4_1, test4_2, test4_3, test5_1, test5_2, test5_3, test6)

# Merges the test and training to create one data set
activity <- bind_rows(training, test)

### STEP 2: EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT
activity <- activity %>% select(ID:Activity, V1:V6, V41:V46, V121:V126) %>%
      rename(tBodyAccmeanX = V1, tBodyAccmeanY = V2, tBodyAccmeanZ = V3) %>%
      rename(tBodyAccstdX = V4, tBodyAccstdY = V5, tBodyAccstdZ = V6) %>%
      rename(tGravityAccmeanX = V41, tGravityAccmeanY = V42, tGravityAccmeanZ = V43) %>%
      rename(tGravityAccstdX = V44, tGravityAccstdY = V45, tGravityAccstdZ = V46) %>%
      rename(tBodyGyromeanX = V121, tBodyGyromeanY = V122, tBodyGyromeanZ = V123) %>%
      rename(tBodyGyrostdX = V124, tBodyGyrostdY = V125, tBodyGyrostdZ = V126)   

### STEP 3: USE DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITES IN TEH DATA SET
activity$Activity <- ordered(activity$Activity, levels = c(1, 2, 3, 4, 5, 6), 
                             labels = c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying"))

### STEP 4: APPROPRIATELY LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES - already done above

### STEP 5: CREATE A SECOND, INDEPENDENT DATA SET WITH THE AVERAGE OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT

av_activity <- activity %>% select(ID, Activity:tBodyGyrostdZ) %>%
      group_by(ID, Activity)
av_activity <- av_activity %>% summarize(across(tBodyAccmeanX:tBodyGyrostdZ, ~ mean(.x, na.rm = TRUE)))

### SAVE THE DATA SETS AS CSV FILES
write.csv(activity, file = "activity.csv")
write.csv(av_activity, file = "av_activity.csv")

### SAVE THE DATA SETS AS txt FILES
write.table(activity, file = "activity.txt", row.name=FALSE)
write.table(av_activity, file = "av_activity.txt", row.name=FALSE)
