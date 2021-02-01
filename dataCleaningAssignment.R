# Coursera Assignment - Data cleaning with R

setwd("C:\\Users\\chris\\Dropbox\\Data_Science\\Repositories\\Coursera\\DataCleaning\\assignment")

library(data.table)

# load all needed files
testx <- fread(".\\data\\test\\X_test.txt")
testy <- fread(".\\data\\test\\y_test.txt")
testsub <- fread(".\\data\\test\\subject_test.txt")

trainx <- fread(".\\data\\train\\X_train.txt")
trainy <- fread(".\\data\\train\\y_train.txt")
trainsub <- fread(".\\data\\train\\subject_train.txt")

features <- fread(".\\data\\features.txt")
activityLabels <- fread(".\\data\\activity_labels.txt")

# do some renaming to avoid confusion
setnames(testy, c("V1"), c("activity"))
setnames(trainy, c("V1"), c("activity"))
setnames(testsub, c("V1"), c("subject"))
setnames(trainsub, c("V1"), c("subject"))

#--------------------------------------------------------
# 5 Steps of the assignment

# Step 1: Merges the training and the test sets to create
# one data set.

## Cbind all test/train data and then Rbind test and train
test <- cbind(testsub, testx, testy)
train <- cbind(trainsub, trainx, trainy)
dt <- rbind(train, test)

# Step 4: Appropriately labels the data set with
# descriptive variable names.

# I'm doing step 4 before step 2, because I identify
# the variables to be extracted in step 2 by their
# variable label I get from step 4.
 
setnames(dt,
         names(dt[,2:562]),
         features$V2)

# Step 2: Extracts only the measurements on the mean and
# standard deviation for each measurement.

# I interpreted the instructions that per feature the 3 means
# for X, Y, Z an the 3 sd for X, Y, Z should be retrieved.
# I did not include other variables containing the string 'mean'
# like the 'meanFreq()' variables.

cols <- c(1, # first variable is subject
          grep("std\\(\\)|mean\\(\\)", names(dt)),
          ncol(dt)) # last variable is activity
dt <- dt[order(subject, activity),
         ..cols] # fast subsetting with data.table


# 3. Uses descriptive activity names to name the activities
# in the data set

# Here I update the activity variable and transform it into
# a factor with labels according to the included Codebook.
dt[, activity := factor(activity,
                        levels = ..activityLabels$V1,
                        labels = ..activityLabels$V2)]

# 5. From the data set in step 4, creates a second, 
# independent tidy data set with the average of each 
# variable for each activity and each subject.

cols <- grep("std\\(\\)", names(dt), value = T)
dt2 <- dt[, lapply(.SD, mean), # apply mean function
          by = .(subject, activity), # grouped by the subject variable
          .SDcols = cols] # to this columns

## I prefer long format over wide format
## to get wide:
# dt3 <- dcast(dt2,
#              subject ~ activity,
#              value.var = names(dt2)[-(1:2)])



